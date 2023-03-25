// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// generate-lex-tables creates character classification and state machine
// transition tables for quick-lint-js' lexer.
//
// The state machine implements a deterministic finite automaton (DFA).
//
// Currently, the state machine only recognizes plain symbols such as "+=",
// "||=", and "~".
//
// == State machine lookup algorithm ==
//
// The code currently lives inside lexer::try_parse_current_token. See
// NOTE[lex-table-lookup].
//
// The algorithm requires three tables which are accessed in the following
// order:
//
// 1. Character classification table (character_class_table).
//    See NOTE[lex-table-class].
// 2. State transition table (transition_table).
// 3. Terminal state lookup table (state_to_token).
//    See NOTE[lex-table-token-type].
//
// == Design choices ==
//
// For implementation simplicity, after character classificiation, the DFA is a
// tree, not a graph:
//
// * no cycles
// * two different inputs cannot lead to the same state
//
// NOTE[lex-table-class]: To reduce the size of the transition table, input
// bytes are first classified into a small number of equivalence classes via
// character_class_table. Currently, bytes not part of symbols (i.e. almost all
// bytes) are classified to a special equivalence class, and all transitions for
// that special equivalence class lead to the 'retract' state.
//
// NOTE[lex-table-initial]: In normal DFA tables, there is on initial state.
// In our table, there are many initial states. The numbers used for character
// classifications are identical to the numbers used for these initial states.
// A normal DFA table would do the following to determine the first transition:
//     transition_table[character_class_table[input[0]]][state::initial]
// However, because of our initial state optimization, we need fewer lookups to
// get the same answer:
//     /*            */ character_class_table[input[0]]
// This removes one table lookup. It also shrinks the transition table slightly.
//
// == Improvements ==
//
// NOTE[lex-table-token-type]: For now, classification only returns a valid
// token type. This should be changed in the future if non-trivial cases which
// require further processing need to be supported.

#include <algorithm>
#include <cerrno>
#include <cstdio>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/arg-parser.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <set>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
std::vector<string8_view> symbols = {
    u8"!"_sv,  u8"!="_sv,  u8"!=="_sv, u8"%"_sv,    u8"%="_sv, u8"&"_sv,
    u8"&&"_sv, u8"&&="_sv, u8"&="_sv,  u8"+"_sv,    u8"++"_sv, u8"+="_sv,
    u8"="_sv,  u8"=="_sv,  u8"==="_sv, u8"=>"_sv,   u8">"_sv,  u8">="_sv,
    u8">>"_sv, u8">>="_sv, u8">>>"_sv, u8">>>="_sv, u8"^"_sv,  u8"^="_sv,
    u8"|"_sv,  u8"|="_sv,  u8"||"_sv,  u8"||="_sv,
};

const char* identifier_for_character(char8 symbol);
std::string make_cpp_string(string8_view);
std::string make_comment(string8_view);

struct generate_lex_tables_options {
  const char* output_path = nullptr;
};

enum class lex_state_kind {
  intermediate,
  non_unique_terminal,
  unique_terminal,
};

// A specific state a lexer might enter.
struct lex_state {
  lex_state_kind kind;

  // All of the characters which needed to be visited in order to reach this
  // state.
  string8_view history;

  bool is_terminal() const {
    return this->kind == lex_state_kind::unique_terminal ||
           this->kind == lex_state_kind::non_unique_terminal;
  }

  // Returns the C++ source code for this state's lex_tables::state.
  std::string name() const {
    std::string name;
    if (this->kind == lex_state_kind::unique_terminal) {
      name += "done_";
    }
    bool need_underscore = false;
    for (char8 c : this->history) {
      if (need_underscore) {
        name += '_';
      }
      name += identifier_for_character(c);
      need_underscore = true;
    }
    return name;
  }

  // Returns the C++ source code for this state's quick_lint_js::token_type.
  //
  // Precondition: This is a terminal state.
  std::string token_type_name() const {
    QLJS_ASSERT(this->is_terminal());
    std::string name = "token_type::";
    bool need_underscore = false;
    for (char8 c : this->history) {
      if (need_underscore) {
        name += '_';
      }
      name += identifier_for_character(c);
      need_underscore = true;
    }
    return name;
  }

  // Returns a string for this state's history suitable for a C++ comment.
  std::string comment() const { return make_comment(this->history); }
};

generate_lex_tables_options parse_generate_lex_tables_options(int argc,
                                                              char** argv) {
  generate_lex_tables_options o;

  arg_parser parser(argc, argv);
  while (!parser.done()) {
    if (const char* argument = parser.match_argument()) {
      std::fprintf(stderr, "error: unexpected argument: %s\n", argument);
      std::exit(2);
    } else if (const char* arg_value =
                   parser.match_option_with_value("--output"sv)) {
      o.output_path = arg_value;
    } else {
      const char* unrecognized = parser.match_anything();
      std::fprintf(stderr, "error: unrecognized option: %s\n", unrecognized);
      std::exit(2);
    }
  }

  return o;
}

struct character_class {
  explicit character_class() {}

  explicit character_class(std::size_t number)
      : number(narrow_cast<std::uint8_t>(number)) {}

  std::uint8_t number;

  friend bool operator==(character_class lhs, character_class rhs) {
    return lhs.number == rhs.number;
  }
  [[maybe_unused]] friend bool operator!=(character_class lhs,
                                          character_class rhs) {
    return !(lhs == rhs);
  }
};

struct character_class_table {
  character_class byte_to_class[256];

  std::size_t size() const { return std::size(this->byte_to_class); }

  character_class& operator[](char8 c) {
    return this->byte_to_class[static_cast<std::uint8_t>(c)];
  }

  [[maybe_unused]] const character_class& operator[](char8 c) const {
    return this->byte_to_class[static_cast<std::uint8_t>(c)];
  }

  [[maybe_unused]] character_class& operator[](std::size_t c) {
    QLJS_ASSERT(c < this->size());
    return this->byte_to_class[c];
  }

  const character_class& operator[](std::size_t c) const {
    QLJS_ASSERT(c < this->size());
    return this->byte_to_class[c];
  }
};

struct single_state_transition_table {
  // Key: character class
  // Value: new state index, or retract or table_broken
  std::vector<std::size_t> transitions;

  std::size_t& operator[](character_class c_class) {
    return this->transitions.at(c_class.number);
  }

  const std::size_t& operator[](character_class c_class) const {
    return this->transitions.at(c_class.number);
  }

  static constexpr std::size_t retract = 0xffffffffU;
  static constexpr std::size_t table_broken = 0xfffffffeU;
};

struct state_to_token_entry {
  std::string token_type;  // C++ source code.
  std::string comment;
};

struct lex_tables {
  character_class_table character_classes;
  std::size_t input_character_class_count;

  // states is partitioned by lex_state::kind: All states with
  // lex_state_kind::intermediate or lex_state_kind::non_unique_terminal come
  // before all states with lex_state_kind::unique_terminal.
  std::vector<lex_state> states;
  std::size_t intermediate_or_non_unique_terminal_state_count;
  std::size_t unique_terminal_state_count;

  // Key: old state index (corresponds with this->states)
  //      (must not correspond to lex_state_kind::unique_terminal)
  std::vector<single_state_transition_table> transition_table;

  /// Key: state index (corresponds with this->states)
  std::vector<state_to_token_entry> state_to_token_table;

  std::size_t find_state_index(string8_view history) const {
    auto it = std::find_if(this->states.begin(), this->states.end(),
                           [&](const lex_state& state) -> bool {
                             return state.history == history;
                           });
    if (it != this->states.end()) {
      return narrow_cast<std::size_t>(it - this->states.begin());
    }
    QLJS_ASSERT(false);
    return single_state_transition_table::table_broken;
  }

  // Returns a string for this character class suitable for a C++ comment.
  std::string character_class_comment(character_class c_class) const {
    if (c_class.number == this->input_character_class_count) {
      return "(other)";
    }
    for (std::size_t i = 0; i < this->character_classes.size(); ++i) {
      if (this->character_classes[i] == c_class) {
        char8 buffer = static_cast<char8>(i);
        return make_comment(string8_view(&buffer, 1));
      }
    }
    QLJS_UNIMPLEMENTED();
    return "???";
  }
};

void classify_characters(lex_tables& t) {
  character_class unallocated(0xff);
  std::fill(std::begin(t.character_classes.byte_to_class),
            std::end(t.character_classes.byte_to_class), unallocated);

  t.input_character_class_count = 0;
  for (std::size_t i = 0; i < t.states.size(); ++i) {
    const lex_state& state = t.states[i];
    if (state.history.size() == 1) {
      character_class c_class(i);
      t.character_classes[state.history[0]] = c_class;
      if (state.kind != lex_state_kind::unique_terminal) {
        t.input_character_class_count = i + 1;
      }
    }
  }
  QLJS_ASSERT(t.input_character_class_count <= unallocated.number);

  // The catch-all character class is numbered one higher than any other
  // character class. Fill in unallocated slots with this number.
  character_class other_c_class(t.input_character_class_count);
  for (character_class& c_class : t.character_classes.byte_to_class) {
    if (c_class == unallocated) {
      c_class = other_c_class;
    }
  }
}

bool is_strict_prefix_of_any_symbol(string8_view s) {
  for (string8_view symbol : symbols) {
    if (symbol.size() != s.size() && starts_with(symbol, s)) {
      return true;
    }
  }
  return false;
}

void compute_states(lex_tables& t) {
  // Find all terminal (unique_terminal and non_unique_terminal) states.
  for (string8_view symbol : symbols) {
    bool is_unique = true;
    if (is_strict_prefix_of_any_symbol(symbol)) {
      is_unique = false;
    }
    if (symbol.size() == 1) {
      // Because of our initial state optimization
      // (see NOTE[lex-table-initial]), we must treat 1-character unique
      // terminal symbols as non-unique. If we don't, then the character class
      // will be a high number, and a high number will make our tables big. We
      // force 1-character unique terminal symbols to transition to the
      // 'retract' state.
      //
      // This actually doesn't matter right because we currently use a different
      // code path for 1-character unique terminal symbols (see
      // NOTE[one-byte-symbols]).
      is_unique = false;
    }
    t.states.push_back(lex_state{
        .kind = is_unique ? lex_state_kind::unique_terminal
                          : lex_state_kind::non_unique_terminal,
        .history = symbol,
    });
  }

  // Find all intermediate states.
  auto add_intermediate_state = [&](string8_view history) -> void {
    for (const lex_state& existing_state : t.states) {
      if (existing_state.history == history) {
        QLJS_ASSERT(existing_state.kind == lex_state_kind::intermediate ||
                    existing_state.kind == lex_state_kind::non_unique_terminal);
        return;
      }
    }
    t.states.push_back(lex_state{
        .kind = lex_state_kind::intermediate,
        .history = history,
    });
  };
  for (string8_view symbol : symbols) {
    for (std::size_t i = 1; i < symbol.size() - 1; ++i) {
      add_intermediate_state(symbol.substr(0, i));
    }
  }

  // Place all intermediate_or_non_unique_terminal states before all
  // unique_terminal states.
  //
  // Also, pack initial states (i.e. states with a history of size 1) at the
  // front so that the character class numbers are as low as possible.
  std::sort(t.states.begin(), t.states.end(),
            [](const lex_state& a, const lex_state& b) -> bool {
              if (a.kind != b.kind) {
                // intermediate and non_unique_terminal states comes before
                // unique_terminal states.
                return a.kind != lex_state_kind::unique_terminal;
              }
              if (a.history.size() != b.history.size()) {
                return a.history.size() < b.history.size();
              }
              return a.history < b.history;
            });

  t.unique_terminal_state_count = narrow_cast<std::size_t>(std::count_if(
      t.states.begin(), t.states.end(), [](const lex_state& state) -> bool {
        return state.kind == lex_state_kind::unique_terminal;
      }));
  t.intermediate_or_non_unique_terminal_state_count =
      t.states.size() - t.unique_terminal_state_count;

  for (const lex_state& state : t.states) {
    t.state_to_token_table.push_back(state_to_token_entry{
        .token_type = state.is_terminal() ? state.token_type_name()
                                          : "invalid_token_type",
        .comment = state.comment(),
    });
  }
}

void compute_transition_table(lex_tables& t) {
  t.transition_table.resize(t.intermediate_or_non_unique_terminal_state_count);
  for (single_state_transition_table& tt : t.transition_table) {
    tt.transitions =
        std::vector<std::size_t>(t.input_character_class_count + 1,
                                 single_state_transition_table::retract);
  }

  for (string8_view symbol : symbols) {
    std::size_t current_state_index = t.find_state_index(symbol.substr(0, 1));
    for (std::size_t i = 1; i < symbol.size(); ++i) {
      std::size_t new_state_index = t.find_state_index(symbol.substr(0, i + 1));

      character_class c_class = t.character_classes[symbol[i]];
      if (c_class.number >= t.input_character_class_count) {
        continue;
      }

      std::size_t* new_state_index_pointer =
          &t.transition_table.at(current_state_index)[c_class];
      if (*new_state_index_pointer != single_state_transition_table::retract) {
        // If we wrote into the table already, what is there should be identical
        // to what we're about to write.
        QLJS_ASSERT(*new_state_index_pointer == new_state_index);
      }
      *new_state_index_pointer = new_state_index;
      current_state_index = new_state_index;
    }
  }
}

void dump_table_code(const lex_tables& t, ::FILE* f) {
  std::fprintf(
      f, "%s",
      R"(// Code generated by tools/generate-lex-tables.cpp. DO NOT EDIT.

// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_LEX_TABLES_GENERATED_H
#define QUICK_LINT_JS_FE_LEX_TABLES_GENERATED_H

#include <cstdint>
#include <quick-lint-js/fe/token.h>

namespace quick_lint_js {
struct lex_tables {
)");

  std::fprintf(f, R"(  // See NOTE[lex-table-class].
  static constexpr std::uint8_t character_class_table[256] = {
)");
  for (std::size_t row = 0; row < 16; ++row) {
    std::fprintf(f, "      ");
    for (std::size_t col = 0; col < 16; ++col) {
      std::fprintf(f, "%u, ", t.character_classes[row * 16 + col].number);
    }
    std::fprintf(f, " //\n");
  }
  std::fprintf(f, "  };\n");
  std::fprintf(f, "  static constexpr int character_class_count = %zu;\n",
               t.input_character_class_count);

  std::fprintf(f, "%s", R"(
  enum state : std::uint8_t {
)");
  for (std::size_t i = 0; i < t.states.size(); ++i) {
    const lex_state& state = t.states[i];
    if (i == 0) {
      std::fprintf(f, "    // Initial states:\n");
    }
    if (i == t.input_character_class_count) {
      std::fprintf(f, "\n    // Possibly-incomplete states:\n");
    }
    if (i == t.intermediate_or_non_unique_terminal_state_count) {
      std::fprintf(f, "\n    // Complete/terminal states:\n");
    }
    std::fprintf(f, "    %s,\n", state.name().c_str());
  }

  std::fprintf(f, "%s", R"(
    // An unexpected character was detected. The lexer should retract the most
    // recent byte.
    retract,

    // Indicates a bug in the table. The state machine should never enter this
    // state.
    table_broken,
  };
)");

  std::fprintf(f, "  static constexpr int input_state_count = %zu;\n\n",
               t.intermediate_or_non_unique_terminal_state_count);

  std::fprintf(f, "  // clang-format off\n");
  for (std::size_t i = 0; i < t.input_character_class_count; ++i) {
    std::fprintf(f,
                 "  "
                 "static_assert(character_class_table[static_cast<std::uint8_t>"
                 "(u8'%s')] == state::%s);\n",
                 make_cpp_string(t.states[i].history).c_str(),
                 t.states[i].name().c_str());
  }
  std::fprintf(f, "  // clang-format on\n");

  QLJS_ASSERT(t.unique_terminal_state_count > 0);
  std::fprintf(f, R"(
  // Returns true if there are no transitions from this state to any other
  // state.
  static bool is_terminal_state(state s) { return s >= %s; }
)",
               t.states[t.intermediate_or_non_unique_terminal_state_count]
                   .name()
                   .c_str());

  QLJS_ASSERT(t.unique_terminal_state_count > 0);
  std::fprintf(f, R"(
  // Returns true if there are no transitions from this state to any other
  // state.
  //
  // Precondition: s is an initial state.
  static bool is_initial_state_terminal(state s) { return s >= %s; }
)",
               t.states[t.input_character_class_count].name().c_str());

  std::fprintf(f, R"(
  static constexpr state
      transition_table[character_class_count + 1][input_state_count] = {
)");
  // +1 to include the '(other)' catch-all character class.
  for (std::size_t c_class_number = 0;
       c_class_number < t.input_character_class_count + 1; ++c_class_number) {
    character_class c_class(c_class_number);
    struct transition {
      const lex_state* old_state;     // Used for comments.
      std::string new_state_comment;  // Used for comments.
      std::string new_state_name;     // C++ source code.
    };
    std::vector<transition> transitions;
    for (std::size_t old_state_index = 0;
         old_state_index < t.transition_table.size(); ++old_state_index) {
      std::size_t new_state_index =
          t.transition_table[old_state_index][c_class];
      transition tr;
      tr.old_state = &t.states[old_state_index];
      if (new_state_index == single_state_transition_table::retract) {
        tr.new_state_name = "retract";
      } else if (new_state_index ==
                 single_state_transition_table::table_broken) {
        tr.new_state_name = "table_broken";
      } else {
        const lex_state& new_state = t.states[new_state_index];
        tr.new_state_comment = new_state.comment();
        tr.new_state_name = new_state.name();
      }
      transitions.push_back(std::move(tr));
    }

    std::size_t max_new_state_name_length = 0;
    for (const transition& tr : transitions) {
      max_new_state_name_length =
          std::max(tr.new_state_name.size(), max_new_state_name_length);
    }

    std::string c_class_comment = t.character_class_comment(c_class);
    std::fprintf(f, "          // %s\n", c_class_comment.c_str());
    std::fprintf(f, "          {\n");
    for (const transition& tr : transitions) {
      std::fprintf(f, "              %s, %*s //", tr.new_state_name.c_str(),
                   narrow_cast<int>(max_new_state_name_length -
                                    tr.new_state_name.size()),
                   "");
      if (tr.new_state_comment.empty()) {
        std::string invalid_state_source =
            tr.old_state->comment() + c_class_comment;
        std::fprintf(f, " %-*s (invalid)", 16, invalid_state_source.c_str());
      } else {
        std::fprintf(f, " %s -> %s", tr.old_state->comment().c_str(),
                     tr.new_state_comment.c_str());
      }
      std::fprintf(f, "\n");
    }
    std::fprintf(f, "          },\n");
  }
  std::fprintf(f, R"(  };
)");

  std::fprintf(f, R"(
  static constexpr token_type invalid_token_type = token_type::identifier;
  // See NOTE[lex-table-token-type].
  static constexpr token_type state_to_token[] = {
)");
  std::size_t max_token_type_length = 0;
  for (const state_to_token_entry& entry : t.state_to_token_table) {
    max_token_type_length =
        std::max(entry.token_type.size(), max_token_type_length);
  }
  for (const state_to_token_entry& entry : t.state_to_token_table) {
    std::fprintf(
        f, "      %s,%*s  // %s\n", entry.token_type.c_str(),
        narrow_cast<int>(max_token_type_length - entry.token_type.size()), "",
        entry.comment.c_str());
  }
  std::fprintf(f, "  };\n");

  std::fprintf(f, "%s", R"(};
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
)");
}

void dump_table_code(const lex_tables& t, const char* file_path) {
  FILE* f = std::fopen(file_path, "wb");
  if (f == nullptr) {
    std::fprintf(stderr, "error: failed to open %s for writing: %s\n",
                 file_path, std::strerror(errno));
    std::exit(1);
  }
  dump_table_code(t, f);
  if (std::fclose(f) != 0) {
    std::fprintf(stderr, "error: failed to write to %s: %s\n", file_path,
                 std::strerror(errno));
    std::exit(1);
  }
}

// Returns a C++ identifier for the given character. For example,
// identifier_for_character(u8'!') == "bang"sv.
const char* identifier_for_character(char8 c) {
  switch (c) {
  case u8'!':
    return "bang";
  case u8'%':
    return "percent";
  case u8'&':
    return "ampersand";
  case u8'+':
    return "plus";
  case u8'=':
    return "equal";
  case u8'>':
    return "greater";
  case u8'^':
    return "circumflex";
  case u8'|':
    return "pipe";
  default:
    QLJS_UNIMPLEMENTED();
    return "???";
  }
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")
std::string make_cpp_string(string8_view s) {
  std::string result;
  for (char8 c : s) {
    if ((u8' ' <= c && c <= u8'~') && c != u8'\\') {
      result += static_cast<char>(c);
    } else {
      QLJS_UNIMPLEMENTED();
    }
  }
  return result;
}
QLJS_WARNING_POP

std::string make_comment(string8_view s) { return make_cpp_string(s); }
}
}

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  generate_lex_tables_options o = parse_generate_lex_tables_options(argc, argv);
  if (o.output_path == nullptr) {
    std::fprintf(stderr, "error: missing --output path\n");
    return 2;
  }

  lex_tables tables;
  compute_states(tables);
  classify_characters(tables);
  compute_transition_table(tables);
  dump_table_code(tables, o.output_path);

  return 0;
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
