// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <gtest/gtest.h>
#include <quick-lint-js/container/hash-set.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/fe/lex-tables.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <string>
#include <vector>

#define QLJS_DUMP_TRANSITION_TABLE_FINAL_STATE_HISTORIES 0

QLJS_WARNING_IGNORE_CLANG("-Wchar-subscripts")

namespace quick_lint_js {
namespace {
std::vector<string8_view> symbols = {
    u8"!"_sv,  u8"!="_sv,  u8"!=="_sv, u8"%"_sv,      u8"%="_sv,  u8"&"_sv,
    u8"&&"_sv, u8"&&="_sv, u8"&="_sv,  u8"*"_sv,      u8"**"_sv,  u8"**="_sv,
    u8"*="_sv, u8"+"_sv,   u8"++"_sv,  u8"+="_sv,     u8"-"_sv,   u8"--"_sv,
    u8"-="_sv, u8"."_sv,   u8"..."_sv, u8"/"_sv,      u8"/="_sv,  u8"<"_sv,
    u8"<<"_sv, u8"<<="_sv, u8"<="_sv,  u8"="_sv,      u8"=="_sv,  u8"==="_sv,
    u8"=>"_sv, u8">"_sv,   u8">="_sv,  u8">>"_sv,     u8">>="_sv, u8">>>"_sv,
    u8"?"_sv,  u8"?."_sv,  u8"??"_sv,  u8"?\x3f="_sv, u8"^"_sv,   u8"^="_sv,
    u8"|"_sv,  u8"|="_sv,  u8"||"_sv,  u8"||="_sv,
};

std::string pretty_character_class(lex_tables::state_type);

TEST(test_lex_tables, symbols_transition_to_done_or_retract) {
  hash_set<lex_tables::state_type> all_character_classes;
  for (lex_tables::state_type c_class = 0;
       c_class < lex_tables::character_class_count; ++c_class) {
    all_character_classes.insert(c_class);
  }

  for (string8_view symbol : symbols) {
    SCOPED_TRACE(out_string8(symbol));
    ASSERT_GT(symbol.size(), 0);

    lex_tables::state state = static_cast<lex_tables::state>(
        lex_tables::character_class_table[static_cast<std::uint8_t>(
            symbol[0])]);

    // Case A: Single-character unique terminal symbol such as '('.
    {
      if (lex_tables::is_initial_state_terminal(state)) {
        EXPECT_EQ(symbol.size(), 1) << "initial terminal state should indicate "
                                       "that the symbol has only one character";
        goto done;
      }
    }

    // Case B: Symbol with unique terminal state such as '++' (e.g. there is no
    // '+++' or '++=' symbol).
    {
      for (std::size_t i = 1; i < symbol.size(); ++i) {
        const lex_tables::state* transitions = lex_tables::transition_table
            [lex_tables::character_class_table[static_cast<std::uint8_t>(
                symbol[i])]];
        state = transitions[state];
        if (lex_tables::is_terminal_state(state)) {
          EXPECT_EQ(symbol.substr(i + 1), u8""_sv)
              << "terminal state should indicate that the symbol has no more "
                 "characters";
          goto done;
        }
      }
    }

    // Case C: Symbol with non-unique terminal state such as '>>' (e.g. there is
    // '>>>' and '>>='). Every next character in the symbol table should cause a
    // transition to the retract state.
    {
      hash_set<lex_tables::state_type> expected_retracting_character_classes =
          all_character_classes;
      for (string8_view other_symbol : symbols) {
        if (other_symbol.size() > symbol.size() &&
            starts_with(other_symbol, symbol)) {
          expected_retracting_character_classes.erase(
              lex_tables::character_class_table[static_cast<std::uint8_t>(
                  other_symbol[symbol.size()])]);
        }
      }
      ASSERT_FALSE(expected_retracting_character_classes.empty());
      for (lex_tables::state_type c_class :
           expected_retracting_character_classes) {
        if (symbol == u8"."_sv &&
            c_class == lex_tables::character_class_table[u8'0']) {
          // '.9' does not retract.
          continue;
        }
        if (symbol == u8"?."_sv &&
            c_class == lex_tables::character_class_table[u8'0']) {
          // '?.9' retracts two characters, not one.
          continue;
        }
        if (symbol == u8"<"_sv &&
            c_class == lex_tables::character_class_table[u8'!']) {
          // '<!--' is parsed specially.
          continue;
        }
        if (symbol == u8">>"_sv &&
            c_class == lex_tables::character_class_table[u8'>']) {
          // '>>>' and '>>>=' are parsed specially.
          continue;
        }
        if ((symbol == u8"*"_sv || symbol == u8"**"_sv) &&
            c_class == lex_tables::character_class_table[u8'/']) {
          // '*/' and '**/' are parsed specially.
          continue;
        }
        if (symbol == u8"/"_sv &&
            (c_class == lex_tables::character_class_table[u8'*'] ||
             c_class == lex_tables::character_class_table[u8'/'])) {
          // '/*' and '//' are parsed specially.
          continue;
        }

        const lex_tables::state* transitions =
            lex_tables::transition_table[c_class];
        // NOTE(strager): Casts to long improve Google Test failure output.
        lex_tables::state next_state = transitions[state];
        EXPECT_EQ(long{next_state},
                  long{lex_tables::state::done_retract_for_symbol})
            << "transition from state " << long{state}
            << " with character class " << pretty_character_class(c_class)
            << " should arrive at the retract state";
      }
    }

  done:;
  }
}

TEST(test_lex_tables, maximum_depth) {
  constexpr std::uint8_t depth_limit = 20;

  struct state_history {
    lex_tables::state_type c_class_history[depth_limit];
    std::uint8_t depth;  // Number of entries in c_class_history.

    std::string to_string() const {
      std::string result;
      for (std::uint8_t d = 0; d < this->depth; ++d) {
        if (d > 0) {
          result += ", "sv;
        }
        result += pretty_character_class(this->c_class_history[d]);
      }
      return result;
    }
  };
  struct queue_entry {
    state_history history;
    // Invariant: next_c_class == history.c_class_history[history.depth - 1]
    lex_tables::state_type next_c_class;
    lex_tables::state_type next_state;
  };

  state_history max_depths[lex_tables::character_class_count]
                          [lex_tables::input_state_count] = {};

  std::vector<queue_entry> queue;
  queue.reserve(narrow_cast<std::size_t>(lex_tables::character_class_count) *
                lex_tables::input_state_count);

  // First level: Initial state can be any character class except
  // other_character_class.
  for (lex_tables::state_type state = 0;
       state < lex_tables::character_class::other_character_class; ++state) {
    if (!lex_tables::is_initial_state_terminal(state)) {
      for (lex_tables::state_type c_class = 0;
           c_class < lex_tables::character_class_count; ++c_class) {
        queue.push_back(queue_entry{
            .history =
                state_history{
                    .c_class_history = {state, c_class},
                    .depth = 2,
                },
            .next_c_class = c_class,
            .next_state = state,
        });
      }
    }
  }

  // Second and subsequent levels: State can be any input or transition state.
  while (!queue.empty()) {
    queue_entry entry = queue.back();
    queue.pop_back();

    ASSERT_LE(entry.next_c_class, lex_tables::character_class_count);
    ASSERT_LE(entry.next_state, lex_tables::input_state_count);
    if (entry.history.depth >
        max_depths[entry.next_c_class][entry.next_state].depth) {
      max_depths[entry.next_c_class][entry.next_state] = entry.history;
    }

    lex_tables::state_type new_state =
        lex_tables::transition_table[entry.next_c_class][entry.next_state];
    if (!lex_tables::is_terminal_state(new_state)) {
      ASSERT_LT(entry.history.depth, depth_limit)
          << "transition table should not have cycles";
      for (lex_tables::state_type new_c_class = 0;
           new_c_class < lex_tables::character_class_count; ++new_c_class) {
        queue_entry new_entry = entry;
        new_entry.history.c_class_history[entry.history.depth] = new_c_class;
        new_entry.history.depth =
            narrow_cast<std::uint8_t>(new_entry.history.depth + 1);
        new_entry.next_c_class = new_c_class;
        new_entry.next_state = new_state;
        queue.push_back(new_entry);
      }
    }
  }

#if QLJS_DUMP_TRANSITION_TABLE_FINAL_STATE_HISTORIES
  for (lex_tables::state_type c_class = 0;
       c_class < lex_tables::character_class_count; ++c_class) {
    for (lex_tables::state_type final_state = 0;
         final_state < lex_tables::input_state_count; ++final_state) {
      state_history& history = max_depths[c_class][final_state];
      std::fprintf(stderr, "max_depths[%s][%d] = %d (%s)\n",
                   pretty_character_class(c_class).c_str(), final_state,
                   history.depth, history.to_string().c_str());
    }
  }
#endif

  for (lex_tables::state_type c_class = 0;
       c_class < lex_tables::character_class_count; ++c_class) {
    for (lex_tables::state_type final_state = 0;
         final_state < lex_tables::input_state_count; ++final_state) {
      state_history& history = max_depths[c_class][final_state];
      ASSERT_LE(history.depth, lex_tables::maximum_state_depth)
          << "final_state=" << long{final_state}
          << ", history=" << history.to_string();
    }
  }
}

std::string pretty_character_class(lex_tables::state_type c_class) {
  std::string result = std::to_string(narrow_cast<unsigned>(c_class)) + " (";
  if (c_class == lex_tables::character_class::other_character_class) {
    return result + "other)";
  }
  for (std::size_t i = 0; i < std::size(lex_tables::character_class_table);
       ++i) {
    if (lex_tables::character_class_table[i] == c_class) {
      result += static_cast<char>(i);
    }
  }
  result += ')';
  return result;
}
}
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
