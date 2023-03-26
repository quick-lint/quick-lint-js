// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <gtest/gtest.h>
#include <quick-lint-js/container/hash-set.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/fe/lex-tables.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
namespace {
std::vector<string8_view> symbols = {
    u8"!"_sv,      u8"!="_sv,  u8"!=="_sv, u8"%"_sv,    u8"%="_sv, u8"&"_sv,
    u8"&&"_sv,     u8"&&="_sv, u8"&="_sv,  u8"+"_sv,    u8"++"_sv, u8"+="_sv,
    u8"="_sv,      u8"=="_sv,  u8"==="_sv, u8"=>"_sv,   u8">"_sv,  u8">="_sv,
    u8">>"_sv,     u8">>="_sv, u8">>>"_sv, u8">>>="_sv, u8"?"_sv,  u8"??"_sv,
    u8"?\x3f="_sv, u8"^"_sv,   u8"^="_sv,  u8"|"_sv,    u8"|="_sv, u8"||"_sv,
    u8"||="_sv,
};

TEST(test_lex_tables, symbols_transition_to_done_or_retract) {
  hash_set<std::uint8_t> all_character_classes;
  for (int i = 0; i < lex_tables::character_class_count; ++i) {
    all_character_classes.insert(narrow_cast<std::uint8_t>(i));
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
      hash_set<std::uint8_t> expected_retracting_character_classes =
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
      for (std::uint8_t c_class : expected_retracting_character_classes) {
        const lex_tables::state* transitions =
            lex_tables::transition_table[c_class];
        // NOTE(strager): Casts to long improve Google Test failure output.
        lex_tables::state next_state = transitions[state];
        EXPECT_EQ(long{next_state},
                  long{lex_tables::state::done_retract_for_symbol})
            << "transition from state " << long{state}
            << " with character class " << long{c_class}
            << " should arrive at the retract state";
      }
    }

  done:;
  }
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
