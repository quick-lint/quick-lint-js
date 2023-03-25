// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_LEX_TABLES_H
#define QUICK_LINT_JS_FE_LEX_TABLES_H

#include <cstdint>
#include <quick-lint-js/fe/token.h>

namespace quick_lint_js {
struct lex_tables {
  // Folds each character into a small set of equivalence classes. This makes
  // transition_table significantly smaller.
  static constexpr std::uint8_t character_class_table[256] = {
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  //
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  //
      // '!' => 3
      // '%' => 6
      // '&' => 7
      // '+' => 4
      0, 3, 0, 0, 0, 6, 7, 0, 0, 0, 0, 4, 0, 0, 0, 0,  // (sp) !"#$%&'()*+,-./
      // '=' => 1
      // '>' => 2
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0,  // 0123456789:;<=>?
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  // @ABCDEFGHIJKLMNO
      // '^' => 5
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0,  // PQRSTUVWXYZ[\]^_
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  // `abcdefghijklmno
      // '|' => 8
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0,  // pqrstuvwxyz{|}~ (del)
  };
  static constexpr int character_class_count = 9;

  enum state {
    initial,

    // Possibly-incomplete states:
    ampersand,
    ampersand_ampersand,
    bang,
    bang_equal,
    circumflex,
    equal,
    equal_equal,
    greater,
    greater_greater,
    greater_greater_greater,
    percent,
    pipe,
    pipe_pipe,
    plus,

    input_state_count,
    first_done_state = input_state_count,

    // Complete/terminal states:
    done_ampersand_ampersand_equal = first_done_state,
    done_ampersand_equal,
    done_bang_equal_equal,
    done_circumflex_equal,
    done_equal_equal_equal,
    done_equal_greater,
    done_greater_equal,
    done_greater_greater_equal,
    done_greater_greater_greater_equal,
    done_percent_equal,
    done_pipe_equal,
    done_pipe_pipe_equal,
    done_plus_equal,
    done_plus_plus,

    // An unexpected character was detected. The lexer should retract the most
    // recent byte.
    retract,

    // Indicates a bug in the table. The state machine should never enter this
    // state.
    table_broken,
  };

  // Returns true if there are no transitions from this state to any other
  // state.
  static bool is_terminal_state(state s) {
    return s >= state::input_state_count;
  }

  static constexpr state
      transition_table[character_class_count][input_state_count] = {
          /*[invalid_fold] =*/{
              /*[initial] =*/table_broken,
              /*[ampersand] =*/retract,
              /*[ampersand_ampersand] =*/retract,
              /*[bang] =*/retract,
              /*[bang_equal] =*/retract,
              /*[circumflex] =*/retract,
              /*[equal] =*/retract,
              /*[equal_equal] =*/retract,
              /*[greater] =*/retract,
              /*[greater_greater] =*/retract,
              /*[greater_greater_greater] =*/retract,
              /*[percent] =*/retract,
              /*[pipe] =*/retract,
              /*[pipe_pipe] =*/retract,
              /*[plus] =*/retract,
          },
          /*[character_class_table[u8'=']] =*/
          {
              /*[initial] =*/equal,
              /*[ampersand] =*/done_ampersand_equal,
              /*[ampersand_ampersand] =*/done_ampersand_ampersand_equal,
              /*[bang] =*/bang_equal,
              /*[bang_equal] =*/done_bang_equal_equal,
              /*[circumflex] =*/done_circumflex_equal,
              /*[equal] =*/equal_equal,
              /*[equal_equal] =*/done_equal_equal_equal,
              /*[greater] =*/done_greater_equal,
              /*[greater_greater] =*/done_greater_greater_equal,
              /*[greater_greater_greater] =*/done_greater_greater_greater_equal,
              /*[percent] =*/done_percent_equal,
              /*[pipe] =*/done_pipe_equal,
              /*[pipe_pipe] =*/done_pipe_pipe_equal,
              /*[plus] =*/done_plus_equal,
          },
          /*[character_class_table[u8'>']] =*/
          {
              /*[initial] =*/greater,
              /*[ampersand] =*/retract,
              /*[ampersand_ampersand] =*/retract,
              /*[bang] =*/retract,
              /*[bang_equal] =*/retract,
              /*[circumflex] =*/retract,
              /*[equal] =*/done_equal_greater,
              /*[equal_equal] =*/retract,
              /*[greater] =*/greater_greater,
              /*[greater_greater] =*/greater_greater_greater,
              /*[greater_greater_greater] =*/retract,
              /*[percent] =*/retract,
              /*[pipe] =*/retract,
              /*[pipe_pipe] =*/retract,
              /*[plus] =*/retract,
          },
          /*[character_class_table[u8'!']] =*/
          {
              /*[initial] =*/bang,
              /*[ampersand] =*/retract,
              /*[ampersand_ampersand] =*/retract,
              /*[bang] =*/retract,
              /*[bang_equal] =*/retract,
              /*[circumflex] =*/retract,
              /*[equal] =*/retract,
              /*[equal_equal] =*/retract,
              /*[greater] =*/retract,
              /*[greater_greater] =*/retract,
              /*[greater_greater_greater] =*/retract,
              /*[percent] =*/retract,
              /*[pipe] =*/retract,
              /*[pipe_pipe] =*/retract,
              /*[plus] =*/retract,
          },
          /*[character_class_table[u8'+']] =*/
          {
              /*[initial] =*/plus,
              /*[ampersand] =*/retract,
              /*[ampersand_ampersand] =*/retract,
              /*[bang] =*/retract,
              /*[bang_equal] =*/retract,
              /*[circumflex] =*/retract,
              /*[equal] =*/retract,
              /*[equal_equal] =*/retract,
              /*[greater] =*/retract,
              /*[greater_greater] =*/retract,
              /*[greater_greater_greater] =*/retract,
              /*[percent] =*/retract,
              /*[pipe] =*/retract,
              /*[pipe_pipe] =*/retract,
              /*[plus] =*/done_plus_plus,
          },
          /*[character_class_table[u8'^']] =*/
          {
              /*[initial] =*/circumflex,
              /*[ampersand] =*/retract,
              /*[ampersand_ampersand] =*/retract,
              /*[bang] =*/retract,
              /*[bang_equal] =*/retract,
              /*[circumflex] =*/retract,
              /*[equal] =*/retract,
              /*[equal_equal] =*/retract,
              /*[greater] =*/retract,
              /*[greater_greater] =*/retract,
              /*[greater_greater_greater] =*/retract,
              /*[percent] =*/retract,
              /*[pipe] =*/retract,
              /*[pipe_pipe] =*/retract,
              /*[plus] =*/retract,
          },
          /*[character_class_table[u8'%']] =*/
          {
              /*[initial] =*/percent,
              /*[ampersand] =*/retract,
              /*[ampersand_ampersand] =*/retract,
              /*[bang] =*/retract,
              /*[bang_equal] =*/retract,
              /*[circumflex] =*/retract,
              /*[equal] =*/retract,
              /*[equal_equal] =*/retract,
              /*[greater] =*/retract,
              /*[greater_greater] =*/retract,
              /*[greater_greater_greater] =*/retract,
              /*[percent] =*/retract,
              /*[pipe] =*/retract,
              /*[pipe_pipe] =*/retract,
              /*[plus] =*/retract,
          },
          /*[character_class_table[u8'&']] =*/
          {
              /*[initial] =*/ampersand,
              /*[ampersand] =*/ampersand_ampersand,
              /*[ampersand_ampersand] =*/retract,
              /*[bang] =*/retract,
              /*[bang_equal] =*/retract,
              /*[circumflex] =*/retract,
              /*[equal] =*/retract,
              /*[equal_equal] =*/retract,
              /*[greater] =*/retract,
              /*[greater_greater] =*/retract,
              /*[greater_greater_greater] =*/retract,
              /*[percent] =*/retract,
              /*[pipe] =*/retract,
              /*[pipe_pipe] =*/retract,
              /*[plus] =*/retract,
          },
          /*[character_class_table[u8'|']] =*/
          {
              /*[initial] =*/pipe,
              /*[ampersand] =*/retract,
              /*[ampersand_ampersand] =*/retract,
              /*[bang] =*/retract,
              /*[bang_equal] =*/retract,
              /*[circumflex] =*/retract,
              /*[equal] =*/retract,
              /*[equal_equal] =*/retract,
              /*[greater] =*/retract,
              /*[greater_greater] =*/retract,
              /*[greater_greater_greater] =*/retract,
              /*[percent] =*/retract,
              /*[pipe] =*/pipe_pipe,
              /*[pipe_pipe] =*/retract,
              /*[plus] =*/retract,
          },
  };

  static constexpr token_type state_to_token[] = {
      /*[initial] =*/token_type::identifier,  // Invalid.

      /*[ampersand] =*/token_type::ampersand,
      /*[ampersand_ampersand] =*/token_type::ampersand_ampersand,
      /*[bang] =*/token_type::bang,
      /*[bang_equal] =*/token_type::bang_equal,
      /*[circumflex] =*/token_type::circumflex,
      /*[equal] =*/token_type::equal,
      /*[equal_equal] =*/token_type::equal_equal,
      /*[greater] =*/token_type::greater,
      /*[greater_greater] =*/token_type::greater_greater,
      /*[greater_greater_greater] =*/token_type::greater_greater_greater,
      /*[percent] =*/token_type::percent,
      /*[pipe] =*/token_type::pipe,
      /*[pipe_pipe] =*/token_type::pipe_pipe,
      /*[plus] =*/token_type::plus,

      /*[done_ampersand_ampersand_equal] =*/
      token_type::ampersand_ampersand_equal,
      /*[done_ampersand_equal] =*/token_type::ampersand_equal,
      /*[done_bang_equal_equal] =*/token_type::bang_equal_equal,
      /*[done_circumflex_equal] =*/token_type::circumflex_equal,
      /*[done_equal_equal_equal] =*/token_type::equal_equal_equal,
      /*[done_equal_greater] =*/token_type::equal_greater,
      /*[done_greater_equal] =*/token_type::greater_equal,
      /*[done_greater_greater_equal] =*/token_type::greater_greater_equal,
      /*[done_greater_greater_greater_equal] =*/
      token_type::greater_greater_greater_equal,
      /*[done_percent_equal] =*/token_type::percent_equal,
      /*[done_pipe_equal] =*/token_type::pipe_equal,
      /*[done_pipe_pipe_equal] =*/token_type::pipe_pipe_equal,
      /*[done_plus_equal] =*/token_type::plus_equal,
      /*[done_plus_plus] =*/token_type::plus_plus,
  };
};
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
