// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_LEX_TABLES_H
#define QUICK_LINT_JS_FE_LEX_TABLES_H

#include <climits>
#include <cstdint>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>

#if QLJS_HAVE_GNU_COMPUTED_GOTO
#define QLJS_LEX_HANDLER_USE_COMPUTED_GOTO 1
#else
#define QLJS_LEX_HANDLER_USE_COMPUTED_GOTO 0
#endif

QLJS_WARNING_PUSH

#if QLJS_LEX_HANDLER_USE_COMPUTED_GOTO
QLJS_WARNING_IGNORE_CLANG("-Wgnu-label-as-value")
// "taking the address of a label is non-standard"
QLJS_WARNING_IGNORE_GCC("-Wpedantic")
#endif

namespace quick_lint_js {
// Character classification and state machine transition tables for
// the lexer class.
//
// The state machine implements a deterministic finite automaton (DFA).
//
// Currently, the state machine only recognizes plain symbols such as "+=",
// "||=", and "~".
//
// == State machine lookup algorithm ==
//
// The lookup algorithm code lives in lex_tables::try_parse_current_token. See
// NOTE[lex-table-lookup].
//
// The algorithm requires four tables which are accessed in the following
// order:
//
// 1. Character classification table (character_class_table).
//    See NOTE[lex-table-class].
// 2. State transition table (transition_table).
// 3. Handler table.
// 4. Terminal state lookup table (state_to_token).
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
// NOTE[lex-table-state-order]: States are carefully ordered:
//
// A. Initial non-terminal states.
// B. Initial terminal states. Currently, this set is empty, but if it wasn't,
//    it'd be like A above except they have no transitions.
// C. Intermediate and possibly-terminal states.
// D. Complete states.
// E. Misc states.
//
// The order of these groups is carefully chosen to keep the transition table
// small:
//
// * The initial states (A and B) are indexes into the transition table, so
//   their number must be low. They have numbers equal to some character classes
//   (see NOTE[lex-table-initial]), so their number must be very low.
// * Intermediate and possibly-terminal states are indexes into the transition
//   table, so their table must be low.
//
// The order of these groups also makes certain queries more efficient:
//
// * is_terminal_state can check if a state is a complete state or a misc
//   state (D or E) using a single >=.
// * is_initial_state_terminal can check if a state is an initial terminal
//   state (A) using a single >=.
//
// == Improvements ==
//
// NOTE[lex-table-token-type]: For now, classification only returns a valid
// token type. This should be changed in the future if non-trivial cases which
// require further processing need to be supported.
struct lex_tables {
  // See NOTE[lex-table-class].
  enum character_class : std::uint8_t {
    bang,
    percent,
    ampersand,
    plus,
    equal,
    greater,
    circumflex,
    pipe,

    // Must be last:
    other_character_class,

    character_class_count,
  };

  // Folds each character into a small set of equivalence classes. This makes
  // transition_table significantly smaller.
  //
  // See NOTE[lex-table-class].
#define _ other_character_class
  // clang-format off
  static constexpr std::uint8_t character_class_table[256] = {
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  //
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  //
      _, bang, _, _, _, percent, ampersand, _, _, _, _, plus, _,    _,     _,          _,  // (sp) !"#$%&'()*+,-./
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    equal, greater,    _,  // 0123456789:;<=>?
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  // @ABCDEFGHIJKLMNO
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     circumflex, _,  // PQRSTUVWXYZ[\]^_
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  // `abcdefghijklmno
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    pipe, _,     _,          _,  // pqrstuvwxyz{|}~ (del)
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  //
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  //
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  //
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  //
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  //
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  //
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  //
      _, _,    _, _, _, _,       _,         _, _, _, _, _,    _,    _,     _,          _,  //
  };
  // clang-format on
#undef _

  // clang-format off
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'!')] == character_class::bang);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'%')] == character_class::percent);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'&')] == character_class::ampersand);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'+')] == character_class::plus);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'=')] == character_class::equal);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'>')] == character_class::greater);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'^')] == character_class::circumflex);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'|')] == character_class::pipe);
  // clang-format on

  using state_type = std::uint8_t;

  // How many bits in the state are reserved for the state number for
  // intermediate and non-unique terminal states, or for extra data for unique
  // terminal states and the retract terminal state.
  static constexpr int state_data_bits = 5;
  static constexpr state_type state_data_mask = (1 << state_data_bits) - 1;

  // How many bits in the state are reserved for selecting the handler.
  // See enum handler.
  static constexpr int state_handler_bits = 3;

  static_assert(sizeof(state_type) >=
                    (state_data_bits + state_handler_bits) / CHAR_BIT,
                "state_type should be big enough to fit all data bits and "
                "handler bits");

  enum handler {
    handler_transition = 0,
    handler_done_retract_for_symbol,

    // The state data is the index into unique_terminal_symbol_tokens.
    handler_done_unique_terminal_symbol,

    handler_count,
  };
  static_assert(handler_transition == 0,
                "handler_transition must be 0 to keep states in the transition "
                "table low-numbered");
  static_assert(handler_count < (1 << state_handler_bits),
                "state_handler_bits should be big enough to fit all handlers");

#define QLJS_STATE(handler, data) (((handler) << state_data_bits) | (data))
  enum state : state_type {
    // Initial states.
    // Handler must be handler_transition (0) for these states.
    // See enum character_class and NOTE[lex-table-initial].

    // Possibly-incomplete states.
    // Handler must be handler_transition (0) for these states.
    bang_equal = other_character_class,
    ampersand_ampersand,
    equal_equal,
    greater_greater,
    pipe_pipe,
    greater_greater_greater,

    input_state_count,

    // Complete/terminal states:
    // clang-format off
    done_percent_equal                 = QLJS_STATE(handler_done_unique_terminal_symbol, 0),
    done_ampersand_equal               = QLJS_STATE(handler_done_unique_terminal_symbol, 1),
    done_plus_plus                     = QLJS_STATE(handler_done_unique_terminal_symbol, 2),
    done_plus_equal                    = QLJS_STATE(handler_done_unique_terminal_symbol, 3),
    done_equal_greater                 = QLJS_STATE(handler_done_unique_terminal_symbol, 4),
    done_greater_equal                 = QLJS_STATE(handler_done_unique_terminal_symbol, 5),
    done_circumflex_equal              = QLJS_STATE(handler_done_unique_terminal_symbol, 6),
    done_pipe_equal                    = QLJS_STATE(handler_done_unique_terminal_symbol, 7),
    done_bang_equal_equal              = QLJS_STATE(handler_done_unique_terminal_symbol, 8),
    done_ampersand_ampersand_equal     = QLJS_STATE(handler_done_unique_terminal_symbol, 9),
    done_equal_equal_equal             = QLJS_STATE(handler_done_unique_terminal_symbol, 10),
    done_greater_greater_equal         = QLJS_STATE(handler_done_unique_terminal_symbol, 11),
    done_pipe_pipe_equal               = QLJS_STATE(handler_done_unique_terminal_symbol, 12),
    done_greater_greater_greater_equal = QLJS_STATE(handler_done_unique_terminal_symbol, 13),
    // clang-format on

    // An unexpected character was detected. The lexer should retract the most
    // recent byte then consult retract_for_symbol_tokens using the previous
    // state.
    done_retract_for_symbol = QLJS_STATE(handler_done_retract_for_symbol, 0),
  };
#undef QLJS_STATE

  // Returns true if there are no transitions from this state to any other
  // state.
  static bool is_terminal_state(state s) {
    // Any state with a handler other than handler_transition is a terminal
    // state.
    static_assert(handler_transition == 0);
    bool is_terminal = s >= (1 << state_data_bits);
    QLJS_ASSERT(is_terminal == (s >= input_state_count));
    QLJS_ASSERT(is_terminal == ((s >> state_data_bits) != handler_transition));
    return is_terminal;
  }

  // Returns true if there are no transitions from this state to any other
  // state.
  //
  // Precondition: s is an initial state.
  static bool is_initial_state_terminal(state s) {
    // See NOTE[lex-table-state-order].
    return static_cast<state_type>(s) >= other_character_class;
  }

  static constexpr state
      transition_table[character_class_count + 1][input_state_count] = {
          // !
          {
              done_retract_for_symbol,  // !!               (invalid)
              done_retract_for_symbol,  // %!               (invalid)
              done_retract_for_symbol,  // &!               (invalid)
              done_retract_for_symbol,  // +!               (invalid)
              done_retract_for_symbol,  // =!               (invalid)
              done_retract_for_symbol,  // >!               (invalid)
              done_retract_for_symbol,  // ^!               (invalid)
              done_retract_for_symbol,  // |!               (invalid)
              done_retract_for_symbol,  // !=!              (invalid)
              done_retract_for_symbol,  // &&!              (invalid)
              done_retract_for_symbol,  // ==!              (invalid)
              done_retract_for_symbol,  // >>!              (invalid)
              done_retract_for_symbol,  // ||!              (invalid)
              done_retract_for_symbol,  // >>>!             (invalid)
          },
          // %
          {
              done_retract_for_symbol,  // !%               (invalid)
              done_retract_for_symbol,  // %%               (invalid)
              done_retract_for_symbol,  // &%               (invalid)
              done_retract_for_symbol,  // +%               (invalid)
              done_retract_for_symbol,  // =%               (invalid)
              done_retract_for_symbol,  // >%               (invalid)
              done_retract_for_symbol,  // ^%               (invalid)
              done_retract_for_symbol,  // |%               (invalid)
              done_retract_for_symbol,  // !=%              (invalid)
              done_retract_for_symbol,  // &&%              (invalid)
              done_retract_for_symbol,  // ==%              (invalid)
              done_retract_for_symbol,  // >>%              (invalid)
              done_retract_for_symbol,  // ||%              (invalid)
              done_retract_for_symbol,  // >>>%             (invalid)
          },
          // &
          {
              done_retract_for_symbol,  // !&               (invalid)
              done_retract_for_symbol,  // %&               (invalid)
              ampersand_ampersand,      // & -> &&
              done_retract_for_symbol,  // +&               (invalid)
              done_retract_for_symbol,  // =&               (invalid)
              done_retract_for_symbol,  // >&               (invalid)
              done_retract_for_symbol,  // ^&               (invalid)
              done_retract_for_symbol,  // |&               (invalid)
              done_retract_for_symbol,  // !=&              (invalid)
              done_retract_for_symbol,  // &&&              (invalid)
              done_retract_for_symbol,  // ==&              (invalid)
              done_retract_for_symbol,  // >>&              (invalid)
              done_retract_for_symbol,  // ||&              (invalid)
              done_retract_for_symbol,  // >>>&             (invalid)
          },
          // +
          {
              done_retract_for_symbol,  // !+               (invalid)
              done_retract_for_symbol,  // %+               (invalid)
              done_retract_for_symbol,  // &+               (invalid)
              done_plus_plus,           // + -> ++
              done_retract_for_symbol,  // =+               (invalid)
              done_retract_for_symbol,  // >+               (invalid)
              done_retract_for_symbol,  // ^+               (invalid)
              done_retract_for_symbol,  // |+               (invalid)
              done_retract_for_symbol,  // !=+              (invalid)
              done_retract_for_symbol,  // &&+              (invalid)
              done_retract_for_symbol,  // ==+              (invalid)
              done_retract_for_symbol,  // >>+              (invalid)
              done_retract_for_symbol,  // ||+              (invalid)
              done_retract_for_symbol,  // >>>+             (invalid)
          },
          // =
          {
              bang_equal,                          // ! -> !=
              done_percent_equal,                  // % -> %=
              done_ampersand_equal,                // & -> &=
              done_plus_equal,                     // + -> +=
              equal_equal,                         // = -> ==
              done_greater_equal,                  // > -> >=
              done_circumflex_equal,               // ^ -> ^=
              done_pipe_equal,                     // | -> |=
              done_bang_equal_equal,               // != -> !==
              done_ampersand_ampersand_equal,      // && -> &&=
              done_equal_equal_equal,              // == -> ===
              done_greater_greater_equal,          // >> -> >>=
              done_pipe_pipe_equal,                // || -> ||=
              done_greater_greater_greater_equal,  // >>> -> >>>=
          },
          // >
          {
              done_retract_for_symbol,  // !>               (invalid)
              done_retract_for_symbol,  // %>               (invalid)
              done_retract_for_symbol,  // &>               (invalid)
              done_retract_for_symbol,  // +>               (invalid)
              done_equal_greater,       // = -> =>
              greater_greater,          // > -> >>
              done_retract_for_symbol,  // ^>               (invalid)
              done_retract_for_symbol,  // |>               (invalid)
              done_retract_for_symbol,  // !=>              (invalid)
              done_retract_for_symbol,  // &&>              (invalid)
              done_retract_for_symbol,  // ==>              (invalid)
              greater_greater_greater,  // >> -> >>>
              done_retract_for_symbol,  // ||>              (invalid)
              done_retract_for_symbol,  // >>>>             (invalid)
          },
          // ^
          {
              done_retract_for_symbol,  // !^               (invalid)
              done_retract_for_symbol,  // %^               (invalid)
              done_retract_for_symbol,  // &^               (invalid)
              done_retract_for_symbol,  // +^               (invalid)
              done_retract_for_symbol,  // =^               (invalid)
              done_retract_for_symbol,  // >^               (invalid)
              done_retract_for_symbol,  // ^^               (invalid)
              done_retract_for_symbol,  // |^               (invalid)
              done_retract_for_symbol,  // !=^              (invalid)
              done_retract_for_symbol,  // &&^              (invalid)
              done_retract_for_symbol,  // ==^              (invalid)
              done_retract_for_symbol,  // >>^              (invalid)
              done_retract_for_symbol,  // ||^              (invalid)
              done_retract_for_symbol,  // >>>^             (invalid)
          },
          // |
          {
              done_retract_for_symbol,  // !|               (invalid)
              done_retract_for_symbol,  // %|               (invalid)
              done_retract_for_symbol,  // &|               (invalid)
              done_retract_for_symbol,  // +|               (invalid)
              done_retract_for_symbol,  // =|               (invalid)
              done_retract_for_symbol,  // >|               (invalid)
              done_retract_for_symbol,  // ^|               (invalid)
              pipe_pipe,                // | -> ||
              done_retract_for_symbol,  // !=|              (invalid)
              done_retract_for_symbol,  // &&|              (invalid)
              done_retract_for_symbol,  // ==|              (invalid)
              done_retract_for_symbol,  // >>|              (invalid)
              done_retract_for_symbol,  // |||              (invalid)
              done_retract_for_symbol,  // >>>|             (invalid)
          },
          // (other)
          {
              done_retract_for_symbol,  // !(other)         (invalid)
              done_retract_for_symbol,  // %(other)         (invalid)
              done_retract_for_symbol,  // &(other)         (invalid)
              done_retract_for_symbol,  // +(other)         (invalid)
              done_retract_for_symbol,  // =(other)         (invalid)
              done_retract_for_symbol,  // >(other)         (invalid)
              done_retract_for_symbol,  // ^(other)         (invalid)
              done_retract_for_symbol,  // |(other)         (invalid)
              done_retract_for_symbol,  // !=(other)        (invalid)
              done_retract_for_symbol,  // &&(other)        (invalid)
              done_retract_for_symbol,  // ==(other)        (invalid)
              done_retract_for_symbol,  // >>(other)        (invalid)
              done_retract_for_symbol,  // ||(other)        (invalid)
              done_retract_for_symbol,  // >>>(other)       (invalid)
          },
  };

  static constexpr token_type invalid_token_type = token_type::identifier;
  // See NOTE[lex-table-token-type].
  static constexpr token_type unique_terminal_symbol_tokens[] = {
      token_type::percent_equal,                  // %=
      token_type::ampersand_equal,                // &=
      token_type::plus_plus,                      // ++
      token_type::plus_equal,                     // +=
      token_type::equal_greater,                  // =>
      token_type::greater_equal,                  // >=
      token_type::circumflex_equal,               // ^=
      token_type::pipe_equal,                     // |=
      token_type::bang_equal_equal,               // !==
      token_type::ampersand_ampersand_equal,      // &&=
      token_type::equal_equal_equal,              // ===
      token_type::greater_greater_equal,          // >>=
      token_type::pipe_pipe_equal,                // ||=
      token_type::greater_greater_greater_equal,  // >>>=
  };

  // Key: a state < input_state_count
  // Value: corresponding token_type
  static constexpr token_type retract_for_symbol_tokens[] = {
      token_type::bang,                     // !
      token_type::percent,                  // %
      token_type::ampersand,                // &
      token_type::plus,                     // +
      token_type::equal,                    // =
      token_type::greater,                  // >
      token_type::circumflex,               // ^
      token_type::pipe,                     // |
      token_type::bang_equal,               // !=
      token_type::ampersand_ampersand,      // &&
      token_type::equal_equal,              // ==
      token_type::greater_greater,          // >>
      token_type::pipe_pipe,                // ||
      token_type::greater_greater_greater,  // >>>
  };

  // NOTE[lex-table-lookup]:
  static bool try_parse_current_token(lexer* l) {
    const char8* input = l->input_;

    lex_tables::state old_state;

#if QLJS_LEX_HANDLER_USE_COMPUTED_GOTO
    static void* handler_table[] = {
        /*[handler_transition] = */ &&transition,
        /*[handler_done_retract_for_symbol] = */ &&done_retract_for_symbol,
        /*[handler_done_unique_terminal_symbol] = */
        &&done_unique_terminal_symbol,
    };
#endif

    // The first lookup is special. In normal DFA tables, there is on initial
    // state. In our table, there are many initial states. The character class
    // of the first character corresponds to the initial state. Therefore, for
    // the first character, do not use lex_tables::transition_table. See
    // NOTE[lex-table-initial].
    lex_tables::state new_state = static_cast<lex_tables::state>(
        lex_tables::character_class_table[static_cast<std::uint8_t>(*input)]);
    QLJS_ASSERT((new_state >> state_data_bits) !=
                handler_done_retract_for_symbol);
    input += 1;
    if (lex_tables::is_initial_state_terminal(new_state)) {
#if QLJS_LEX_HANDLER_USE_COMPUTED_GOTO
      goto* handler_table[new_state >> state_data_bits];
#else
      goto dispatch_to_handler;
#endif
    } else {
      goto transition;
    }

  transition : {
    old_state = new_state;
    const lex_tables::state* transitions = lex_tables::transition_table
        [lex_tables::character_class_table[static_cast<std::uint8_t>(*input)]];
    new_state = transitions[new_state];
    input += 1;
#if QLJS_LEX_HANDLER_USE_COMPUTED_GOTO
    goto* handler_table[new_state >> state_data_bits];
#else
    if (lex_tables::is_terminal_state(new_state)) {
      goto dispatch_to_handler;
    } else {
      goto transition;
    }
#endif
  }

#if !QLJS_LEX_HANDLER_USE_COMPUTED_GOTO
  dispatch_to_handler : {
    switch (new_state >> state_data_bits) {
    case handler_transition:
      goto transition;
    case handler_done_retract_for_symbol:
      goto done_retract_for_symbol;
    case handler_done_unique_terminal_symbol:
      goto done_unique_terminal_symbol;

    default:
      QLJS_UNREACHABLE();
      goto transition;
    }
  }
#endif

  done_unique_terminal_symbol : {
    l->last_token_.type =
        lex_tables::unique_terminal_symbol_tokens[new_state & state_data_mask];
    l->input_ = input;
    l->last_token_.end = input;
    return true;
  }

  done_retract_for_symbol : {
    QLJS_WARNING_PUSH
    // Clang thinks that old_state is uninitialized if we goto done_retract
    // before assigning to it. However, if we didn't assign to old_state, we
    // also asserted that new_state != lex_tables::state::retract, thus we
    // shouldn't reach here anyway.
    QLJS_WARNING_IGNORE_CLANG("-Wconditional-uninitialized")

    input -= 1;
    QLJS_ASSERT(old_state < input_state_count);
    QLJS_ASSERT((old_state >> state_data_bits) == handler_transition);
    l->last_token_.type = lex_tables::retract_for_symbol_tokens[old_state];
    l->input_ = input;
    l->last_token_.end = input;
    return true;

    QLJS_WARNING_POP
  }
  }
};
}

QLJS_WARNING_POP

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
