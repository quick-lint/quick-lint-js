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
// 1. Character classification tables (initial_character_class_table and
//    character_class_table). See NOTE[lex-table-class].
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
//     /*    */ initial_character_class_table[input[0]]
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
// * is_initial_state_terminal (which doesn't exist right now) can check if a
//   state is an initial terminal state (A) using a single >=.
struct lex_tables {
  // See NOTE[lex-table-class].
  enum character_class : std::uint8_t {
    bang,
    percent,
    ampersand,
    star,
    plus,
    minus,
    slash,
    less,
    equal,
    greater,
    circumflex,
    pipe,
    question,
    dot,
    digit,

    // Must be after normal character classes:
    other_character_class,

    character_class_count,

    // Character classes used only for the initial byte:
    ident = character_class_count,  // Initial identifier character.
    quote,                          // " or '
    tick,                           // `

    initial_character_class_count,
  };

  // Folds each character into a small set of equivalence classes. This makes
  // transition_table significantly smaller.
  //
  // initial_character_class_count is used only for the first input byte.
  // character_class_table is used for subsequent bytes.
  //
  // See NOTE[lex-table-class].
#define _ other_character_class
  // clang-format off
  static constexpr std::uint8_t initial_character_class_table[256] = {
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     bang,  quote, _,     ident, percent, ampersand, quote, _,     _,     star,  plus, _,     minus, dot,        slash,     // (sp) !"#$%&'()*+,-./
      digit, digit, digit, digit, digit, digit,   digit,     digit, digit, digit, _,     _,    less,  equal, greater,    question,  // 0123456789:;<=>?
      _,     ident, ident, ident, ident, ident,   ident,     ident, ident, ident, ident, ident,ident, ident, ident,      ident,     // @ABCDEFGHIJKLMNO
      ident, ident, ident, ident, ident, ident,   ident,     ident, ident, ident, ident, _,    ident, _,     circumflex, ident,     // PQRSTUVWXYZ[\]^_
      tick,  ident, ident, ident, ident, ident,   ident,     ident, ident, ident, ident, ident,ident, ident, ident,      ident,     // `abcdefghijklmno
      ident, ident, ident, ident, ident, ident,   ident,     ident, ident, ident, ident, _,    pipe,  _,     _,          _,         // pqrstuvwxyz{|}~ (del)
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
  };

  static constexpr std::uint8_t character_class_table[256] = {
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     bang,  _,     _,     _,     percent, ampersand, _,     _,     _,     star,  plus, _,     minus, dot,        slash,     // (sp) !"#$%&'()*+,-./
      digit, digit, digit, digit, digit, digit,   digit,     digit, digit, digit, _,     _,    less,  equal, greater,    question,  // 0123456789:;<=>?
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         // @ABCDEFGHIJKLMNO
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     circumflex, _,         // PQRSTUVWXYZ[\]^_
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         // `abcdefghijklmno
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    pipe,  _,     _,          _,         // pqrstuvwxyz{|}~ (del)
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
      _,     _,     _,     _,     _,     _,       _,         _,     _,     _,     _,     _,    _,     _,     _,          _,         //
  };
  // clang-format on
#undef _

  // clang-format off
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'!')] == character_class::bang);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'"')] == character_class::quote);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'$')] == character_class::ident);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'%')] == character_class::percent);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'&')] == character_class::ampersand);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'*')] == character_class::star);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'+')] == character_class::plus);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'-')] == character_class::minus);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'.')] == character_class::dot);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'/')] == character_class::slash);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'0')] == character_class::digit);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'1')] == character_class::digit);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'2')] == character_class::digit);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'3')] == character_class::digit);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'4')] == character_class::digit);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'5')] == character_class::digit);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'6')] == character_class::digit);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'7')] == character_class::digit);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'8')] == character_class::digit);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'9')] == character_class::digit);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'<')] == character_class::less);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'=')] == character_class::equal);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'>')] == character_class::greater);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'?')] == character_class::question);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'A')] == character_class::ident);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'Z')] == character_class::ident);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'\'')] == character_class::quote);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'\\')] == character_class::ident);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'^')] == character_class::circumflex);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'_')] == character_class::ident);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'`')] == character_class::tick);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'a')] == character_class::ident);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'z')] == character_class::ident);
  static_assert(initial_character_class_table[static_cast<std::uint8_t>(u8'|')] == character_class::pipe);

  static_assert(character_class_table[static_cast<std::uint8_t>(u8'!')] == character_class::bang);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'%')] == character_class::percent);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'&')] == character_class::ampersand);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'*')] == character_class::star);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'+')] == character_class::plus);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'-')] == character_class::minus);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'.')] == character_class::dot);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'/')] == character_class::slash);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'0')] == character_class::digit);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'1')] == character_class::digit);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'2')] == character_class::digit);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'3')] == character_class::digit);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'4')] == character_class::digit);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'5')] == character_class::digit);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'6')] == character_class::digit);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'7')] == character_class::digit);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'8')] == character_class::digit);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'9')] == character_class::digit);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'<')] == character_class::less);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'=')] == character_class::equal);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'>')] == character_class::greater);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'?')] == character_class::question);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'^')] == character_class::circumflex);
  static_assert(character_class_table[static_cast<std::uint8_t>(u8'|')] == character_class::pipe);
  // clang-format on

  using state_type = std::uint8_t;

  // How many bits in the state are reserved for the state number for
  // intermediate and non-unique terminal states, or for extra data for unique
  // terminal states and the retract terminal state.
  static constexpr int state_data_bits = 4;
  static constexpr state_type state_data_mask = (1 << state_data_bits) - 1;

  // How many bits in the state are reserved for selecting the handler.
  // See enum handler.
  static constexpr int state_handler_bits = 4;

  static_assert(sizeof(state_type) >=
                    (state_data_bits + state_handler_bits) / CHAR_BIT,
                "state_type should be big enough to fit all data bits and "
                "handler bits");

  static constexpr state_type get_state_data(state_type s) {
    return s & state_data_mask;
  }

  static constexpr state_type get_state_handler(state_type s) {
    return static_cast<state_type>(s >> state_data_bits);
  }

  enum handler {
    handler_transition_0 = 0,
    handler_transition_1 = 1,

    // The state data is how many characters to retract.
    handler_done_retract_for_symbol,

    // The state data is the index into unique_terminal_symbol_tokens_a.
    handler_done_unique_terminal_symbol_a,
    // The state data is the index into unique_terminal_symbol_tokens_b.
    handler_done_unique_terminal_symbol_b,

    handler_done_block_comment,
    handler_done_line_comment,
    handler_done_minus_minus,
    handler_done_number,

    // State data 0: "?.9" was parsed; return "?".
    // State data 1: "<!" was parsed; parse a "<!--" comment or return "<".
    // State data 2: ">>>" was parsed; parse either ">>>" or ">>>=".
    // State data 3: "**/" was parsed; parse either "**" or "*".
    // State data 4: "*/" was parsed; parse either "*" or report
    //               diag_unclosed_block_comment.
    handler_done_special_slow,

    handler_done_table_broken,

    handler_count,
  };
  static_assert(
      handler_transition_0 == 0,
      "handler_transition_* must be 0 to keep states in the transition "
      "table low-numbered");
  static_assert(
      handler_transition_1 == 1,
      "handler_transition_* must be 1 to keep states in the transition "
      "table low-numbered");
  static_assert(handler_count <= (1 << state_handler_bits),
                "state_handler_bits should be big enough to fit all handlers");

#define QLJS_STATE(handler, data) (((handler) << state_data_bits) | (data))
  enum state : state_type {
    // Initial states.
    // Handler must be handler_transition_* (0 or 1) for these states.
    // See enum character_class and NOTE[lex-table-initial].

    // Possibly-incomplete states.
    // Handler must be handler_transition_* (0 or 1) for these states.
    bang_equal = other_character_class,
    ampersand_ampersand,
    less_less,
    equal_equal,
    greater_greater,
    pipe_pipe,
    question_question,
    question_dot,
    dot_dot,
    star_star,

    input_state_count,

    // Complete/terminal states:
    // clang-format off
    done_percent_equal                 = QLJS_STATE(handler_done_unique_terminal_symbol_a, 0),
    done_ampersand_equal               = QLJS_STATE(handler_done_unique_terminal_symbol_a, 1),
    done_plus_plus                     = QLJS_STATE(handler_done_unique_terminal_symbol_a, 2),
    done_plus_equal                    = QLJS_STATE(handler_done_unique_terminal_symbol_a, 3),
    done_equal_greater                 = QLJS_STATE(handler_done_unique_terminal_symbol_a, 4),
    done_greater_equal                 = QLJS_STATE(handler_done_unique_terminal_symbol_a, 5),
    done_circumflex_equal              = QLJS_STATE(handler_done_unique_terminal_symbol_a, 6),
    done_pipe_equal                    = QLJS_STATE(handler_done_unique_terminal_symbol_a, 7),
    done_bang_equal_equal              = QLJS_STATE(handler_done_unique_terminal_symbol_a, 8),
    done_ampersand_ampersand_equal     = QLJS_STATE(handler_done_unique_terminal_symbol_a, 9),
    done_equal_equal_equal             = QLJS_STATE(handler_done_unique_terminal_symbol_a, 10),
    done_greater_greater_equal         = QLJS_STATE(handler_done_unique_terminal_symbol_a, 11),
    done_pipe_pipe_equal               = QLJS_STATE(handler_done_unique_terminal_symbol_a, 12),
    done_question_question_equal       = QLJS_STATE(handler_done_unique_terminal_symbol_a, 13),
    done_dot_dot_dot                   = QLJS_STATE(handler_done_unique_terminal_symbol_a, 14),
    done_less_equal                    = QLJS_STATE(handler_done_unique_terminal_symbol_a, 15),
    done_less_less_equal               = QLJS_STATE(handler_done_unique_terminal_symbol_b, 0),
    done_minus_equal                   = QLJS_STATE(handler_done_unique_terminal_symbol_b, 1),
    done_star_equal                    = QLJS_STATE(handler_done_unique_terminal_symbol_b, 2),
    done_star_star_equal               = QLJS_STATE(handler_done_unique_terminal_symbol_b, 3),
    done_slash_equal                   = QLJS_STATE(handler_done_unique_terminal_symbol_b, 4),
    // clang-format on

    done_greater_greater_greater = QLJS_STATE(handler_done_special_slow, 2),
    done_less_bang = QLJS_STATE(handler_done_special_slow, 1),
    done_minus_minus = QLJS_STATE(handler_done_minus_minus, 0),
    done_number = QLJS_STATE(handler_done_number, 0),
    done_question_dot_digit = QLJS_STATE(handler_done_special_slow, 0),
    done_star_slash = QLJS_STATE(handler_done_special_slow, 4),
    done_star_star_slash = QLJS_STATE(handler_done_special_slow, 3),

    done_block_comment = QLJS_STATE(handler_done_block_comment, 0),
    done_line_comment = QLJS_STATE(handler_done_line_comment, 0),

    // An unexpected character was detected. The lexer should retract the most
    // recent byte then consult retract_for_symbol_tokens using the previous
    // state.
    done_retract_for_symbol = QLJS_STATE(handler_done_retract_for_symbol, 1),
    done_retract_2_for_symbol = QLJS_STATE(handler_done_retract_for_symbol, 2),

    done_table_broken = QLJS_STATE(handler_done_table_broken, 0),
  };
#undef QLJS_STATE

  // Returns true if there are no transitions from this state to any other
  // state.
  static bool is_terminal_state(state_type s) {
    // Any state with a handler other than handler_transition is a terminal
    // state.
    static_assert(handler_transition_0 == 0 && handler_transition_1 == 1);
    bool is_terminal = s >= ((handler_transition_1 + 1) << state_data_bits);
    QLJS_ASSERT(is_terminal == (s >= input_state_count));
    QLJS_ASSERT(is_terminal == (get_state_handler(s) != handler_transition_0 &&
                                get_state_handler(s) != handler_transition_1));
    return is_terminal;
  }

  static constexpr state
      transition_table[character_class_count][input_state_count] = {
          // !
          {
              done_retract_for_symbol,    // !!               (invalid)
              done_retract_for_symbol,    // %!               (invalid)
              done_retract_for_symbol,    // &!               (invalid)
              done_retract_for_symbol,    // *!               (invalid)
              done_retract_for_symbol,    // +!               (invalid)
              done_retract_for_symbol,    // -!               (invalid)
              done_retract_for_symbol,    // /!               (invalid)
              done_less_bang,             // < -> <! (possibly <!--)
              done_retract_for_symbol,    // =!               (invalid)
              done_retract_for_symbol,    // >!               (invalid)
              done_retract_for_symbol,    // ^!               (invalid)
              done_retract_for_symbol,    // |!               (invalid)
              done_retract_for_symbol,    // ?!               (invalid)
              done_retract_for_symbol,    // .!               (invalid)
              done_table_broken,          // 9!               (invalid)
              done_retract_for_symbol,    // !=!              (invalid)
              done_retract_for_symbol,    // &&!              (invalid)
              done_retract_for_symbol,    // <<!              (invalid)
              done_retract_for_symbol,    // ==!              (invalid)
              done_retract_for_symbol,    // >>!              (invalid)
              done_retract_for_symbol,    // ||!              (invalid)
              done_retract_for_symbol,    // ??!              (invalid)
              done_retract_for_symbol,    // ?.!              (invalid)
              done_retract_2_for_symbol,  // ..!              (invalid)
              done_retract_for_symbol,    // **!              (invalid)
          },
          // %
          {
              done_retract_for_symbol,    // !%               (invalid)
              done_retract_for_symbol,    // %%               (invalid)
              done_retract_for_symbol,    // &%               (invalid)
              done_retract_for_symbol,    // *%               (invalid)
              done_retract_for_symbol,    // +%               (invalid)
              done_retract_for_symbol,    // -%               (invalid)
              done_retract_for_symbol,    // /%               (invalid)
              done_retract_for_symbol,    // <%               (invalid)
              done_retract_for_symbol,    // =%               (invalid)
              done_retract_for_symbol,    // >%               (invalid)
              done_retract_for_symbol,    // ^%               (invalid)
              done_retract_for_symbol,    // |%               (invalid)
              done_retract_for_symbol,    // ?%               (invalid)
              done_retract_for_symbol,    // .%               (invalid)
              done_table_broken,          // 9%               (invalid)
              done_retract_for_symbol,    // !=%              (invalid)
              done_retract_for_symbol,    // &&%              (invalid)
              done_retract_for_symbol,    // <<%              (invalid)
              done_retract_for_symbol,    // ==%              (invalid)
              done_retract_for_symbol,    // >>%              (invalid)
              done_retract_for_symbol,    // ||%              (invalid)
              done_retract_for_symbol,    // ??%              (invalid)
              done_retract_for_symbol,    // ?.%              (invalid)
              done_retract_2_for_symbol,  // ..%              (invalid)
              done_retract_for_symbol,    // **%              (invalid)
          },
          // &
          {
              done_retract_for_symbol,    // !&               (invalid)
              done_retract_for_symbol,    // %&               (invalid)
              ampersand_ampersand,        // & -> &&
              done_retract_for_symbol,    // *&               (invalid)
              done_retract_for_symbol,    // +&               (invalid)
              done_retract_for_symbol,    // -&               (invalid)
              done_retract_for_symbol,    // /&               (invalid)
              done_retract_for_symbol,    // <&               (invalid)
              done_retract_for_symbol,    // =&               (invalid)
              done_retract_for_symbol,    // >&               (invalid)
              done_retract_for_symbol,    // ^&               (invalid)
              done_retract_for_symbol,    // |&               (invalid)
              done_retract_for_symbol,    // ?&               (invalid)
              done_retract_for_symbol,    // .&               (invalid)
              done_table_broken,          // 9&               (invalid)
              done_retract_for_symbol,    // !=&              (invalid)
              done_retract_for_symbol,    // &&&              (invalid)
              done_retract_for_symbol,    // <<&              (invalid)
              done_retract_for_symbol,    // ==&              (invalid)
              done_retract_for_symbol,    // >>&              (invalid)
              done_retract_for_symbol,    // ||&              (invalid)
              done_retract_for_symbol,    // ??&              (invalid)
              done_retract_for_symbol,    // ?.&              (invalid)
              done_retract_2_for_symbol,  // ..&              (invalid)
              done_retract_for_symbol,    // **&              (invalid)
          },
          // *
          {
              done_retract_for_symbol,    // !*               (invalid)
              done_retract_for_symbol,    // %*               (invalid)
              done_retract_for_symbol,    // &*               (invalid)
              star_star,                  // * -> **
              done_retract_for_symbol,    // +*               (invalid)
              done_retract_for_symbol,    // -*               (invalid)
              done_block_comment,         // / -> /*
              done_retract_for_symbol,    // <*               (invalid)
              done_retract_for_symbol,    // =*               (invalid)
              done_retract_for_symbol,    // >*               (invalid)
              done_retract_for_symbol,    // ^*               (invalid)
              done_retract_for_symbol,    // |*               (invalid)
              done_retract_for_symbol,    // ?*               (invalid)
              done_retract_for_symbol,    // .*               (invalid)
              done_table_broken,          // 9*               (invalid)
              done_retract_for_symbol,    // !=*              (invalid)
              done_retract_for_symbol,    // &&*              (invalid)
              done_retract_for_symbol,    // <<*              (invalid)
              done_retract_for_symbol,    // ==*              (invalid)
              done_retract_for_symbol,    // >>*              (invalid)
              done_retract_for_symbol,    // ||*              (invalid)
              done_retract_for_symbol,    // ??*              (invalid)
              done_retract_for_symbol,    // ?.*              (invalid)
              done_retract_2_for_symbol,  // ..*              (invalid)
              done_retract_for_symbol,    // ***              (invalid)
          },
          // +
          {
              done_retract_for_symbol,    // !+               (invalid)
              done_retract_for_symbol,    // %+               (invalid)
              done_retract_for_symbol,    // &+               (invalid)
              done_retract_for_symbol,    // *+               (invalid)
              done_plus_plus,             // + -> ++
              done_retract_for_symbol,    // -+               (invalid)
              done_retract_for_symbol,    // /+               (invalid)
              done_retract_for_symbol,    // <+               (invalid)
              done_retract_for_symbol,    // =+               (invalid)
              done_retract_for_symbol,    // >+               (invalid)
              done_retract_for_symbol,    // ^+               (invalid)
              done_retract_for_symbol,    // |+               (invalid)
              done_retract_for_symbol,    // ?+               (invalid)
              done_retract_for_symbol,    // .+               (invalid)
              done_table_broken,          // 9+               (invalid)
              done_retract_for_symbol,    // !=+              (invalid)
              done_retract_for_symbol,    // &&+              (invalid)
              done_retract_for_symbol,    // <<+              (invalid)
              done_retract_for_symbol,    // ==+              (invalid)
              done_retract_for_symbol,    // >>+              (invalid)
              done_retract_for_symbol,    // ||+              (invalid)
              done_retract_for_symbol,    // ??+              (invalid)
              done_retract_for_symbol,    // ?.+              (invalid)
              done_retract_2_for_symbol,  // ..+              (invalid)
              done_retract_for_symbol,    // **+              (invalid)
          },
          // -
          {
              done_retract_for_symbol,    // !-               (invalid)
              done_retract_for_symbol,    // %-               (invalid)
              done_retract_for_symbol,    // &-               (invalid)
              done_retract_for_symbol,    // *-               (invalid)
              done_retract_for_symbol,    // +-               (invalid)
              done_minus_minus,           // - -> -- (potentially -->)
              done_retract_for_symbol,    // <-               (invalid)
              done_retract_for_symbol,    // /-               (invalid)
              done_retract_for_symbol,    // =-               (invalid)
              done_retract_for_symbol,    // >-               (invalid)
              done_retract_for_symbol,    // ^-               (invalid)
              done_retract_for_symbol,    // |-               (invalid)
              done_retract_for_symbol,    // ?-               (invalid)
              done_retract_for_symbol,    // .-               (invalid)
              done_table_broken,          // 9-               (invalid)
              done_retract_for_symbol,    // !=-              (invalid)
              done_retract_for_symbol,    // &&-              (invalid)
              done_retract_for_symbol,    // <<-              (invalid)
              done_retract_for_symbol,    // ==-              (invalid)
              done_retract_for_symbol,    // >>-              (invalid)
              done_retract_for_symbol,    // ||-              (invalid)
              done_retract_for_symbol,    // ??-              (invalid)
              done_retract_for_symbol,    // ?.-              (invalid)
              done_retract_2_for_symbol,  // ..-              (invalid)
              done_retract_for_symbol,    // **-              (invalid)
          },
          // /
          {
              done_retract_for_symbol,    // !/               (invalid)
              done_retract_for_symbol,    // %/               (invalid)
              done_retract_for_symbol,    // &/               (invalid)
              done_star_slash,            // * -> */ (possibly * or */)
              done_retract_for_symbol,    // +/               (invalid)
              done_retract_for_symbol,    // -/               (invalid)
              done_line_comment,          // / -> //
              done_retract_for_symbol,    // </               (invalid)
              done_retract_for_symbol,    // =/               (invalid)
              done_retract_for_symbol,    // >/               (invalid)
              done_retract_for_symbol,    // ^/               (invalid)
              done_retract_for_symbol,    // |/               (invalid)
              done_retract_for_symbol,    // ?/               (invalid)
              done_retract_for_symbol,    // ./               (invalid)
              done_table_broken,          // 9/               (invalid)
              done_retract_for_symbol,    // !=/              (invalid)
              done_retract_for_symbol,    // &&/              (invalid)
              done_retract_for_symbol,    // <</              (invalid)
              done_retract_for_symbol,    // ==/              (invalid)
              done_retract_for_symbol,    // >>/              (invalid)
              done_retract_for_symbol,    // ||/              (invalid)
              done_retract_for_symbol,    // ??/              (invalid)
              done_retract_for_symbol,    // ?./              (invalid)
              done_retract_2_for_symbol,  // ../              (invalid)
              done_star_star_slash,       // ** -> **/ (possibly * or **)
          },
          // <
          {
              done_retract_for_symbol,    // !<               (invalid)
              done_retract_for_symbol,    // %<               (invalid)
              done_retract_for_symbol,    // &<               (invalid)
              done_retract_for_symbol,    // *<               (invalid)
              done_retract_for_symbol,    // +<               (invalid)
              done_retract_for_symbol,    // -<               (invalid)
              done_retract_for_symbol,    // /<               (invalid)
              less_less,                  // < -> <<
              done_retract_for_symbol,    // =<               (invalid)
              done_retract_for_symbol,    // <>               (invalid)
              done_retract_for_symbol,    // ^<               (invalid)
              done_retract_for_symbol,    // |<               (invalid)
              done_retract_for_symbol,    // ?<               (invalid)
              done_retract_for_symbol,    // .<               (invalid)
              done_table_broken,          // 9<               (invalid)
              done_retract_for_symbol,    // !=<              (invalid)
              done_retract_for_symbol,    // &&<              (invalid)
              done_retract_for_symbol,    // <<<              (invalid)
              done_retract_for_symbol,    // ==<              (invalid)
              done_retract_for_symbol,    // >><              (invalid
              done_retract_for_symbol,    // ||<              (invalid)
              done_retract_for_symbol,    // ??<              (invalid)
              done_retract_for_symbol,    // ?.<              (invalid)
              done_retract_2_for_symbol,  // ..<              (invalid)
              done_retract_for_symbol,    // **<              (invalid)
          },
          // =
          {
              bang_equal,                      // ! -> !=
              done_percent_equal,              // % -> %=
              done_ampersand_equal,            // & -> &=
              done_star_equal,                 // * -> *=
              done_plus_equal,                 // + -> +=
              done_minus_equal,                // - -> -=
              done_slash_equal,                // / -> /=
              done_less_equal,                 // < -> <=
              equal_equal,                     // = -> ==
              done_greater_equal,              // > -> >=
              done_circumflex_equal,           // ^ -> ^=
              done_pipe_equal,                 // | -> |=
              done_retract_for_symbol,         // ?=           (invalid)
              done_retract_for_symbol,         // .=           (invalid)
              done_table_broken,               // 9=           (invalid)
              done_bang_equal_equal,           // != -> !==
              done_ampersand_ampersand_equal,  // && -> &&=
              done_less_less_equal,            // << -> <<=
              done_equal_equal_equal,          // == -> ===
              done_greater_greater_equal,      // >> -> >>=
              done_pipe_pipe_equal,            // || -> ||=
              done_question_question_equal,    // ?? -> ??=
              done_retract_for_symbol,         // ?.=          (invalid)
              done_retract_2_for_symbol,       // ..=          (invalid)
              done_star_star_equal,            // ** -> **=
          },
          // >
          {
              done_retract_for_symbol,       // !>               (invalid)
              done_retract_for_symbol,       // %>               (invalid)
              done_retract_for_symbol,       // &>               (invalid)
              done_retract_for_symbol,       // *>               (invalid)
              done_retract_for_symbol,       // +>               (invalid)
              done_retract_for_symbol,       // ->               (invalid)
              done_retract_for_symbol,       // />               (invalid)
              done_retract_for_symbol,       // <>               (invalid)
              done_equal_greater,            // = -> =>
              greater_greater,               // > -> >>
              done_retract_for_symbol,       // ^>               (invalid)
              done_retract_for_symbol,       // |>               (invalid)
              done_retract_for_symbol,       // ?>               (invalid)
              done_retract_for_symbol,       // .>               (invalid)
              done_table_broken,             // 9>               (invalid)
              done_retract_for_symbol,       // !=>              (invalid)
              done_retract_for_symbol,       // &&>              (invalid)
              done_retract_for_symbol,       // <<>              (invalid)
              done_retract_for_symbol,       // ==>              (invalid)
              done_greater_greater_greater,  // >> -> >>> (possibly >>>=)
              done_retract_for_symbol,       // ||>              (invalid)
              done_retract_for_symbol,       // ??>              (invalid)
              done_retract_for_symbol,       // ?.>              (invalid)
              done_retract_2_for_symbol,     // ..>              (invalid)
              done_retract_for_symbol,       // **>              (invalid)
          },
          // ^
          {
              done_retract_for_symbol,    // !^               (invalid)
              done_retract_for_symbol,    // %^               (invalid)
              done_retract_for_symbol,    // &^               (invalid)
              done_retract_for_symbol,    // *^               (invalid)
              done_retract_for_symbol,    // +^               (invalid)
              done_retract_for_symbol,    // -^               (invalid)
              done_retract_for_symbol,    // /^               (invalid)
              done_retract_for_symbol,    // <^               (invalid)
              done_retract_for_symbol,    // =^               (invalid)
              done_retract_for_symbol,    // >^               (invalid)
              done_retract_for_symbol,    // ^^               (invalid)
              done_retract_for_symbol,    // |^               (invalid)
              done_retract_for_symbol,    // ?^               (invalid)
              done_retract_for_symbol,    // .^               (invalid)
              done_table_broken,          // 9^               (invalid)
              done_retract_for_symbol,    // !=^              (invalid)
              done_retract_for_symbol,    // &&^              (invalid)
              done_retract_for_symbol,    // <<^              (invalid)
              done_retract_for_symbol,    // ==^              (invalid)
              done_retract_for_symbol,    // >>^              (invalid)
              done_retract_for_symbol,    // ||^              (invalid)
              done_retract_for_symbol,    // ??^              (invalid)
              done_retract_for_symbol,    // ?.^              (invalid)
              done_retract_2_for_symbol,  // ..^              (invalid)
              done_retract_for_symbol,    // **^              (invalid)
          },
          // |
          {
              done_retract_for_symbol,    // !|               (invalid)
              done_retract_for_symbol,    // %|               (invalid)
              done_retract_for_symbol,    // &|               (invalid)
              done_retract_for_symbol,    // *|               (invalid)
              done_retract_for_symbol,    // +|               (invalid)
              done_retract_for_symbol,    // -|               (invalid)
              done_retract_for_symbol,    // /|               (invalid)
              done_retract_for_symbol,    // <|               (invalid)
              done_retract_for_symbol,    // =|               (invalid)
              done_retract_for_symbol,    // >|               (invalid)
              done_retract_for_symbol,    // ^|               (invalid)
              pipe_pipe,                  // | -> ||
              done_retract_for_symbol,    // ?|               (invalid)
              done_retract_for_symbol,    // .|               (invalid)
              done_table_broken,          // 9|               (invalid)
              done_retract_for_symbol,    // !=|              (invalid)
              done_retract_for_symbol,    // &&|              (invalid)
              done_retract_for_symbol,    // <<|              (invalid)
              done_retract_for_symbol,    // ==|              (invalid)
              done_retract_for_symbol,    // >>|              (invalid)
              done_retract_for_symbol,    // |||              (invalid)
              done_retract_for_symbol,    // ??|              (invalid)
              done_retract_for_symbol,    // ?.|              (invalid)
              done_retract_2_for_symbol,  // ..|              (invalid)
              done_retract_for_symbol,    // **|              (invalid)
          },
          // ?
          {
              done_retract_for_symbol,    // !?               (invalid)
              done_retract_for_symbol,    // %?               (invalid)
              done_retract_for_symbol,    // &?               (invalid)
              done_retract_for_symbol,    // *?               (invalid)
              done_retract_for_symbol,    // +?               (invalid)
              done_retract_for_symbol,    // -?               (invalid)
              done_retract_for_symbol,    // /?               (invalid)
              done_retract_for_symbol,    // <?               (invalid)
              done_retract_for_symbol,    // =?               (invalid)
              done_retract_for_symbol,    // >?               (invalid)
              done_retract_for_symbol,    // ^?               (invalid)
              done_retract_for_symbol,    // |?               (invalid)
              question_question,          // ? -> ??
              done_retract_for_symbol,    // .?               (invalid)
              done_table_broken,          // 9?               (invalid)
              done_retract_for_symbol,    // !=?              (invalid)
              done_retract_for_symbol,    // &&?              (invalid)
              done_retract_for_symbol,    // <<?              (invalid)
              done_retract_for_symbol,    // ==?              (invalid)
              done_retract_for_symbol,    // >>?              (invalid)
              done_retract_for_symbol,    // ||?              (invalid)
              done_retract_for_symbol,    // ???              (invalid)
              done_retract_for_symbol,    // ?.?              (invalid)
              done_retract_2_for_symbol,  // ..?              (invalid)
              done_retract_for_symbol,    // **?              (invalid)
          },
          // .
          {
              done_retract_for_symbol,  // !.               (invalid)
              done_retract_for_symbol,  // %.               (invalid)
              done_retract_for_symbol,  // &.               (invalid)
              done_retract_for_symbol,  // *.               (invalid)
              done_retract_for_symbol,  // +.               (invalid)
              done_retract_for_symbol,  // -.               (invalid)
              done_retract_for_symbol,  // /.               (invalid)
              done_retract_for_symbol,  // <.               (invalid)
              done_retract_for_symbol,  // =.               (invalid)
              done_retract_for_symbol,  // >.               (invalid)
              done_retract_for_symbol,  // ^.               (invalid)
              done_retract_for_symbol,  // |.               (invalid)
              question_dot,             // ? -> ?.
              dot_dot,                  // . -> ..
              done_table_broken,        // 9.               (invalid)
              done_retract_for_symbol,  // !=.              (invalid)
              done_retract_for_symbol,  // &&.              (invalid)
              done_retract_for_symbol,  // <<.              (invalid)
              done_retract_for_symbol,  // ==.              (invalid)
              done_retract_for_symbol,  // >>.              (invalid)
              done_retract_for_symbol,  // ||.              (invalid)
              done_retract_for_symbol,  // ??.              (invalid)
              done_retract_for_symbol,  // ?..              (invalid)
              done_dot_dot_dot,         // .. -> ...
              done_retract_for_symbol,  // **.              (invalid)
          },
          // [0-9]
          {
              done_retract_for_symbol,    // !9         (invalid)
              done_retract_for_symbol,    // %9         (invalid)
              done_retract_for_symbol,    // &9         (invalid)
              done_retract_for_symbol,    // *9         (invalid)
              done_retract_for_symbol,    // +9         (invalid)
              done_retract_for_symbol,    // -9         (invalid)
              done_retract_for_symbol,    // /9         (invalid)
              done_retract_for_symbol,    // <9         (invalid)
              done_retract_for_symbol,    // =9         (invalid)
              done_retract_for_symbol,    // >9         (invalid)
              done_retract_for_symbol,    // ^9         (invalid)
              done_retract_for_symbol,    // |9         (invalid)
              done_retract_for_symbol,    // ?9         (invalid)
              done_number,                // . -> .9
              done_table_broken,          // 99         (invalid)
              done_retract_for_symbol,    // !=9        (invalid)
              done_retract_for_symbol,    // &&9        (invalid)
              done_retract_for_symbol,    // <<9        (invalid)
              done_retract_for_symbol,    // ==9        (invalid)
              done_retract_for_symbol,    // >>9        (invalid)
              done_retract_for_symbol,    // ||9        (invalid)
              done_retract_for_symbol,    // ??9        (invalid)
              done_question_dot_digit,    // ?.9        (invalid; remove '.9')
              done_retract_2_for_symbol,  // ..9        (invalid)
              done_retract_for_symbol,    // **9        (invalid)
          },
          // (other)
          {
              done_retract_for_symbol,    // !(other)         (invalid)
              done_retract_for_symbol,    // %(other)         (invalid)
              done_retract_for_symbol,    // &(other)         (invalid)
              done_retract_for_symbol,    // *(other)         (invalid)
              done_retract_for_symbol,    // +(other)         (invalid)
              done_retract_for_symbol,    // -(other)         (invalid)
              done_retract_for_symbol,    // /(other)         (invalid)
              done_retract_for_symbol,    // <(other)         (invalid)
              done_retract_for_symbol,    // =(other)         (invalid)
              done_retract_for_symbol,    // >(other)         (invalid)
              done_retract_for_symbol,    // ^(other)         (invalid)
              done_retract_for_symbol,    // |(other)         (invalid)
              done_retract_for_symbol,    // ?(other)         (invalid)
              done_retract_for_symbol,    // .(other)         (invalid)
              done_retract_for_symbol,    // 9(other)         (invalid)
              done_retract_for_symbol,    // !=(other)        (invalid)
              done_retract_for_symbol,    // &&(other)        (invalid)
              done_retract_for_symbol,    // <<(other)        (invalid)
              done_retract_for_symbol,    // ==(other)        (invalid)
              done_retract_for_symbol,    // >>(other)        (invalid)
              done_retract_for_symbol,    // ||(other)        (invalid)
              done_retract_for_symbol,    // ??(other)        (invalid)
              done_retract_for_symbol,    // ?.(other)        (invalid)
              done_retract_2_for_symbol,  // ..(other)        (invalid)
              done_retract_for_symbol,    // **(other)        (invalid)
          },
  };

  // The maximum number of input bytes which can be processed by the lexer
  // tables before terminating.
  static constexpr int maximum_state_depth = 3;

  static constexpr token_type invalid_token_type = token_type::identifier;

  // See NOTE[lex-table-token-type].
  static constexpr token_type unique_terminal_symbol_tokens_a[] = {
      token_type::percent_equal,              // %=
      token_type::ampersand_equal,            // &=
      token_type::plus_plus,                  // ++
      token_type::plus_equal,                 // +=
      token_type::equal_greater,              // =>
      token_type::greater_equal,              // >=
      token_type::circumflex_equal,           // ^=
      token_type::pipe_equal,                 // |=
      token_type::bang_equal_equal,           // !==
      token_type::ampersand_ampersand_equal,  // &&=
      token_type::equal_equal_equal,          // ===
      token_type::greater_greater_equal,      // >>=
      token_type::pipe_pipe_equal,            // ||=
      token_type::question_question_equal,    // ??=
      token_type::dot_dot_dot,                // ...
      token_type::less_equal,                 // <=
  };
  static constexpr token_type unique_terminal_symbol_tokens_b[] = {
      token_type::less_less_equal,  // <<=
      token_type::minus_equal,      // -=
      token_type::star_equal,       // *=
      token_type::star_star_equal,  // **=
      token_type::slash_equal,      // /=
  };

  // Key: a state < input_state_count
  // Value: corresponding token_type
  static constexpr token_type retract_for_symbol_tokens[] = {
      token_type::bang,                 // !
      token_type::percent,              // %
      token_type::ampersand,            // &
      token_type::star,                 // *
      token_type::plus,                 // +
      token_type::minus,                // -
      token_type::slash,                // /
      token_type::less,                 // <
      token_type::equal,                // =
      token_type::greater,              // >
      token_type::circumflex,           // ^
      token_type::pipe,                 // |
      token_type::question,             // ?
      token_type::dot,                  // .
      invalid_token_type,               // 9    (invalid)
      token_type::bang_equal,           // !=
      token_type::ampersand_ampersand,  // &&
      token_type::less_less,            // <<
      token_type::equal_equal,          // ==
      token_type::greater_greater,      // >>
      token_type::pipe_pipe,            // ||
      token_type::question_question,    // ??
      token_type::question_dot,         // ?.
      token_type::dot,        // ..(misc) -> . ( done_retract_2_for_symbol)
      token_type::star_star,  // **
  };

  // NOTE[lex-table-lookup]:
  static bool try_parse_current_token(lexer* l) {
    const char8* input = l->input_;

    lex_tables::state old_state;

#if QLJS_LEX_HANDLER_USE_COMPUTED_GOTO
    static void* initial_handler_table[] = {
        /*[bang] = */ &&transition,
        /*[percent] = */ &&transition,
        /*[ampersand] = */ &&transition,
        /*[star] = */ &&transition,
        /*[plus] = */ &&transition,
        /*[minus] = */ &&transition,
        /*[slash] = */ &&transition,
        /*[less] = */ &&transition,
        /*[equal] = */ &&transition,
        /*[greater] = */ &&transition,
        /*[circumflex] = */ &&transition,
        /*[pipe] = */ &&transition,
        /*[question] = */ &&transition,
        /*[dot] = */ &&transition,
        /*[digit] = */ &&done_number,
        /*[other_character_class] = */ &&done_table_broken,
        /*[ident] = */ &&done_identifier,
        /*[quote] = */ &&done_string_literal,
        /*[tick] = */ &&done_template_literal,
    };

    static void* handler_table[] = {
        /*[handler_transition_0] = */ &&transition,
        /*[handler_transition_1] = */ &&transition,
        /*[handler_done_retract_for_symbol] = */ &&done_retract_for_symbol,
        /*[handler_done_unique_terminal_symbol_a] = */
        &&done_unique_terminal_symbol_a,
        /*[handler_done_unique_terminal_symbol_b] = */
        &&done_unique_terminal_symbol_b,
        /*[handler_done_block_comment] = */ &&done_block_comment,
        /*[handler_done_line_comment] = */ &&done_line_comment,
        /*[handler_done_minus_minus] = */ &&done_minus_minus,
        /*[handler_done_number] = */ &&done_number,
        /*[handler_done_special_slow] = */ &&done_special_slow,
        /*[handler_done_table_broken] = */ &&done_table_broken,
    };
#endif

    // The first lookup is special. In normal DFA tables, there is on initial
    // state. In our table, there are many initial states. The character class
    // of the first character corresponds to the initial state. Therefore, for
    // the first character, do not use lex_tables::transition_table. See
    // NOTE[lex-table-initial].
    std::uint8_t initial_character_class =
        lex_tables::initial_character_class_table[static_cast<std::uint8_t>(
            *input)];
    lex_tables::state new_state =
        static_cast<lex_tables::state>(initial_character_class);
    QLJS_ASSERT(get_state_handler(new_state) !=
                handler_done_retract_for_symbol);
    input += 1;
#if QLJS_LEX_HANDLER_USE_COMPUTED_GOTO
    QLJS_ASSERT(new_state < std::size(initial_handler_table));
    goto* initial_handler_table[initial_character_class];
#else
    switch (initial_character_class) {
    case character_class::ampersand:
    case character_class::bang:
    case character_class::circumflex:
    case character_class::dot:
    case character_class::equal:
    case character_class::greater:
    case character_class::less:
    case character_class::minus:
    case character_class::percent:
    case character_class::pipe:
    case character_class::plus:
    case character_class::question:
    case character_class::slash:
    case character_class::star:
      goto transition;

    case character_class::digit:
      goto done_number;

    case character_class::ident:
      goto done_identifier;

    case character_class::quote:
      goto done_string_literal;

    case character_class::tick:
      goto done_template_literal;

    case character_class::other_character_class:
    default:
      goto done_table_broken;
    }
#endif

  transition : {
    old_state = new_state;
    const lex_tables::state* transitions = lex_tables::transition_table
        [lex_tables::character_class_table[static_cast<std::uint8_t>(*input)]];
    new_state = transitions[new_state];
    input += 1;
#if QLJS_LEX_HANDLER_USE_COMPUTED_GOTO
    goto* handler_table[get_state_handler(new_state)];
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
    case handler_transition_0:
    case handler_transition_1:
      goto transition;
    case handler_done_retract_for_symbol:
      goto done_retract_for_symbol;
    case handler_done_unique_terminal_symbol_a:
      goto done_unique_terminal_symbol_a;
    case handler_done_unique_terminal_symbol_b:
      goto done_unique_terminal_symbol_b;
    case handler_done_block_comment:
      goto done_block_comment;
    case handler_done_line_comment:
      goto done_line_comment;
    case handler_done_minus_minus:
      goto done_minus_minus;
    case handler_done_number:
      goto done_number;
    case handler_done_special_slow:
      goto done_special_slow;
    case handler_done_table_broken:
      goto done_table_broken;

    default:
      QLJS_UNREACHABLE();
      goto transition;
    }
  }
#endif

  done_unique_terminal_symbol_a : {
    l->last_token_.type =
        lex_tables::unique_terminal_symbol_tokens_a[get_state_data(new_state)];
    QLJS_ASSERT(l->last_token_.type != invalid_token_type);
    l->input_ = input;
    l->last_token_.end = l->input_;
    return true;
  }

  done_unique_terminal_symbol_b : {
    l->last_token_.type =
        lex_tables::unique_terminal_symbol_tokens_b[get_state_data(new_state)];
    QLJS_ASSERT(l->last_token_.type != invalid_token_type);
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

    QLJS_ASSERT(get_state_data(new_state) > 0);
    input -= get_state_data(new_state);
    QLJS_ASSERT(old_state < input_state_count);
    QLJS_ASSERT(get_state_handler(old_state) == handler_transition_0 ||
                get_state_handler(old_state) == handler_transition_1);
    l->last_token_.type = lex_tables::retract_for_symbol_tokens[old_state];
    QLJS_ASSERT(l->last_token_.type != invalid_token_type);
    l->input_ = input;
    l->last_token_.end = input;
    return true;

    QLJS_WARNING_POP
  }

  // /*
  done_block_comment : {
    QLJS_ASSERT(l->input_[0] == u8'/');
    QLJS_ASSERT(l->input_[1] == u8'*');
    l->skip_block_comment();
    return false;
  }

  // //
  done_line_comment : {
    QLJS_ASSERT(l->input_[0] == u8'/');
    QLJS_ASSERT(l->input_[1] == u8'/');
    l->input_ += 2;
    l->skip_line_comment_body();
    return false;
  }

  // -->
  // --
  done_minus_minus : {
    QLJS_ASSERT(l->input_[0] == u8'-');
    QLJS_ASSERT(l->input_[1] == u8'-');
    if (l->input_[2] == u8'>' && l->is_first_token_on_line()) {
      // -->
      l->input_ += 3;
      l->skip_line_comment_body();
      return false;
    } else {
      // --(other)
      l->input_ = input;
      l->last_token_.type = token_type::minus_minus;
      l->last_token_.end = l->input_;
      return true;
    }
  }

  done_identifier : {
    lexer::parsed_identifier ident =
        l->parse_identifier(l->input_, lexer::identifier_kind::javascript);
    l->input_ = ident.after;
    l->last_token_.normalized_identifier = ident.normalized;
    l->last_token_.end = ident.after;
    l->last_token_.type = l->identifier_token_type(ident.normalized);
    if (ident.escape_sequences && !ident.escape_sequences->empty()) {
      switch (l->last_token_.type) {
      case token_type::identifier:
        l->last_token_.type = token_type::identifier;
        break;

      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case token_type::kw_await:
      case token_type::kw_yield:
        // Escape sequences in identifiers prevent it from becoming a
        // contextual keyword.
        l->last_token_.type = token_type::identifier;
        break;

      QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
        // TODO(#73): Treat 'protected', 'implements', etc. in strict mode as
        // reserved words.
        l->last_token_.type = token_type::identifier;
        break;

      QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD:
        // Escape sequences in identifiers prevent it from becoming a reserved
        // keyword.
        l->last_token_.type = token_type::reserved_keyword_with_escape_sequence;
        l->last_token_.identifier_escape_sequences = ident.escape_sequences;
        break;

      default:
        QLJS_UNREACHABLE();
      }
    }
    return true;
  }

  // "
  // '
  done_string_literal : {
    l->input_ = l->parse_string_literal();
    l->last_token_.type = token_type::string;
    l->last_token_.end = l->input_;
    return true;
  }

  // `
  done_template_literal : {
    l->input_ += 1;
    lexer::parsed_template_body body = l->parse_template_body(
        l->input_, l->last_token_.begin, l->diag_reporter_);
    l->last_token_.template_escape_sequence_diagnostics =
        body.escape_sequence_diagnostics;
    l->last_token_.type = body.type;
    l->input_ = body.end;
    l->last_token_.end = l->input_;
    return true;
  }

  done_number : {
    // Ignore the 'input' variable. l->parse_number() will re-parse.
    l->last_token_.type = token_type::number;
    if (l->input_[0] == '0') {
      switch (l->input_[1]) {
      case 'b':
      case 'B':
        l->input_ += 2;
        l->parse_binary_number();
        break;
      case 'o':
      case 'O':
        l->input_ += 2;
        l->parse_modern_octal_number();
        break;
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        l->input_ += 1;
        l->parse_legacy_octal_number();
        break;
      case 'x':
      case 'X':
        l->input_ += 2;
        l->parse_hexadecimal_number();
        break;
      default:
        l->parse_number();
        break;
      }
    } else {
      l->parse_number();
    }
    l->last_token_.end = l->input_;
    return true;
  }

  // ?.9
  done_special_slow : {
    switch (get_state_data(new_state)) {
    // "?.9" was parsed; return "?".
    case 0:
      QLJS_ASSERT(l->input_[0] == u8'?');
      QLJS_ASSERT(l->input_[1] == u8'.');
      QLJS_ASSERT(l->is_digit(l->input_[2]));
      l->last_token_.type = token_type::question;
      l->input_ += 1;
      l->last_token_.end = l->input_;
      return true;

    // "<!" was parsed; parse a "<!--" comment or return "<".
    case 1:
      QLJS_ASSERT(l->input_[0] == u8'<');
      QLJS_ASSERT(l->input_[1] == u8'!');
      if (l->input_[2] == u8'-' && l->input_[3] == u8'-') {
        // <!--
        l->input_ += 4;
        l->skip_line_comment_body();
        return false;
      } else {
        // <!(other)
        // Only parse the '<'.
        l->input_ += 1;
        l->last_token_.type = token_type::less;
        l->last_token_.end = l->input_;
        return true;
      }

    // ">>>" was parsed; parse either ">>>" or ">>>=".
    case 2:
      QLJS_ASSERT(l->input_[0] == u8'>');
      QLJS_ASSERT(l->input_[1] == u8'>');
      QLJS_ASSERT(l->input_[2] == u8'>');
      if (l->input_[3] == u8'=') {
        // >>>=
        l->input_ += 4;
        l->last_token_.type = token_type::greater_greater_greater_equal;
      } else {
        // >>>
        l->input_ += 3;
        l->last_token_.type = token_type::greater_greater_greater;
      }
      l->last_token_.end = l->input_;
      return true;

    // "**/" was parsed; parse either "**" or "*".
    case 3: {
      QLJS_ASSERT(l->input_[0] == u8'*');
      QLJS_ASSERT(l->input_[1] == u8'*');
      QLJS_ASSERT(l->input_[2] == u8'/');
      bool parsed_ok = l->test_for_regexp(&l->input_[2]);
      if (!parsed_ok) {
        // We saw '**/'. Emit a '*' token now. Later, we will interpret the
        // following '*/' as a comment.
        l->last_token_.type = token_type::star;
        l->input_ += 1;
      } else {
        l->last_token_.type = token_type::star_star;
        l->input_ += 2;
      }
      l->last_token_.end = l->input_;
      return true;
    }

    // "*/" was parsed; parse either "*" or report diag_unclosed_block_comment.
    case 4: {
      QLJS_ASSERT(l->input_[0] == u8'*');
      QLJS_ASSERT(l->input_[1] == u8'/');
      const char8* starpos = &l->input_[0];
      bool parsed_ok = l->test_for_regexp(&l->input_[1]);

      if (!parsed_ok) {
        l->diag_reporter_->report(diag_unopened_block_comment{
            source_code_span(starpos, &l->input_[2])});
        l->input_ += 2;
        l->skip_whitespace();
        return false;
      } else {
        l->last_token_.type = token_type::star;
        l->input_ += 1;
      }
      l->last_token_.end = l->input_;
      return true;
    }

    default:
      QLJS_UNREACHABLE();
      return false;
    }
  }

  done_table_broken : {
    QLJS_ALWAYS_ASSERT(false && "lexer table is broken");
    return false;
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
