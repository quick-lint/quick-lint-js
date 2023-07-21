// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAGNOSTIC_ASSERTION_H
#define QUICK_LINT_JS_DIAGNOSTIC_ASSERTION_H

#include <cstddef>
#include <memory>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/port/char8.h>
#include <string>
#include <vector>

namespace quick_lint_js {
// NOTE[_diag-syntax]: Diagnostic_Assertion objects are created from a
// specification string:
//
//   u8"^^^ Diag_Unexpected_Token"_diag
//
// A specification has four parts:
// * Alignment: Zero or more space characters which position the diagnostic
//   span.
// * Diagnostic span: One of the following:
//   * One or more '^' characters. Each '^' represents a code character that the
//     diagnostic covers.
//   * One '`' character. The '`' represents a diagnostic in between two code
//     characters. The '`' is positioned on the latter of the two code
//     characters.
// * Diagnostic type: A Diag_ class name.
// * (optional) Member variable: A field inside the Diag_ class. Written after
//   '.' after the diagnostic type. If the Diag_ class only has one member
//   variable, the member variable (including the '.') may be omitted.
//
// Diagnostic_Assertion objects are to be used with
// test_parse_and_visit_statement and related functions.
//
// Here are some examples uses:
//
// clang-format off
//
//   // Parse the code and assert that there is a
//   // Diag_Unexpected_Comma_After_Field_Initialization diagnostic. The
//   // diagnostic's .comma is asserted to be a Source_Code_Span covering one
//   // character: the ','. .comma starts at offset 15 and ends at offset 16.
//   test_parse_and_visit_statement(
//      u8"class C { a = 1, b = 2 }"_sv,  //
//      u8"               ^ Diag_Unexpected_Comma_After_Field_Initialization"_diag);
//
//   // Parse the code and assert that there is a
//   // Diag_Missing_Function_Parameter_List diagnostic. The
//   // diagnostic's .expected_parameter_list is asserted to be a
//   // Source_Code_Span covering zero characters. .expected_parameter_list
//   // starts at offset 16 and ends at offset 16.
//   test_parse_and_visit_statement(
//      u8"class C { method { body; } }"_sv,  //
//      u8"                ` Diag_Missing_Function_Parameter_List"_diag);
//
//    // Parse the code and assert that there are two separate diagnostics.
//    test_parse_and_visit_statement(
//      u8"class C { if method(arg) { body; } instanceof myField; }"_sv,              //
//      u8"                                   ^^^^^^^^^^ Diag_Unexpected_Token"_diag, //
//      u8"          ^^ Diag_Unexpected_Token"_diag);
//
// clang-format on
struct Diagnostic_Assertion {
  Diag_Type type = Diag_Type();
  const char* member_name = nullptr;
  std::uint8_t member_offset = 0;
  Diagnostic_Arg_Type member_type = Diagnostic_Arg_Type::invalid;
  Padded_String_Size span_begin_offset = static_cast<Padded_String_Size>(-1);
  Padded_String_Size span_end_offset = static_cast<Padded_String_Size>(-1);

  // If the specification is malformed, return a list of messages to report to
  // the user.
  static Result<Diagnostic_Assertion, std::vector<std::string>> parse(
      const Char8* specification);

  // If the specification is malformed, exit the program.
  static Diagnostic_Assertion parse_or_exit(const Char8* specification);
};

// See [_diag-syntax].
//
// Exits the program at run-time if the specification is malformed.
Diagnostic_Assertion operator""_diag(const Char8* specification,
                                     std::size_t specification_length);
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
