// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/diagnostic-assertion.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/cpp.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <type_traits>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
// To reduce build bloat, a Diagnostic_Assertion should not have code generated
// to destroy it.
static_assert(std::is_trivially_destructible_v<Diagnostic_Assertion>);

namespace {
#define QLJS_DIAG_TYPE_NAME(name) \
  {QLJS_CPP_QUOTE_U8_SV(name), ::quick_lint_js::Diag_Type::name},
Hash_Map<String8_View, Diag_Type> diag_type_name_to_diag_type =
    Hash_Map<String8_View, Diag_Type>{QLJS_X_DIAG_TYPE_NAMES};
#undef QLJS_DIAG_TYPE_NAME

bool is_diag_type_char(Char8 c) {
  return (u8'a' <= c && c <= u8'z') ||  //
         (u8'A' <= c && c <= u8'Z') ||  //
         (u8'0' <= c && c <= u8'9') ||  //
         c == u8'_';
}
}

Result<Diagnostic_Assertion, std::vector<std::string>>
Diagnostic_Assertion::parse(const Char8* specification) {
  const Char8* p = specification;
  std::vector<std::string> errors;

  Diagnostic_Assertion out_assertion;

  Padded_String_Size leading_space_count = 0;
  for (; *p == u8' '; ++p) {
    leading_space_count += 1;
  }
  out_assertion.span_begin_offset = leading_space_count;

  Padded_String_Size caret_count = 0;
  if (*p == u8'`') {
    ++p;
  } else {
    for (; *p == u8'^'; ++p) {
      caret_count += 1;
    }
  }
  out_assertion.span_end_offset = leading_space_count + caret_count;

  for (; *p == u8' '; ++p) {
  }

  const Char8* diag_type_begin = p;
  for (; is_diag_type_char(*p); ++p) {
  }
  const Char8* diag_type_end = p;
  String8_View diag_type_span =
      make_string_view(diag_type_begin, diag_type_end);

  String8_View diag_member_span;
  if (*p == u8'.') {
    ++p;
    const Char8* diag_member_begin = p;
    for (; is_diag_type_char(*p); ++p) {
    }
    const Char8* diag_member_end = p;
    if (diag_member_begin == diag_member_end) {
      errors.push_back("expected member variable name after '.'");
    }
    diag_member_span = make_string_view(diag_member_begin, diag_member_end);
  }

  if (*p != u8'\0') {
    if (*p == ' ') {
      errors.push_back("trailing whitespace is not allowed in _diag");
    } else {
      errors.push_back(concat("unexpected '"sv,
                              to_string_view(String8_View(p, 1)),
                              "' in _diag"sv));
    }
  }

  auto diag_type_it = diag_type_name_to_diag_type.find(diag_type_span);
  if (diag_type_it == diag_type_name_to_diag_type.end()) {
    if (errors.empty()) {
      errors.push_back(concat("invalid diagnostic type: '"sv,
                              to_string_view(diag_type_span), "'"sv));
    }
  } else {
    out_assertion.type = diag_type_it->second;
    const Diagnostic_Info_Debug& diag_info =
        get_diagnostic_info_debug(out_assertion.type);
    int member_count = 0;
    for (const Diagnostic_Info_Variable_Debug& var : diag_info.variables) {
      if (var.name != nullptr) {
        member_count += 1;
      }
    }
    bool member_is_required = member_count > 1;
    if (member_is_required && diag_member_span.empty()) {
      std::string members;
      for (const Diagnostic_Info_Variable_Debug& var : diag_info.variables) {
        if (var.name != nullptr &&
            var.type == Diagnostic_Arg_Type::source_code_span) {
          if (!members.empty()) {
            members += " or "sv;
          }
          members += '.';
          members += var.name;
        }
      }
      errors.push_back(concat("member required for "sv,
                              to_string_view(diag_type_span), "; try "sv,
                              members));
    }

    const Diagnostic_Info_Variable_Debug* member = nullptr;
    if (diag_member_span.empty()) {
      // Default to the first Source_Code_Span member.
      for (const Diagnostic_Info_Variable_Debug& var : diag_info.variables) {
        if (var.name != nullptr &&
            var.type == Diagnostic_Arg_Type::source_code_span) {
          member = &var;
        }
      }
    } else {
      for (const Diagnostic_Info_Variable_Debug& var : diag_info.variables) {
        if (var.name != nullptr &&
            var.name == to_string_view(diag_member_span)) {
          member = &var;
        }
      }
    }
    QLJS_ALWAYS_ASSERT(member != nullptr);

    out_assertion.member_name = member->name;
    out_assertion.member_offset = member->offset;
    out_assertion.member_type = member->type;
  }

  if (!errors.empty()) {
    return failed_result(std::move(errors));
  }
  return out_assertion;
}

Diagnostic_Assertion Diagnostic_Assertion::parse_or_exit(
    const Char8* specification) {
  Result<Diagnostic_Assertion, std::vector<std::string>> da =
      Diagnostic_Assertion::parse(specification);
  if (!da.ok()) {
    std::fprintf(stderr, "error while parsing \"%s\"_diag:\n",
                 reinterpret_cast<const char*>(specification));
    for (const std::string& s : da.error()) {
      std::fprintf(stderr, "%s\n", s.c_str());
    }
    std::exit(1);
  }
  return *da;
}

Diagnostic_Assertion operator""_diag(
    const Char8* specification,
    [[maybe_unused]] std::size_t specification_length) {
  return Diagnostic_Assertion::parse_or_exit(specification);
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
