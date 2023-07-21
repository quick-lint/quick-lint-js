// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAG_DIAGNOSTIC_H
#define QUICK_LINT_JS_DIAG_DIAGNOSTIC_H

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/i18n/translation.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/warning.h>
#include <string_view>

// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=105191
#if defined(__GNUC__) && !defined(__clang__)
#define QLJS_WORK_AROUND_GCC_BUG_105191 = {}
#else
#define QLJS_WORK_AROUND_GCC_BUG_105191
#endif

namespace quick_lint_js {
class Source_Code_Span;
enum class Diag_Type;
enum class Enum_Kind;
enum class Statement_Kind;

enum class Diagnostic_Severity : std::uint8_t {
  error,
  note,
  warning,
};

enum class Diagnostic_Arg_Type : std::uint8_t {
  invalid = 0,

  char8,
  enum_kind,
  source_code_span,
  statement_kind,
  string8_view,
  variable_kind,
};

// If we support more than two infos (i.e. more than one note), the VS Code
// plugin needs to be updated. See NOTE(multiple notes).
constexpr int diagnostic_max_message_count = 2;

struct Diagnostic_Message_Arg_Info {
  // offset_shift is how many bits are removed in compact_offset.
  //
  // For example, if offset_shift is 3, then an arg must be 8-byte aligned.
  static constexpr int offset_shift = 1;
  static constexpr int offset_mask = (1 << offset_shift) - 1;
  static constexpr int offset_bits = 5;

  /*implicit*/ constexpr Diagnostic_Message_Arg_Info() noexcept
      : compact_offset(0), type(Diagnostic_Arg_Type::invalid) {}

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_CLANG("-Wimplicit-int-conversion")
  QLJS_WARNING_IGNORE_GCC("-Wconversion")
  /*implicit*/ constexpr Diagnostic_Message_Arg_Info(
      std::size_t offset, Diagnostic_Arg_Type type) noexcept
      : compact_offset(offset >> offset_shift), type(type) {
    // offset should be small.
    QLJS_CONSTEXPR_ASSERT((offset >> offset_shift) < (1 << offset_bits));
    // offset should be aligned.
    QLJS_CONSTEXPR_ASSERT((offset & offset_mask) == 0);
  }
  QLJS_WARNING_POP

  constexpr std::size_t offset() const noexcept {
    return static_cast<std::size_t>(this->compact_offset << offset_shift);
  }

  std::uint8_t compact_offset : offset_bits QLJS_WORK_AROUND_GCC_BUG_105191;
  Diagnostic_Arg_Type type : (8 - offset_bits) QLJS_WORK_AROUND_GCC_BUG_105191;
};

using Diagnostic_Message_Args = std::array<Diagnostic_Message_Arg_Info, 3>;

struct Diagnostic_Info {
  std::array<char, 5> code_string() const noexcept;

  std::uint16_t code : 14;
  Diagnostic_Severity severity : 2 QLJS_WORK_AROUND_GCC_BUG_105191;

  Translatable_Message message_formats[diagnostic_max_message_count];
  Diagnostic_Message_Args message_args[diagnostic_max_message_count];
};

// See NOTE[Diagnostic_Info_Debug].
struct Diagnostic_Info_Variable_Debug {
  const char *name = nullptr;
  Diagnostic_Arg_Type type;
  std::uint8_t offset;
};

// NOTE[Diagnostic_Info_Debug]: This class is for testing only. It is included
// in quick-lint-js proper.
struct Diagnostic_Info_Debug {
  Diagnostic_Info_Variable_Debug variables[4];
};

const Diagnostic_Info &get_diagnostic_info(Diag_Type) noexcept;
std::optional<Diag_Type> diag_type_from_code_slow(
    std::string_view code) noexcept;

// See NOTE[Diagnostic_Info_Debug].
const Diagnostic_Info_Debug &get_diagnostic_info_debug(Diag_Type) noexcept;

template <class Arg_Type>
constexpr Diagnostic_Arg_Type get_diagnostic_message_arg_type() noexcept;
template <>
constexpr Diagnostic_Arg_Type
get_diagnostic_message_arg_type<Char8>() noexcept {
  return Diagnostic_Arg_Type::char8;
}
template <>
constexpr Diagnostic_Arg_Type
get_diagnostic_message_arg_type<Enum_Kind>() noexcept {
  return Diagnostic_Arg_Type::enum_kind;
}
template <>
constexpr Diagnostic_Arg_Type
get_diagnostic_message_arg_type<Source_Code_Span>() noexcept {
  return Diagnostic_Arg_Type::source_code_span;
}
template <>
constexpr Diagnostic_Arg_Type
get_diagnostic_message_arg_type<Statement_Kind>() noexcept {
  return Diagnostic_Arg_Type::statement_kind;
}
template <>
constexpr Diagnostic_Arg_Type
get_diagnostic_message_arg_type<String8_View>() noexcept {
  return Diagnostic_Arg_Type::string8_view;
}
}

#undef QLJS_WORK_AROUND_GCC_BUG_105191

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
