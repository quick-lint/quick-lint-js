// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAGNOSTIC_H
#define QUICK_LINT_JS_DIAGNOSTIC_H

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/translation.h>
#include <quick-lint-js/warning.h>
#include <string_view>

// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=105191
#if defined(__GNUC__) && !defined(__clang__)
#define QLJS_WORK_AROUND_GCC_BUG_105191 = {}
#else
#define QLJS_WORK_AROUND_GCC_BUG_105191
#endif

namespace quick_lint_js {
enum class error_type;

enum class diagnostic_severity : std::uint8_t {
  error,
  note,
  warning,
};

enum class diagnostic_arg_type : std::uint8_t {
  invalid = 0,

  char8,
  identifier,
  source_code_span,
  statement_kind,
  string8_view,
  variable_kind,
};

// If we support more than two infos (i.e. more than one note), the VS Code
// plugin needs to be updated. See NOTE(multiple notes).
constexpr int diagnostic_max_message_count = 2;

struct diagnostic_message_arg_info {
  // offset_shift is how many bits are removed in compact_offset.
  //
  // For example, if offset_shift is 3, then an arg must be 8-byte aligned.
  static constexpr int offset_shift = 1;
  static constexpr int offset_mask = (1 << offset_shift) - 1;
  static constexpr int offset_bits = 5;

  /*implicit*/ constexpr diagnostic_message_arg_info() noexcept
      : compact_offset(0), type(diagnostic_arg_type::invalid) {}

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_CLANG("-Wimplicit-int-conversion")
  QLJS_WARNING_IGNORE_GCC("-Wconversion")
  /*implicit*/ constexpr diagnostic_message_arg_info(
      std::size_t offset, diagnostic_arg_type type) noexcept
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
  diagnostic_arg_type type : (8 - offset_bits) QLJS_WORK_AROUND_GCC_BUG_105191;
};

struct diagnostic_message_info {
  translatable_message format;
  diagnostic_message_arg_info args[3];
};

struct diagnostic_info {
  std::array<char, 5> code_string() const noexcept;

  std::uint16_t code : 14;
  diagnostic_severity severity : 2 QLJS_WORK_AROUND_GCC_BUG_105191;
  diagnostic_message_info messages[diagnostic_max_message_count];
};

const diagnostic_info &get_diagnostic_info(error_type) noexcept;
std::optional<error_type> error_type_from_code_slow(
    std::string_view code) noexcept;
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
