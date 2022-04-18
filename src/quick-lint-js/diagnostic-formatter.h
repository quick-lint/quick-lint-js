// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAGNOSTIC_FORMATTER_H
#define QUICK_LINT_JS_DIAGNOSTIC_FORMATTER_H

#include <cstddef>
#include <initializer_list>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/translation.h>
#include <quick-lint-js/unreachable.h>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
string8_view translated_headlinese_statement_kind(statement_kind) noexcept;
string8_view translated_singular_statement_kind(statement_kind) noexcept;

class diagnostic_formatter_base {
 public:
  static source_code_span get_argument_source_code_span(
      const diagnostic_message_info& message_info, const void* diagnostic,
      int arg_index) {
    auto [arg_data, arg_type] = get_arg(message_info, diagnostic, arg_index);
    switch (arg_type) {
    case diagnostic_arg_type::identifier:
      return reinterpret_cast<const identifier*>(arg_data)->span();

    case diagnostic_arg_type::source_code_span:
      return *reinterpret_cast<const source_code_span*>(arg_data);

    case diagnostic_arg_type::char8:
    case diagnostic_arg_type::statement_kind:
    case diagnostic_arg_type::string8_view:
    case diagnostic_arg_type::variable_kind:
      QLJS_UNREACHABLE();
    }
    QLJS_UNREACHABLE();
  }

  static string8_view expand_argument(
      const diagnostic_message_info& message_info, const void* diagnostic,
      int arg_index) {
    auto [arg_data, arg_type] = get_arg(message_info, diagnostic, arg_index);
    switch (arg_type) {
    case diagnostic_arg_type::char8:
      return string8_view(reinterpret_cast<const char8*>(arg_data), 1);

    case diagnostic_arg_type::identifier:
      return reinterpret_cast<const identifier*>(arg_data)
          ->span()
          .string_view();

    case diagnostic_arg_type::source_code_span:
      return reinterpret_cast<const source_code_span*>(arg_data)->string_view();

    case diagnostic_arg_type::string8_view:
      return *reinterpret_cast<const string8_view*>(arg_data);

    case diagnostic_arg_type::statement_kind:
    case diagnostic_arg_type::variable_kind:
      QLJS_UNREACHABLE();
    }
    QLJS_UNREACHABLE();
  }

  static string8_view expand_argument_headlinese(
      const diagnostic_message_info& message_info, const void* diagnostic,
      int arg_index) {
    auto [arg_data, arg_type] = get_arg(message_info, diagnostic, arg_index);
    switch (arg_type) {
    case diagnostic_arg_type::statement_kind:
      return translated_headlinese_statement_kind(
          *reinterpret_cast<const statement_kind*>(arg_data));

    case diagnostic_arg_type::char8:
    case diagnostic_arg_type::identifier:
    case diagnostic_arg_type::source_code_span:
    case diagnostic_arg_type::string8_view:
    case diagnostic_arg_type::variable_kind:
      QLJS_UNREACHABLE();
    }
    QLJS_UNREACHABLE();
  }

  static string8_view expand_argument_singular(
      const diagnostic_message_info& message_info, const void* diagnostic,
      int arg_index) {
    auto [arg_data, arg_type] = get_arg(message_info, diagnostic, arg_index);
    switch (arg_type) {
    case diagnostic_arg_type::statement_kind:
      return translated_singular_statement_kind(
          *reinterpret_cast<const statement_kind*>(arg_data));

    case diagnostic_arg_type::char8:
    case diagnostic_arg_type::identifier:
    case diagnostic_arg_type::source_code_span:
    case diagnostic_arg_type::string8_view:
    case diagnostic_arg_type::variable_kind:
      QLJS_UNREACHABLE();
    }
    QLJS_UNREACHABLE();
  }

 private:
  static std::pair<const void*, diagnostic_arg_type> get_arg(
      const diagnostic_message_info& message_info, const void* diagnostic,
      int arg_index) noexcept {
    const diagnostic_message_arg_info& arg_info = message_info.args[arg_index];
    const void* arg_data =
        reinterpret_cast<const char*>(diagnostic) + arg_info.offset;
    return std::make_pair(arg_data, arg_info.type);
  }
};

template <class Derived>
class diagnostic_formatter : private diagnostic_formatter_base {
 public:
  // Assumed member functions in Derived:
  // void write_before_message(std::string_view code, diagnostic_severity, const
  // source_code_span &origin); void write_message_part(std::string_view code,
  // diagnostic_severity, string8_view); void
  // write_after_message(std::string_view code, diagnostic_severity, const
  // source_code_span &origin);

  void format(const diagnostic_info& info, const void* diagnostic);

  void format_message(std::string_view code, diagnostic_severity severity,
                      const diagnostic_message_info& info,
                      const void* diagnostic);
};

template <class Derived>
inline void diagnostic_formatter<Derived>::format(const diagnostic_info& info,
                                                  const void* diagnostic) {
  this->format_message(info.code, info.severity, info.messages[0], diagnostic);
  if (info.messages[1].format.valid()) {
    this->format_message(info.code, diagnostic_severity::note, info.messages[1],
                         diagnostic);
  }
}

template <class Derived>
inline void diagnostic_formatter<Derived>::format_message(
    std::string_view code, diagnostic_severity severity,
    const diagnostic_message_info& info, const void* diagnostic) {
  static constexpr auto npos = string8_view::npos;
  using string8_pos = string8_view::size_type;

  Derived* self = static_cast<Derived*>(this);

  source_code_span origin_span =
      get_argument_source_code_span(info, diagnostic, 0);
  self->write_before_message(code, severity, origin_span);

  string8_view remaining_message(translate(info.format));
  string8_pos left_curly_index;
  while ((left_curly_index = remaining_message.find(u8'{')) != npos) {
    QLJS_ASSERT(left_curly_index != remaining_message.size() &&
                "invalid message format: { at end of string has no matching }");

    if (remaining_message[left_curly_index + 1] == '{') {
      // "{{"; the '{' is escaped.
      self->write_message_part(
          code, severity, remaining_message.substr(0, left_curly_index + 1));
      remaining_message = remaining_message.substr(left_curly_index + 2);
      continue;
    }

    self->write_message_part(code, severity,
                             remaining_message.substr(0, left_curly_index));

    string8_pos right_curly_index =
        remaining_message.find(u8'}', left_curly_index + 1);
    QLJS_ASSERT(right_curly_index != npos &&
                "invalid message format: missing }");
    string8_view curly_content = remaining_message.substr(
        left_curly_index + 1, right_curly_index - (left_curly_index + 1));

    string8_view expanded_parameter;
    if (curly_content == u8"0") {
      expanded_parameter = this->expand_argument(info, diagnostic, 0);
    } else if (curly_content == u8"1") {
      expanded_parameter = this->expand_argument(info, diagnostic, 1);
    } else if (curly_content == u8"1:headlinese") {
      expanded_parameter =
          this->expand_argument_headlinese(info, diagnostic, 1);
    } else if (curly_content == u8"1:singular") {
      expanded_parameter = this->expand_argument_singular(info, diagnostic, 1);
    } else if (curly_content == u8"2") {
      expanded_parameter = this->expand_argument(info, diagnostic, 2);
    } else {
      QLJS_ASSERT(false && "invalid message format: unrecognized placeholder");
      QLJS_UNREACHABLE();
    }

    self->write_message_part(code, severity, expanded_parameter);
    remaining_message = remaining_message.substr(right_curly_index + 1);
  }
  self->write_message_part(code, severity, remaining_message);

  self->write_after_message(code, severity, origin_span);
}
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
