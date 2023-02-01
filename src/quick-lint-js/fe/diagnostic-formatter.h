// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_DIAGNOSTIC_FORMATTER_H
#define QUICK_LINT_JS_FE_DIAGNOSTIC_FORMATTER_H

#include <cstddef>
#include <initializer_list>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/fe/diagnostic.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/i18n/translation.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
string8_view headlinese_enum_kind(enum_kind) noexcept;
translatable_message headlinese_statement_kind(statement_kind) noexcept;
translatable_message singular_statement_kind(statement_kind) noexcept;

class diagnostic_formatter_base {
 public:
  explicit diagnostic_formatter_base(translator t);

  source_code_span get_argument_source_code_span(
      const diagnostic_message_args& args, const void* diagnostic,
      int arg_index);

  string8_view expand_argument(const diagnostic_message_args& args,
                               const void* diagnostic, int arg_index);

  string8_view expand_argument_headlinese(const diagnostic_message_args& args,
                                          const void* diagnostic,
                                          int arg_index);

  string8_view expand_argument_singular(const diagnostic_message_args& args,
                                        const void* diagnostic, int arg_index);

 protected:
  translator translator_;

 private:
  std::pair<const void*, diagnostic_arg_type> get_arg(
      const diagnostic_message_args& args, const void* diagnostic,
      int arg_index) noexcept;
};

template <class Derived>
class diagnostic_formatter : private diagnostic_formatter_base {
 public:
  using diagnostic_formatter_base::diagnostic_formatter_base;

  // Assumed member functions in Derived:
  //
  // void write_before_message(std::string_view code, diagnostic_severity,
  //                           const source_code_span &origin);
  // void write_message_part(std::string_view code,
  //                         diagnostic_severity, string8_view);
  // void write_after_message(std::string_view code, diagnostic_severity,
  //                          const source_code_span &origin);

  void format(const diagnostic_info& info, const void* diagnostic);

  void format_message(std::string_view code, diagnostic_severity severity,
                      translatable_message format,
                      const diagnostic_message_args& args,
                      const void* diagnostic);
};

template <class Derived>
inline void diagnostic_formatter<Derived>::format(const diagnostic_info& info,
                                                  const void* diagnostic) {
  auto code_string = info.code_string();
  std::string_view code_string_view(code_string.data(), code_string.size());

  this->format_message(code_string_view, info.severity, info.message_formats[0],
                       info.message_args[0], diagnostic);
  if (info.message_formats[1].valid()) {
    this->format_message(code_string_view, diagnostic_severity::note,
                         info.message_formats[1], info.message_args[1],
                         diagnostic);
  }
}

template <class Derived>
inline void diagnostic_formatter<Derived>::format_message(
    std::string_view code, diagnostic_severity severity,
    translatable_message message_format, const diagnostic_message_args& args,
    const void* diagnostic) {
  static constexpr auto npos = string8_view::npos;
  using string8_pos = string8_view::size_type;

  Derived* self = static_cast<Derived*>(this);

  source_code_span origin_span =
      get_argument_source_code_span(args, diagnostic, 0);
  self->write_before_message(code, severity, origin_span);

  string8_view remaining_message =
      string8_view::from_c_str(this->translator_.translate(message_format));
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
    if (curly_content == u8"0"_sv) {
      expanded_parameter = this->expand_argument(args, diagnostic, 0);
    } else if (curly_content == u8"1"_sv) {
      expanded_parameter = this->expand_argument(args, diagnostic, 1);
    } else if (curly_content == u8"1:headlinese"_sv) {
      expanded_parameter =
          this->expand_argument_headlinese(args, diagnostic, 1);
    } else if (curly_content == u8"1:singular"_sv) {
      expanded_parameter = this->expand_argument_singular(args, diagnostic, 1);
    } else if (curly_content == u8"2"_sv) {
      expanded_parameter = this->expand_argument(args, diagnostic, 2);
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
