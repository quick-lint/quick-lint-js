// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAG_DIAGNOSTIC_FORMATTER_H
#define QUICK_LINT_JS_DIAG_DIAGNOSTIC_FORMATTER_H

#include <cstddef>
#include <initializer_list>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/diag/diagnostic.h>
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
String8_View headlinese_enum_kind(Enum_Kind) noexcept;
Translatable_Message headlinese_statement_kind(Statement_Kind) noexcept;
Translatable_Message singular_statement_kind(Statement_Kind) noexcept;

class Diagnostic_Formatter_Base {
 public:
  explicit Diagnostic_Formatter_Base(Translator t);

  Source_Code_Span get_argument_source_code_span(
      const Diagnostic_Message_Args& args, const void* diagnostic,
      int arg_index);

  String8_View expand_argument(const Diagnostic_Message_Args& args,
                               const void* diagnostic, int arg_index);

  String8_View expand_argument_headlinese(const Diagnostic_Message_Args& args,
                                          const void* diagnostic,
                                          int arg_index);

  String8_View expand_argument_singular(const Diagnostic_Message_Args& args,
                                        const void* diagnostic, int arg_index);

 protected:
  Translator translator_;

 private:
  std::pair<const void*, Diagnostic_Arg_Type> get_arg(
      const Diagnostic_Message_Args& args, const void* diagnostic,
      int arg_index) noexcept;
};

template <class Derived>
class Diagnostic_Formatter : private Diagnostic_Formatter_Base {
 public:
  using Diagnostic_Formatter_Base::Diagnostic_Formatter_Base;

  // Assumed member functions in Derived:
  //
  // void write_before_message(std::string_view code, diagnostic_severity,
  //                           const Source_Code_Span &origin);
  // void write_message_part(std::string_view code,
  //                         diagnostic_severity, String8_View);
  // void write_after_message(std::string_view code, diagnostic_severity,
  //                          const Source_Code_Span &origin);

  void format(const Diagnostic_Info& info, const void* diagnostic);

  void format_message(std::string_view code, Diagnostic_Severity severity,
                      Translatable_Message format,
                      const Diagnostic_Message_Args& args,
                      const void* diagnostic);
};

template <class Derived>
inline void Diagnostic_Formatter<Derived>::format(const Diagnostic_Info& info,
                                                  const void* diagnostic) {
  auto code_string = info.code_string();
  std::string_view code_string_view(code_string.data(), code_string.size());

  this->format_message(code_string_view, info.severity, info.message_formats[0],
                       info.message_args[0], diagnostic);
  if (info.message_formats[1].valid()) {
    this->format_message(code_string_view, Diagnostic_Severity::note,
                         info.message_formats[1], info.message_args[1],
                         diagnostic);
  }
}

template <class Derived>
inline void Diagnostic_Formatter<Derived>::format_message(
    std::string_view code, Diagnostic_Severity severity,
    Translatable_Message message_format, const Diagnostic_Message_Args& args,
    const void* diagnostic) {
  static constexpr auto npos = String8_View::npos;
  using String8_Pos = String8_View::size_type;

  Derived* self = static_cast<Derived*>(this);

  Source_Code_Span origin_span =
      get_argument_source_code_span(args, diagnostic, 0);
  self->write_before_message(code, severity, origin_span);

  String8_View remaining_message(this->translator_.translate(message_format));
  String8_Pos left_curly_index;
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

    String8_Pos right_curly_index =
        remaining_message.find(u8'}', left_curly_index + 1);
    QLJS_ASSERT(right_curly_index != npos &&
                "invalid message format: missing }");
    String8_View curly_content = remaining_message.substr(
        left_curly_index + 1, right_curly_index - (left_curly_index + 1));

    String8_View expanded_parameter;
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
