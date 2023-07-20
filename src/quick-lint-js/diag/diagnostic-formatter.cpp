// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <initializer_list>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/diag/diagnostic-formatter.h>
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
String8_View headlinese_enum_kind(Enum_Kind ek) noexcept {
  switch (ek) {
  case Enum_Kind::const_enum:
    return u8"const enum"_sv;
  case Enum_Kind::declare_const_enum:
    return u8"declare const enum"_sv;
  case Enum_Kind::declare_enum:
    return u8"declare enum"_sv;
  case Enum_Kind::normal:
    return u8"enum"_sv;
  }
  QLJS_UNREACHABLE();
}

Translatable_Message headlinese_statement_kind(Statement_Kind sk) noexcept {
  switch (sk) {
  case Statement_Kind::do_while_loop:
    return QLJS_TRANSLATABLE("'do-while' loop");
  case Statement_Kind::for_loop:
    return QLJS_TRANSLATABLE("'for' loop");
  case Statement_Kind::if_statement:
    return QLJS_TRANSLATABLE("'if' statement");
  case Statement_Kind::while_loop:
    return QLJS_TRANSLATABLE("'while' loop");
  case Statement_Kind::with_statement:
    return QLJS_TRANSLATABLE("'with' statement");
  case Statement_Kind::labelled_statement:
    return QLJS_TRANSLATABLE("labelled statement");
  }
  QLJS_UNREACHABLE();
}

Translatable_Message singular_statement_kind(Statement_Kind sk) noexcept {
  switch (sk) {
  case Statement_Kind::do_while_loop:
    return QLJS_TRANSLATABLE("a 'do-while' loop");
  case Statement_Kind::for_loop:
    return QLJS_TRANSLATABLE("a 'for' loop");
  case Statement_Kind::if_statement:
    return QLJS_TRANSLATABLE("an 'if' statement");
  case Statement_Kind::while_loop:
    return QLJS_TRANSLATABLE("a 'while' loop");
  case Statement_Kind::with_statement:
    return QLJS_TRANSLATABLE("a 'with' statement");
  case Statement_Kind::labelled_statement:
    return QLJS_TRANSLATABLE("a labelled statement");
  }
  QLJS_UNREACHABLE();
}

Diagnostic_Formatter_Base::Diagnostic_Formatter_Base(Translator t)
    : translator_(t) {}

Source_Code_Span Diagnostic_Formatter_Base::get_argument_source_code_span(
    const Diagnostic_Message_Args& args, const void* diagnostic,
    int arg_index) {
  auto [arg_data, arg_type] = get_arg(args, diagnostic, arg_index);
  switch (arg_type) {
  case Diagnostic_Arg_Type::source_code_span:
    return *reinterpret_cast<const Source_Code_Span*>(arg_data);

  case Diagnostic_Arg_Type::char8:
  case Diagnostic_Arg_Type::enum_kind:
  case Diagnostic_Arg_Type::invalid:
  case Diagnostic_Arg_Type::statement_kind:
  case Diagnostic_Arg_Type::string8_view:
  case Diagnostic_Arg_Type::variable_kind:
    QLJS_UNREACHABLE();
  }
  QLJS_UNREACHABLE();
}

String8_View Diagnostic_Formatter_Base::expand_argument(
    const Diagnostic_Message_Args& args, const void* diagnostic,
    int arg_index) {
  auto [arg_data, arg_type] = get_arg(args, diagnostic, arg_index);
  switch (arg_type) {
  case Diagnostic_Arg_Type::char8:
    return String8_View(reinterpret_cast<const Char8*>(arg_data), 1);

  case Diagnostic_Arg_Type::source_code_span:
    return reinterpret_cast<const Source_Code_Span*>(arg_data)->string_view();

  case Diagnostic_Arg_Type::string8_view:
    return *reinterpret_cast<const String8_View*>(arg_data);

  case Diagnostic_Arg_Type::enum_kind:
  case Diagnostic_Arg_Type::invalid:
  case Diagnostic_Arg_Type::statement_kind:
  case Diagnostic_Arg_Type::variable_kind:
    QLJS_UNREACHABLE();
  }
  QLJS_UNREACHABLE();
}

String8_View Diagnostic_Formatter_Base::expand_argument_headlinese(
    const Diagnostic_Message_Args& args, const void* diagnostic,
    int arg_index) {
  auto [arg_data, arg_type] = get_arg(args, diagnostic, arg_index);
  switch (arg_type) {
  case Diagnostic_Arg_Type::enum_kind:
    return headlinese_enum_kind(*reinterpret_cast<const Enum_Kind*>(arg_data));

  case Diagnostic_Arg_Type::statement_kind:
    return this->translator_.translate(headlinese_statement_kind(
        *reinterpret_cast<const Statement_Kind*>(arg_data)));

  case Diagnostic_Arg_Type::char8:
  case Diagnostic_Arg_Type::invalid:
  case Diagnostic_Arg_Type::source_code_span:
  case Diagnostic_Arg_Type::string8_view:
  case Diagnostic_Arg_Type::variable_kind:
    QLJS_UNREACHABLE();
  }
  QLJS_UNREACHABLE();
}

String8_View Diagnostic_Formatter_Base::expand_argument_singular(
    const Diagnostic_Message_Args& args, const void* diagnostic,
    int arg_index) {
  auto [arg_data, arg_type] = get_arg(args, diagnostic, arg_index);
  switch (arg_type) {
  case Diagnostic_Arg_Type::statement_kind:
    return this->translator_.translate(singular_statement_kind(
        *reinterpret_cast<const Statement_Kind*>(arg_data)));

  case Diagnostic_Arg_Type::enum_kind:
    QLJS_UNIMPLEMENTED();
    break;

  case Diagnostic_Arg_Type::char8:
  case Diagnostic_Arg_Type::invalid:
  case Diagnostic_Arg_Type::source_code_span:
  case Diagnostic_Arg_Type::string8_view:
  case Diagnostic_Arg_Type::variable_kind:
    QLJS_UNREACHABLE();
  }
  QLJS_UNREACHABLE();
}

std::pair<const void*, Diagnostic_Arg_Type> Diagnostic_Formatter_Base::get_arg(
    const Diagnostic_Message_Args& args, const void* diagnostic,
    int arg_index) noexcept {
  const Diagnostic_Message_Arg_Info& arg_info =
      args[narrow_cast<std::size_t>(arg_index)];
  const void* arg_data =
      reinterpret_cast<const char*>(diagnostic) + arg_info.offset();
  return std::make_pair(arg_data, arg_info.type);
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
