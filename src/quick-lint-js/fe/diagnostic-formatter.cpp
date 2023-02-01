// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <initializer_list>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/fe/diagnostic-formatter.h>
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
string8_view headlinese_enum_kind(enum_kind ek) noexcept {
  switch (ek) {
  case enum_kind::const_enum:
    return u8"const enum"_sv;
  case enum_kind::declare_const_enum:
    return u8"declare const enum"_sv;
  case enum_kind::declare_enum:
    return u8"declare enum"_sv;
  case enum_kind::normal:
    return u8"enum"_sv;
  }
  QLJS_UNREACHABLE();
}

translatable_message headlinese_statement_kind(statement_kind sk) noexcept {
  switch (sk) {
  case statement_kind::do_while_loop:
    return QLJS_TRANSLATABLE("'do-while' loop");
  case statement_kind::for_loop:
    return QLJS_TRANSLATABLE("'for' loop");
  case statement_kind::if_statement:
    return QLJS_TRANSLATABLE("'if' statement");
  case statement_kind::while_loop:
    return QLJS_TRANSLATABLE("'while' loop");
  case statement_kind::with_statement:
    return QLJS_TRANSLATABLE("'with' statement");
  case statement_kind::labelled_statement:
    return QLJS_TRANSLATABLE("labelled statement");
  }
  QLJS_UNREACHABLE();
}

translatable_message singular_statement_kind(statement_kind sk) noexcept {
  switch (sk) {
  case statement_kind::do_while_loop:
    return QLJS_TRANSLATABLE("a 'do-while' loop");
  case statement_kind::for_loop:
    return QLJS_TRANSLATABLE("a 'for' loop");
  case statement_kind::if_statement:
    return QLJS_TRANSLATABLE("an 'if' statement");
  case statement_kind::while_loop:
    return QLJS_TRANSLATABLE("a 'while' loop");
  case statement_kind::with_statement:
    return QLJS_TRANSLATABLE("a 'with' statement");
  case statement_kind::labelled_statement:
    return QLJS_TRANSLATABLE("a labelled statement");
  }
  QLJS_UNREACHABLE();
}

diagnostic_formatter_base::diagnostic_formatter_base(translator t)
    : translator_(t) {}

source_code_span diagnostic_formatter_base::get_argument_source_code_span(
    const diagnostic_message_args& args, const void* diagnostic,
    int arg_index) {
  auto [arg_data, arg_type] = get_arg(args, diagnostic, arg_index);
  switch (arg_type) {
  case diagnostic_arg_type::identifier:
    return reinterpret_cast<const identifier*>(arg_data)->span();

  case diagnostic_arg_type::source_code_span:
    return *reinterpret_cast<const source_code_span*>(arg_data);

  case diagnostic_arg_type::char8:
  case diagnostic_arg_type::enum_kind:
  case diagnostic_arg_type::invalid:
  case diagnostic_arg_type::statement_kind:
  case diagnostic_arg_type::string8_view:
  case diagnostic_arg_type::variable_kind:
    QLJS_UNREACHABLE();
  }
  QLJS_UNREACHABLE();
}

string8_view diagnostic_formatter_base::expand_argument(
    const diagnostic_message_args& args, const void* diagnostic,
    int arg_index) {
  auto [arg_data, arg_type] = get_arg(args, diagnostic, arg_index);
  switch (arg_type) {
  case diagnostic_arg_type::char8:
    return string8_view(reinterpret_cast<const char8*>(arg_data), 1);

  case diagnostic_arg_type::identifier:
    return reinterpret_cast<const identifier*>(arg_data)->span().string_view();

  case diagnostic_arg_type::source_code_span:
    return reinterpret_cast<const source_code_span*>(arg_data)->string_view();

  case diagnostic_arg_type::string8_view:
    return *reinterpret_cast<const string8_view*>(arg_data);

  case diagnostic_arg_type::enum_kind:
  case diagnostic_arg_type::invalid:
  case diagnostic_arg_type::statement_kind:
  case diagnostic_arg_type::variable_kind:
    QLJS_UNREACHABLE();
  }
  QLJS_UNREACHABLE();
}

string8_view diagnostic_formatter_base::expand_argument_headlinese(
    const diagnostic_message_args& args, const void* diagnostic,
    int arg_index) {
  auto [arg_data, arg_type] = get_arg(args, diagnostic, arg_index);
  switch (arg_type) {
  case diagnostic_arg_type::enum_kind:
    return headlinese_enum_kind(*reinterpret_cast<const enum_kind*>(arg_data));

  case diagnostic_arg_type::statement_kind:
    return string8_view::from_c_str(
        this->translator_.translate(headlinese_statement_kind(
            *reinterpret_cast<const statement_kind*>(arg_data))));

  case diagnostic_arg_type::char8:
  case diagnostic_arg_type::identifier:
  case diagnostic_arg_type::invalid:
  case diagnostic_arg_type::source_code_span:
  case diagnostic_arg_type::string8_view:
  case diagnostic_arg_type::variable_kind:
    QLJS_UNREACHABLE();
  }
  QLJS_UNREACHABLE();
}

string8_view diagnostic_formatter_base::expand_argument_singular(
    const diagnostic_message_args& args, const void* diagnostic,
    int arg_index) {
  auto [arg_data, arg_type] = get_arg(args, diagnostic, arg_index);
  switch (arg_type) {
  case diagnostic_arg_type::statement_kind:
    return string8_view::from_c_str(
        this->translator_.translate(singular_statement_kind(
            *reinterpret_cast<const statement_kind*>(arg_data))));

  case diagnostic_arg_type::enum_kind:
    QLJS_UNIMPLEMENTED();
    break;

  case diagnostic_arg_type::char8:
  case diagnostic_arg_type::identifier:
  case diagnostic_arg_type::invalid:
  case diagnostic_arg_type::source_code_span:
  case diagnostic_arg_type::string8_view:
  case diagnostic_arg_type::variable_kind:
    QLJS_UNREACHABLE();
  }
  QLJS_UNREACHABLE();
}

std::pair<const void*, diagnostic_arg_type> diagnostic_formatter_base::get_arg(
    const diagnostic_message_args& args, const void* diagnostic,
    int arg_index) noexcept {
  const diagnostic_message_arg_info& arg_info =
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
