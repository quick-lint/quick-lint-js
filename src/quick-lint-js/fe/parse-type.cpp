// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/container/pmr/memory_resource.hpp>
#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/buffering-visitor.h>
#include <quick-lint-js/fe/diag-reporter.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/expression.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/location.h>
#include <quick-lint-js/fe/null-visitor.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <utility>

namespace quick_lint_js {
void parser::parse_and_visit_typescript_colon_type_expression(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::colon);
  if (!this->options_.typescript && !this->in_typescript_only_construct_) {
    this->diag_reporter_->report(
        diag_typescript_type_annotations_not_allowed_in_javascript{
            .type_colon = this->peek().span(),
        });
  }
  this->skip();
  this->parse_and_visit_typescript_type_expression(v);
}

void parser::parse_and_visit_typescript_type_expression(parse_visitor_base &v) {
  depth_guard guard(this);

  std::optional<source_code_span> leading_binary_operator;  // '|' or '&'
  if (this->peek().type == token_type::ampersand ||
      this->peek().type == token_type::pipe) {
    // | Type
    // & Type
    leading_binary_operator = this->peek().span();
    this->skip();
  }

again:
  switch (this->peek().type) {
  case token_type::complete_template:
  case token_type::kw_any:
  case token_type::kw_bigint:
  case token_type::kw_boolean:
  case token_type::kw_never:
  case token_type::kw_null:
  case token_type::kw_number:
  case token_type::kw_object:
  case token_type::kw_string:
  case token_type::kw_symbol:
  case token_type::kw_this:
  case token_type::kw_undefined:
  case token_type::kw_unknown:
  case token_type::kw_void:
  case token_type::number:
  case token_type::string:
    this->skip();
    break;

  // `template ${sometype}`
  case token_type::incomplete_template:
    this->parse_and_visit_typescript_template_type_expression(v);
    break;

  // Type
  // ns.Type<T>
  case token_type::kw_abstract:
  case token_type::kw_as:
  case token_type::kw_assert:
  case token_type::kw_asserts:
  case token_type::kw_async:
  case token_type::kw_constructor:
  case token_type::kw_declare:
  case token_type::kw_from:
  case token_type::kw_get:
  case token_type::kw_global:
  case token_type::kw_infer:
  case token_type::kw_intrinsic:
  case token_type::kw_is:
  case token_type::kw_module:
  case token_type::kw_namespace:
  case token_type::kw_of:
  case token_type::kw_out:
  case token_type::kw_override:
  case token_type::kw_readonly:
  case token_type::kw_require:
  case token_type::kw_set:
  case token_type::kw_type:
  case token_type::identifier: {
    identifier name = this->peek().identifier_name();
    bool had_dot = false;
    this->skip();
    while (this->peek().type == token_type::dot) {
      had_dot = true;
      this->skip();
      switch (this->peek().type) {
      QLJS_CASE_KEYWORD:
      case token_type::identifier:
        this->skip();
        break;
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
    if (had_dot) {
      v.visit_variable_namespace_use(name);
    } else {
      v.visit_variable_type_use(name);
    }
    if (this->peek().type == token_type::less ||
        this->peek().type == token_type::less_less) {
      this->parse_and_visit_typescript_generic_arguments(v);
    }
    break;
  }

  // unique
  // unique.prop
  // unique symbol
  case token_type::kw_unique:
    this->skip();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::kw_symbol);
    this->skip();
    break;

  // [A, B, C]
  case token_type::left_square:
    this->parse_and_visit_typescript_tuple_type_expression(v);
    break;

  // (typeexpr)
  // (param, param) => ReturnType
  case token_type::left_paren:
    this->parse_and_visit_typescript_arrow_or_paren_type_expression(v);
    break;

  // new (param, param) => ReturnType
  case token_type::kw_new:
    this->skip();
    this->parse_and_visit_typescript_arrow_type_expression(v);
    break;

  // <T>(param, param) => ReturnType
  case token_type::less:
    this->parse_and_visit_typescript_arrow_type_expression(v);
    break;

  // { key: value }
  case token_type::left_curly:
    this->parse_and_visit_typescript_object_type_expression(v);
    break;

  // & & Type  // Invalid.
  // | | Type  // Invalid.
  case token_type::ampersand:
  case token_type::pipe:
    this->diag_reporter_->report(
        diag_missing_type_between_intersection_or_union{
            .left_operator = leading_binary_operator.value(),
            .right_operator = this->peek().span(),
        });
    break;

  // typeof varname
  // typeof import("modulename")
  // typeof ns.varname[KeyType]
  // typeof varname[]
  // typeof MyClass<T>
  case token_type::kw_typeof:
    this->skip();
    switch (this->peek().type) {
    case token_type::identifier:
    case token_type::kw_abstract:
    case token_type::kw_as:
    case token_type::kw_assert:
    case token_type::kw_asserts:
    case token_type::kw_async:
    case token_type::kw_await:
    case token_type::kw_bigint:
    case token_type::kw_break:
    case token_type::kw_case:
    case token_type::kw_catch:
    case token_type::kw_class:
    case token_type::kw_const:
    case token_type::kw_constructor:
    case token_type::kw_continue:
    case token_type::kw_debugger:
    case token_type::kw_declare:
    case token_type::kw_default:
    case token_type::kw_delete:
    case token_type::kw_do:
    case token_type::kw_else:
    case token_type::kw_enum:
    case token_type::kw_export:
    case token_type::kw_extends:
    case token_type::kw_false:
    case token_type::kw_finally:
    case token_type::kw_for:
    case token_type::kw_from:
    case token_type::kw_function:
    case token_type::kw_get:
    case token_type::kw_global:
    case token_type::kw_if:
    case token_type::kw_in:
    case token_type::kw_infer:
    case token_type::kw_instanceof:
    case token_type::kw_intrinsic:
    case token_type::kw_is:
    case token_type::kw_keyof:
    case token_type::kw_module:
    case token_type::kw_namespace:
    case token_type::kw_new:
    case token_type::kw_null:
    case token_type::kw_object:
    case token_type::kw_of:
    case token_type::kw_out:
    case token_type::kw_override:
    case token_type::kw_readonly:
    case token_type::kw_require:
    case token_type::kw_return:
    case token_type::kw_set:
    case token_type::kw_super:
    case token_type::kw_switch:
    case token_type::kw_symbol:
    case token_type::kw_throw:
    case token_type::kw_true:
    case token_type::kw_try:
    case token_type::kw_type:
    case token_type::kw_typeof:
    case token_type::kw_undefined:
    case token_type::kw_unique:
    case token_type::kw_var:
    case token_type::kw_void:
    case token_type::kw_while:
    case token_type::kw_with:
      v.visit_variable_use(this->peek().identifier_name());
      this->skip();
      break;

    // typeof this
    // typeof this.prop
    case token_type::kw_this:
      this->skip();
      break;

    // typeof import("modulename")
    case token_type::kw_import:
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::string);
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
      this->skip();
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }

    while (this->peek().type == token_type::dot) {
      this->skip();
      switch (this->peek().type) {
      QLJS_CASE_KEYWORD:
      case token_type::identifier:
      case token_type::private_identifier:
        this->skip();
        break;
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
    if (this->peek().type == token_type::less) {
      this->parse_and_visit_typescript_generic_arguments(v);
    }
    while (this->peek().type == token_type::dot) {
      source_code_span dot_span = this->peek().span();
      this->skip();
      switch (this->peek().type) {
      QLJS_CASE_KEYWORD:
      case token_type::identifier:
        this->diag_reporter_->report(
            diag_dot_not_allowed_after_generic_arguments_in_type{
                .dot = dot_span,
                .property_name = this->peek().span(),
            });
        this->skip();
        break;
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
    break;

  // keyof Type
  case token_type::kw_keyof:
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  while (this->peek().type == token_type::left_square) {
    // typeexpr[]
    // typeexpr[Key]
    this->skip();
    if (this->peek().type == token_type::right_square) {
      this->skip();
    } else {
      this->parse_and_visit_typescript_type_expression(v);
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_square);
      this->skip();
    }
  }

  if (this->peek().type == token_type::ampersand ||
      this->peek().type == token_type::pipe) {
    // Type1 | Type2
    leading_binary_operator = this->peek().span();
    this->skip();
    goto again;
  }

  if (this->peek().type == token_type::kw_extends) {
    // T extends T ? T : T
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::question);
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::colon);
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
  }
}

void parser::parse_and_visit_typescript_arrow_type_expression(
    parse_visitor_base &v) {
  v.visit_enter_function_scope();
  if (this->peek().type == token_type::less) {
    this->parse_and_visit_typescript_generic_parameters(v);
  }
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
  this->skip();
  this->parse_and_visit_typescript_arrow_type_expression_after_left_paren_no_scope(
      v);
  v.visit_exit_function_scope();
}

void parser::parse_and_visit_typescript_arrow_type_expression_after_left_paren(
    parse_visitor_base &v) {
  v.visit_enter_function_scope();
  this->parse_and_visit_typescript_arrow_type_expression_after_left_paren_no_scope(
      v);
  v.visit_exit_function_scope();
}

void parser::
    parse_and_visit_typescript_arrow_type_expression_after_left_paren_no_scope(
        parse_visitor_base &v) {
  this->parse_and_visit_function_parameters(v);
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
  this->skip();
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::equal_greater);
  this->skip();
  this->parse_and_visit_typescript_type_expression(v);
}

void parser::parse_and_visit_typescript_arrow_or_paren_type_expression(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::left_paren);
  this->skip();

  if (this->peek().type == token_type::right_paren) {
    // () => ReturnType
    this->parse_and_visit_typescript_arrow_type_expression_after_left_paren(v);
    return;
  }

  // TODO(strager): Performance of this code probably sucks. I suspect arrow
  // types are more common than parenthesized types, so we should assume arrow
  // and fall back to parenthesized.

  this->try_parse(
      [&] {
        buffering_visitor &params_visitor =
            this->buffering_visitor_stack_.emplace(
                boost::container::pmr::new_delete_resource());
        this->parse_and_visit_typescript_type_expression(params_visitor);
        switch (this->peek().type) {
        // (typeexpr)
        // (param) => ReturnType
        case token_type::right_paren:
          this->skip();

          if (this->peek().type == token_type::equal_greater) {
            // (param, param) => ReturnType
            return false;
          } else {
            // (typeexpr)
            params_visitor.move_into(v);
            return true;
          }
          break;

        // (param, param) => ReturnType
        // (param: Type) => ReturnType
        case token_type::colon:
        case token_type::comma:
          return false;

        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
        }
      },
      [&] {
        this->parse_and_visit_typescript_arrow_type_expression_after_left_paren(
            v);
      });
}

void parser::parse_and_visit_typescript_object_type_expression(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::left_curly);
  this->skip();

  auto parse_after_property_name =
      [&](const std::optional<source_code_span> &name) -> void {
    switch (this->peek().type) {
    // { prop? }
    case token_type::question:
      this->skip();
      break;

    // { [k: T]+? }
    // { [k: T]-? }
    case token_type::minus:
    case token_type::plus:
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::question);
      this->skip();
      break;

    default:
      break;
    }

    switch (this->peek().type) {
    // { prop: Type }
    case token_type::colon:
      this->parse_and_visit_typescript_colon_type_expression(v);
      break;

    // { method() }
    // { method<T>() }
    case token_type::left_paren:
    case token_type::less:
      v.visit_enter_function_scope();
      this->parse_and_visit_interface_function_parameters_and_body_no_scope(
          v, name, function_attributes::normal);
      v.visit_exit_function_scope();
      break;

    case token_type::comma:
    case token_type::right_curly:
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  };

  bool is_first = true;
  for (;;) {
    if (!is_first) {
      switch (this->peek().type) {
      case token_type::comma:
      case token_type::semicolon:
        this->skip();
        break;

      case token_type::right_curly:
        break;

      default:
        if (!this->peek().has_leading_newline) {
          const char8 *where = this->lexer_.end_of_previous_token();
          this->diag_reporter_->report(
              diag_missing_separator_between_object_type_entries{
                  .expected_separator = source_code_span(where, where),
              });
        }
        break;
      }
    }

    switch (this->peek().type) {
    // { readonly prop: Type }
    case token_type::kw_readonly:
      this->skip();
      break;

    // { get prop(): Type }
    // { set prop(v: Type) }
    case token_type::kw_get:
    case token_type::kw_set:
      this->skip();
      break;

    // { -readonly [key: Type]: Type }
    // { +readonly [key: Type]: Type }
    case token_type::minus:
    case token_type::plus:
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::kw_readonly);
      this->skip();
      break;

    default:
      break;
    }

    switch (this->peek().type) {
    // { prop }
    // { prop: Type }
    // { prop?: Type }
    // { method(): Type }
    QLJS_CASE_KEYWORD:
    case token_type::identifier: {
      source_code_span name = this->peek().span();
      this->skip();
      parse_after_property_name(name);
      break;
    }

    // { readonly: Type }
    // { get?: Type }
    // { : }  // Invalid.
    // { ? }  // Invalid.
    case token_type::colon:
    case token_type::question: {
      // TODO(strager): Error if the previous token wasn't a modifier like
      // 'readonly' or 'get'.
      std::optional<source_code_span> modifier_span;  // TODO(strager)
      parse_after_property_name(modifier_span);
      break;
    }

    // { [expr] }
    // { [expr]: Type }
    // { [expr](): Type }
    case token_type::left_square: {
      this->skip();

      bool is_index_signature = false;

      switch (this->peek().type) {
      // { [varname]: Type }
      // { [key: Type]: Type }
      // { [Key in Type]: Type }
      // TODO(#765): QLJS_CASE_CONTEXTUAL_KEYWORD overmatches. 'let' and
      // 'static' should error instead.
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case token_type::identifier: {
        token_type ident_token_type = this->peek().type;
        identifier ident = this->peek().identifier_name();
        this->skip();
        switch (this->peek().type) {
        // { [key: Type]: Type }
        case token_type::colon:
          is_index_signature = true;
          v.visit_enter_index_signature_scope();
          this->parse_and_visit_typescript_colon_type_expression(v);
          v.visit_variable_declaration(ident, variable_kind::_parameter,
                                       variable_init_kind::normal);
          break;

        // { [key in Type]: Type }
        case token_type::kw_in:
          this->skip();
          is_index_signature = true;
          v.visit_enter_index_signature_scope();
          this->parse_and_visit_typescript_type_expression(v);
          v.visit_variable_declaration(ident, variable_kind::_generic_parameter,
                                       variable_init_kind::normal);
          if (this->peek().type == token_type::kw_as) {
            this->skip();
            this->parse_and_visit_typescript_type_expression(v);
          }
          break;

        // { [varname]: Type }
        case token_type::right_square:
        default: {
          expression *property_name =
              this->make_expression<expression::variable>(ident,
                                                          ident_token_type);
          property_name =
              this->parse_expression_remainder(v, property_name, precedence{});
          this->visit_expression(property_name, v, variable_context::rhs);
          break;
        }
        }
        break;
      }

      // { [(expr)]: Type }
      // { ['literal']: Type }
      default:
        expression *property_name = this->parse_expression(v);
        this->visit_expression(property_name, v, variable_context::rhs);
        break;
      }

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_square);
      this->skip();

      parse_after_property_name(std::nullopt);

      if (is_index_signature) {
        v.visit_exit_index_signature_scope();
      }
      break;
    }

    // { () }
    // { (param: Type): Type }
    // { <T>(param: Type): Type }
    case token_type::left_paren:
    case token_type::less:
      v.visit_enter_function_scope();
      this->parse_and_visit_interface_function_parameters_and_body_no_scope(
          v, std::nullopt, function_attributes::normal);
      v.visit_exit_function_scope();
      break;

    case token_type::right_curly:
      this->skip();
      return;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    is_first = false;
  }
}

void parser::parse_and_visit_typescript_template_type_expression(
    parse_visitor_base &v) {
  const char8 *template_begin = this->peek().begin;
  for (;;) {
    QLJS_ASSERT(this->peek().type == token_type::incomplete_template);
    // TODO(strager): report_errors_for_escape_sequences_in_template
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
    switch (this->peek().type) {
    case token_type::right_curly:
      this->lexer_.skip_in_template(template_begin);
      switch (this->peek().type) {
      case token_type::complete_template:
        // TODO(strager): report_errors_for_escape_sequences_in_template
        this->skip();
        return;

      case token_type::incomplete_template:
        continue;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  }
}

void parser::parse_and_visit_typescript_tuple_type_expression(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::left_square);
  this->skip();
  bool is_first = true;
  for (;;) {
    if (!is_first) {
      switch (this->peek().type) {
      case token_type::comma:
        this->skip();
        break;

      case token_type::right_square:
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }
    }
    switch (this->peek().type) {
    case token_type::right_square:
      this->skip();
      return;

    case token_type::identifier:
    default:
      this->parse_and_visit_typescript_type_expression(v);
      break;
    }
    is_first = false;
  }
}

void parser::parse_and_visit_typescript_generic_arguments(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::less ||
              this->peek().type == token_type::less_less);
  if (this->peek().type == token_type::less_less) {
    // <<T>() => void>
    this->lexer_.skip_less_less_as_less();
  } else {
    // <T>
    QLJS_ASSERT(this->peek().type == token_type::less);
    this->skip();
  }

  this->parse_and_visit_typescript_type_expression(v);
  while (this->peek().type == token_type::comma) {
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
  }

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::greater);
  this->skip();
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
