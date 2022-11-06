// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

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
#include <quick-lint-js/fe/null-visitor.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <utility>

namespace quick_lint_js {
void parser::parse_typescript_colon_for_type() {
  QLJS_ASSERT(this->peek().type == token_type::colon);
  if (!this->options_.typescript && !this->in_typescript_only_construct_) {
    this->diag_reporter_->report(
        diag_typescript_type_annotations_not_allowed_in_javascript{
            .type_colon = this->peek().span(),
        });
  }
  this->skip();
}

void parser::parse_and_visit_typescript_colon_type_expression(
    parse_visitor_base &v) {
  this->parse_typescript_colon_for_type();
  this->parse_and_visit_typescript_type_expression(v);
}

void parser::parse_and_visit_typescript_type_expression(
    parse_visitor_base &v, bool parse_question_as_invalid) {
  depth_guard guard(this);
  typescript_only_construct_guard ts_guard =
      this->enter_typescript_only_construct();

  bool is_array_type = false;
  bool is_tuple_type = false;

  std::optional<source_code_span> leading_binary_operator;  // '|' or '&'
  if (this->peek().type == token_type::ampersand ||
      this->peek().type == token_type::pipe) {
    // | Type
    // & Type
    leading_binary_operator = this->peek().span();
    this->skip();
  }

again:
  std::optional<source_code_span> readonly_keyword;
  if (this->peek().type == token_type::kw_readonly) {
    // readonly Type[]
    // readonly [Type, Type]
    readonly_keyword = this->peek().span();
    this->skip();
  }

  switch (this->peek().type) {
  case token_type::complete_template:
  case token_type::kw_any:
  case token_type::kw_bigint:
  case token_type::kw_boolean:
  case token_type::kw_false:
  case token_type::kw_never:
  case token_type::kw_null:
  case token_type::kw_number:
  case token_type::kw_object:
  case token_type::kw_string:
  case token_type::kw_symbol:
  case token_type::kw_this:
  case token_type::kw_true:
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

  //: ?Type // invalid
  case token_type::question:
    this->diag_reporter_->report(
        diag_typescript_question_in_type_expression_should_be_void{
            .question = this->peek().span()});
    this->skip();
    goto again;
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
    is_tuple_type = true;
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

  case token_type::comma:
  case token_type::end_of_file:
  case token_type::right_curly:
  case token_type::right_paren:
  default:
    this->diag_reporter_->report(diag_missing_typescript_type{
        .expected_type = source_code_span::unit(this->peek().begin),
    });
    break;
  }

  //: Type? // invalid
  if (!is_tuple_type && parse_question_as_invalid &&
      this->peek().type == token_type::question) {
    this->diag_reporter_->report(
        diag_typescript_question_in_type_expression_should_be_void{
            .question = this->peek().span()});
    this->skip();
  }

  while (this->peek().type == token_type::left_square) {
    // typeexpr[]
    // typeexpr[Key]
    this->skip();
    if (this->peek().type == token_type::right_square) {
      is_array_type = true;
      this->skip();
    } else {
      this->parse_and_visit_typescript_type_expression(v);
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_square);
      this->skip();
    }
  }
  if (readonly_keyword.has_value() && !(is_array_type || is_tuple_type)) {
    this->diag_reporter_->report(
        diag_typescript_readonly_in_type_needs_array_or_tuple_type{
            .readonly_keyword = *readonly_keyword,
        });
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
    this->parse_and_visit_typescript_type_expression(v, false);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::question);
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::colon);
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
  }
}

void parser::parse_and_visit_typescript_colon_type_expression_or_type_predicate(
    parse_visitor_base &v) {
  this->parse_typescript_colon_for_type();
  this->parse_and_visit_typescript_type_expression_or_type_predicate(v);
}

void parser::parse_and_visit_typescript_type_expression_or_type_predicate(
    parse_visitor_base &v) {
  switch (this->peek().type) {
  // param is Type
  // Type
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
  case token_type::identifier:
  case token_type::kw_await:
  case token_type::kw_this:
  case token_type::kw_yield: {
    token_type parameter_type = this->peek().type;
    identifier parameter_name = this->peek().identifier_name();
    lexer_transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    if (this->peek().type == token_type::kw_is) {
      // param is Type
      this->lexer_.commit_transaction(std::move(transaction));
      this->skip();
      if (parameter_type != token_type::kw_this) {
        v.visit_variable_type_predicate_use(parameter_name);
      }
      this->parse_and_visit_typescript_type_expression(v);
    } else {
      // Type
      this->lexer_.roll_back_transaction(std::move(transaction));
      this->parse_and_visit_typescript_type_expression(v);
    }
    break;
  }

  // {key: Value}
  // typeof v
  // () => ReturnType
  default:
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
  this->parse_and_visit_function_parameters(
      v, variable_kind::_function_type_parameter);
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
  this->skip();
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::equal_greater);
  this->skip();
  this->parse_and_visit_typescript_type_expression(v);
}

parser::typescript_type_arrow_or_paren
parser::parse_and_visit_typescript_arrow_or_paren_type_expression(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::left_paren);
  this->skip();

  if (this->peek().type == token_type::right_paren) {
    // () => ReturnType
    this->parse_and_visit_typescript_arrow_type_expression_after_left_paren(v);
    return typescript_type_arrow_or_paren::arrow;
  }

  // TODO(strager): Performance of this code probably sucks. I suspect arrow
  // types are more common than parenthesized types, so we should assume arrow
  // and fall back to parenthesized.

  typescript_type_arrow_or_paren result = typescript_type_arrow_or_paren::paren;
  this->try_parse(
      [&] {
        stacked_buffering_visitor params_visitor =
            this->buffering_visitor_stack_.push();
        const char8 *old_begin = this->peek().begin;
        this->parse_and_visit_typescript_type_expression(
            params_visitor.visitor());
        if (this->peek().begin == old_begin) {
          // We didn't parse anything.
          // (...params) => ReturnType
          return false;
        }
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
            params_visitor.visitor().move_into(v);
            return true;
          }
          break;

        // (param, param) => ReturnType
        // (param: Type) => ReturnType
        // (param?) => ReturnType
        case token_type::colon:
        case token_type::comma:
        case token_type::question:
          return false;

        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
        }
        QLJS_UNREACHABLE();
      },
      [&] {
        result = typescript_type_arrow_or_paren::arrow;
        this->parse_and_visit_typescript_arrow_type_expression_after_left_paren(
            v);
      });
  return result;
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
          this->diag_reporter_->report(
              diag_missing_separator_between_object_type_entries{
                  .expected_separator = source_code_span::unit(
                      this->lexer_.end_of_previous_token()),
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
          v.visit_variable_declaration(
              ident, variable_kind::_index_signature_parameter,
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

  const char8 *first_unnamed_element_begin = nullptr;
  std::optional<source_code_span> first_named_tuple_name_and_colon;
  std::optional<source_code_span> last_optional_question;
  std::optional<source_code_span> first_spread;
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

    std::optional<source_code_span> spread;
    if (this->peek().type == token_type::dot_dot_dot) {
      // [...Type]
      spread = this->peek().span();
      if (first_spread.has_value()) {
        // [...Type1, ...Type2]
        // [...Type1[], ...Type2[]]  // Invalid.
        // TODO(#867): Report a diagnostic if the spread type is an array type.
      } else {
        first_spread = spread;
      }
      this->skip();
    }

    std::optional<source_code_span> optional_question;

    // expected_optional_question is where a '?' could syntactically be placed
    // to make the tuple element optional. If nullptr, '?' can be placed after
    // the type. expected_optional_question is only valid if optional_question
    // is nullopt.
    const char8 *expected_optional_question = nullptr;

    switch (this->peek().type) {
    case token_type::right_square:
      this->skip();
      return;

    // [: Type]  // Invalid.
    case token_type::colon: {
      source_code_span colon_span = this->peek().span();
      this->diag_reporter_->report(
          diag_typescript_missing_name_in_named_tuple_type{
              .colon = colon_span,
          });
      this->skip();
      this->parse_and_visit_typescript_type_expression(v, false);
      break;
    }

    // [name: Type]
    // [Type]
    QLJS_CASE_CONTEXTUAL_KEYWORD:
    QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    case token_type::identifier:
    case token_type::kw_await:
    case token_type::kw_false:
    case token_type::kw_function:
    case token_type::kw_import:
    case token_type::kw_new:
    case token_type::kw_null:
    case token_type::kw_this:
    case token_type::kw_true:
    case token_type::kw_typeof:
    case token_type::kw_void:
    case token_type::kw_yield: {
      lexer_transaction transaction = this->lexer_.begin_transaction();
      const char8 *element_begin = this->peek().begin;
      this->skip();

      bool is_named_element;
      if (this->peek().type == token_type::question) {
        // [Type?]
        // [name?: Type]
        optional_question = this->peek().span();
        this->skip();
        if (this->peek().type == token_type::colon) {
          // [name?: Type]
          is_named_element = true;
        } else {
          // [Type?]
          is_named_element = false;
        }
      } else if (this->peek().type == token_type::colon) {
        // [name: Type]
        expected_optional_question = this->lexer_.end_of_previous_token();
        is_named_element = true;
      } else {
        // [Type]
        is_named_element = false;
      }

      if (is_named_element) {
        // [name: Type]
        // [name?: Type]
        QLJS_ASSERT(this->peek().type == token_type::colon);
        const char8 *colon_end = this->peek().end;
        if (!first_named_tuple_name_and_colon.has_value()) {
          source_code_span name_and_colon(element_begin, colon_end);
          if (first_unnamed_element_begin) {
            // [Type1, name: Type2]  // Invalid.
            this->diag_reporter_->report(
                diag_typescript_missing_name_and_colon_in_named_tuple_type{
                    .expected_name_and_colon =
                        source_code_span::unit(first_unnamed_element_begin),
                    .existing_name = name_and_colon,
                });
          }
          first_named_tuple_name_and_colon = name_and_colon;
        }

        this->skip();
        this->lexer_.commit_transaction(std::move(transaction));

        if (this->peek().type == token_type::dot_dot_dot) {
          // [name: ...Type]  // Invalid.
          // [...name: ...Type]  // Invalid.
          if (spread.has_value()) {
            this->diag_reporter_->report(
                diag_typescript_named_tuple_element_spread_before_name_and_type{
                    .type_spread = this->peek().span(),
                    .name_spread = *spread,
                });
          } else {
            this->diag_reporter_->report(
                diag_typescript_named_tuple_element_spread_before_type{
                    .spread = this->peek().span(),
                    .expected_spread = source_code_span::unit(element_begin),
                });
          }
          this->skip();
        }

        this->parse_and_visit_typescript_type_expression(v, false);
      } else {
        // [Type]
        // [Type?]
        this->lexer_.roll_back_transaction(std::move(transaction));

        // We unparsed the '?' (if any). Reset optional_question so we parse it
        // again later.
        optional_question = std::nullopt;

        if (first_named_tuple_name_and_colon.has_value()) {
          // [name: Type1, Type2]  // Invalid.
          this->diag_reporter_->report(
              diag_typescript_missing_name_and_colon_in_named_tuple_type{
                  .expected_name_and_colon =
                      source_code_span::unit(this->peek().begin),
                  .existing_name = *first_named_tuple_name_and_colon,
              });
        }
        first_unnamed_element_begin = this->peek().begin;

        this->parse_and_visit_typescript_type_expression(v, false);
      }
      break;
    }

    // [(Type)]
    default:
      first_unnamed_element_begin = this->peek().begin;
      this->parse_and_visit_typescript_type_expression(v, false);
      break;
    }

    if (this->peek().type == token_type::question) {
      // [(Type)?]
      // [name: Type?]  // Invalid.
      // [name?: Type?]  // Invalid.
      if (optional_question.has_value()) {
        // [name?: Type?]  // Invalid.
        this->diag_reporter_->report(
            diag_typescript_named_tuple_element_question_after_name_and_type{
                .type_question = this->peek().span(),
                .name_question = *optional_question,
            });
      } else if (expected_optional_question) {
        // [name: Type?]  // Invalid.
        this->diag_reporter_->report(
            diag_typescript_named_tuple_element_question_after_type{
                .question = this->peek().span(),
                .expected_question =
                    source_code_span::unit(expected_optional_question),
            });
      }

      optional_question = this->peek().span();
      this->skip();
    }

    if (optional_question.has_value()) {
      if (spread.has_value()) {
        // [...Type?]  // Invalid.
        this->diag_reporter_->report(
            diag_typescript_spread_element_cannot_be_optional{
                .optional_question = *optional_question,
                .spread = *spread,
            });
        // Don't set last_optional_question; pretend the '?' wasn't there.
      } else if (first_spread.has_value()) {
        // [...Type1, Type2?]  // Invalid.
        this->diag_reporter_->report(
            diag_typescript_optional_tuple_element_cannot_follow_spread_element{
                .optional_question = *optional_question,
                .previous_spread = *first_spread,
            });
        last_optional_question = *optional_question;
      } else {
        // [Type?]
        // [name?: Type]
        last_optional_question = *optional_question;
      }
    } else if (last_optional_question.has_value()) {
      if (spread.has_value()) {
        // [Type1?, ...Type2]
      } else {
        // [Type1?, Type2]  // Invalid.
        if (!expected_optional_question) {
          expected_optional_question = this->lexer_.end_of_previous_token();
        }
        this->diag_reporter_->report(
            diag_typescript_required_tuple_element_after_optional_element{
                .expected_question =
                    source_code_span::unit(expected_optional_question),
                .previous_optional_question = *last_optional_question,
            });
      }
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

  switch (this->peek().type) {
  case token_type::greater:
    this->skip();
    break;
  case token_type::greater_greater:
  case token_type::greater_greater_greater:
    this->lexer_.skip_as_greater();
    break;
  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
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
