// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <memory>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-diag-reporter.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diag-reporter.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/expression.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/warning.h>
#include <utility>

namespace quick_lint_js {
void parser::visit_expression(expression* ast, parse_visitor_base& v,
                              parser::variable_context context) {
  auto visit_children = [&] {
    for (expression* child : ast->children()) {
      this->visit_expression(child, v, context);
    }
  };
  switch (ast->kind()) {
  case expression_kind::_class:
  case expression_kind::_invalid:
  case expression_kind::_missing:
  case expression_kind::arrow_function:
  case expression_kind::function:
  case expression_kind::import:
  case expression_kind::literal:
  case expression_kind::named_function:
  case expression_kind::new_target:
  case expression_kind::private_variable:
  case expression_kind::super:
  case expression_kind::yield_none:
    break;
  case expression_kind::_new:
  case expression_kind::_template:
  case expression_kind::array:
  case expression_kind::binary_operator:
  case expression_kind::call:
  case expression_kind::jsx_element_with_namespace:
  case expression_kind::jsx_fragment:
  case expression_kind::tagged_template_literal:
    visit_children();
    break;
  case expression_kind::trailing_comma: {
    auto& trailing_comma_ast = static_cast<expression::trailing_comma&>(*ast);
    this->diag_reporter_->report(diag_missing_operand_for_operator{
        .where = trailing_comma_ast.comma_span(),
    });
    visit_children();
    break;
  }
  case expression_kind::assignment: {
    expression* lhs = ast->child_0();
    expression* rhs = ast->child_1();
    this->visit_assignment_expression(lhs, rhs, v);
    break;
  }
  case expression_kind::compound_assignment:
  case expression_kind::conditional_assignment: {
    expression* lhs = ast->child_0();
    expression* rhs = ast->child_1();
    this->visit_compound_or_conditional_assignment_expression(lhs, rhs, v);
    break;
  }
  case expression_kind::_typeof: {
    expression* child = ast->child_0()->without_paren();
    if (child->kind() == expression_kind::variable) {
      v.visit_variable_typeof_use(child->variable_identifier());
    } else {
      this->visit_expression(child, v, context);
    }
    break;
  }
  case expression_kind::_delete: {
    expression* child = ast->child_0();
    if (child->kind() == expression_kind::variable) {
      v.visit_variable_delete_use(
          child->variable_identifier(),
          static_cast<expression::_delete*>(ast)->unary_operator_span());
    } else {
      this->visit_expression(child, v, context);
    }
    break;
  }
  case expression_kind::await:
  case expression_kind::spread:
  case expression_kind::unary_operator:
  case expression_kind::yield_many:
  case expression_kind::yield_one:
    this->visit_expression(ast->child_0(), v, context);
    break;
  case expression_kind::conditional:
    this->visit_expression(ast->child_0(), v, context);
    this->visit_expression(ast->child_1(), v, context);
    this->visit_expression(ast->child_2(), v, context);
    break;
  case expression_kind::dot:
    this->visit_expression(ast->child_0(), v, variable_context::rhs);
    break;
  case expression_kind::index:
    this->visit_expression(ast->child_0(), v, variable_context::rhs);
    this->visit_expression(ast->child_1(), v, variable_context::rhs);
    break;
  case expression_kind::jsx_element: {
    auto* element = static_cast<expression::jsx_element*>(ast);
    if (!element->is_intrinsic()) {
      v.visit_variable_use(element->tag);
    }
    visit_children();
    break;
  }
  case expression_kind::jsx_element_with_members: {
    auto* element = static_cast<expression::jsx_element_with_members*>(ast);
    QLJS_ASSERT(element->members.size() >= 1);
    v.visit_variable_use(element->members[0]);
    visit_children();
    break;
  }
  case expression_kind::object:
    for (int i = 0; i < ast->object_entry_count(); ++i) {
      auto entry = ast->object_entry(i);
      if (entry.property.has_value()) {
        this->visit_expression(*entry.property, v, variable_context::rhs);
      }
      this->visit_expression(entry.value, v, context);
    }
    break;
  case expression_kind::paren:
    this->visit_expression(ast->child_0(), v, context);
    break;
  case expression_kind::rw_unary_prefix:
  case expression_kind::rw_unary_suffix: {
    expression* child = ast->child_0();
    this->visit_expression(child, v, variable_context::rhs);
    this->maybe_visit_assignment(child, v);
    break;
  }
  case expression_kind::variable:
    switch (context) {
    case variable_context::lhs:
      break;
    case variable_context::rhs:
      if (ast->variable_identifier_token_type() ==
          token_type::reserved_keyword_with_escape_sequence) {
        v.visit_keyword_variable_use(ast->variable_identifier());
      } else {
        v.visit_variable_use(ast->variable_identifier());
      }
      break;
    }
    break;
  }
}

void parser::visit_assignment_expression(expression* lhs, expression* rhs,
                                         parse_visitor_base& v) {
  this->visit_expression(lhs, v, variable_context::lhs);
  this->visit_expression(rhs, v, variable_context::rhs);
  this->maybe_visit_assignment(lhs, v);
}

void parser::visit_compound_or_conditional_assignment_expression(
    expression* lhs, expression* rhs, parse_visitor_base& v) {
  this->visit_expression(lhs, v, variable_context::rhs);
  this->visit_expression(rhs, v, variable_context::rhs);
  this->maybe_visit_assignment(lhs, v);
}

void parser::maybe_visit_assignment(expression* ast, parse_visitor_base& v) {
  switch (ast->kind()) {
  case expression_kind::array:
    for (expression* child : ast->children()) {
      this->maybe_visit_assignment(child, v);
    }
    break;
  case expression_kind::object:
    for (int i = 0; i < ast->object_entry_count(); ++i) {
      expression* value = ast->object_entry(i).value;
      this->maybe_visit_assignment(value, v);
    }
    break;
  case expression_kind::paren:
    this->maybe_visit_assignment(ast->child_0(), v);
    break;
  case expression_kind::variable:
    v.visit_variable_assignment(ast->variable_identifier());
    break;
  default:
    break;
  }
}

expression* parser::parse_expression(parse_visitor_base& v, precedence prec) {
  depth_guard guard(this);
  expression* ast = this->parse_primary_expression(v, prec);
  if (!prec.binary_operators && prec.math_or_logical_or_assignment) {
    return ast;
  }
  return this->parse_expression_remainder(v, ast, prec);
}

// TODO(strager): Why do we need precedence here? Could we get rid of prec?
expression* parser::parse_primary_expression(parse_visitor_base& v,
                                             precedence prec) {
  switch (this->peek().type) {
  // f  // Variable name.
  identifier:
  case token_type::identifier:
  case token_type::kw_as:
  case token_type::kw_from:
  case token_type::kw_get:
  case token_type::kw_let:
  case token_type::kw_of:
  case token_type::kw_set:
  case token_type::kw_static: {
    expression* ast = this->make_expression<expression::variable>(
        this->peek().identifier_name(), this->peek().type);
    this->skip();
    return ast;
  }

  // \u{69}\u{66} // 'if', but escaped.
  case token_type::reserved_keyword_with_escape_sequence:
    this->lexer_.peek().report_errors_for_escape_sequences_in_keyword(
        this->diag_reporter_);
    goto identifier;

  // protected
  // implements
  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    goto identifier;

  // false
  // "hello"
  // 42
  case token_type::kw_false:
  case token_type::kw_null:
  case token_type::kw_this:
  case token_type::kw_true:
  case token_type::number:
  case token_type::string: {
    expression* ast =
        this->make_expression<expression::literal>(this->peek().span());
    this->skip();
    return ast;
  }

  // `hello`
  case token_type::complete_template: {
    this->peek().report_errors_for_escape_sequences_in_template(
        this->diag_reporter_);
    expression* ast =
        this->make_expression<expression::literal>(this->peek().span());
    this->skip();
    return ast;
  }

  // import.meta
  case token_type::kw_import: {
    expression* ast =
        this->make_expression<expression::import>(this->peek().span());
    this->skip();
    return ast;
  }

  // super()
  case token_type::kw_super: {
    expression* ast =
        this->make_expression<expression::super>(this->peek().span());
    this->skip();
    return ast;
  }

  // `hello${world}`
  case token_type::incomplete_template: {
    expression* ast = this->parse_untagged_template(v);
    return ast;
  }

  // await            // Identifier.
  // await myPromise
  case token_type::kw_await: {
    token await_token = this->peek();
    this->skip();
    return this->parse_await_expression(v, await_token, prec);
  }

  // yield       // Identifier.
  // yield       // Operator.
  // yield item
  case token_type::kw_yield: {
    if (this->in_generator_function_) {
      // yield is a unary operator.
      source_code_span operator_span = this->peek().span();
      this->skip();
      switch (this->peek().type) {
      case token_type::colon:
      case token_type::comma:
      case token_type::end_of_file:
      case token_type::kw_in:
      case token_type::question:
      case token_type::right_curly:
      case token_type::right_paren:
      case token_type::right_square:
      case token_type::semicolon:
        return this->make_expression<expression::yield_none>(operator_span);

      case token_type::star: {
        this->skip();
        expression* child = this->parse_expression(v, prec);
        return this->make_expression<expression::yield_many>(child,
                                                             operator_span);
      }

      default: {
        expression* child = this->parse_expression(v, prec);
        return this->make_expression<expression::yield_one>(child,
                                                            operator_span);
      }
      }
    } else {
      // yield is an identifier.
      goto identifier;
    }
  }

  // ...args  // Spread operator.
  case token_type::dot_dot_dot: {
    source_code_span operator_span = this->peek().span();
    this->skip();
    expression* child = this->parse_expression(v, precedence{.commas = false});
    return this->make_expression<expression::spread>(child, operator_span);
  }

  // !x
  // delete o[key]
  case token_type::bang:
  case token_type::kw_delete:
  case token_type::kw_typeof:
  case token_type::kw_void:
  case token_type::minus:
  case token_type::plus:
  case token_type::tilde: {
    token_type type = this->peek().type;
    source_code_span operator_span = this->peek().span();
    this->skip();
    expression* child =
        this->parse_expression(v, precedence{
                                      .binary_operators = true,
                                      .math_or_logical_or_assignment = false,
                                      .commas = false,
                                      .in_operator = prec.in_operator,
                                      .conditional_operator = false,
                                  });
    if (child->kind() == expression_kind::_missing) {
      this->diag_reporter_->report(diag_missing_operand_for_operator{
          .where = operator_span,
      });
    }
    expression* ast =
        type == token_type::kw_delete
            ? this->make_expression<expression::_delete>(child, operator_span)
            : type == token_type::kw_typeof
                  ? this->make_expression<expression::_typeof>(child,
                                                               operator_span)
                  : this->make_expression<expression::unary_operator>(
                        child, operator_span);
    return ast;
  }

  // --x
  case token_type::minus_minus:
  case token_type::plus_plus: {
    source_code_span operator_span = this->peek().span();
    this->skip();
    expression* child =
        this->parse_expression(v, precedence{
                                      .binary_operators = false,
                                      .math_or_logical_or_assignment = false,
                                      .commas = false,
                                      .in_operator = prec.in_operator,
                                      .conditional_operator = false,
                                  });
    if (child->kind() == expression_kind::_missing) {
      this->diag_reporter_->report(diag_missing_operand_for_operator{
          .where = operator_span,
      });
    }
    return this->make_expression<expression::rw_unary_prefix>(child,
                                                              operator_span);
  }

  // () => {}     // Arrow function.
  // (x) => {}    // Arrow function.
  // (x + y * z)  // Parenthesized expression.
  case token_type::left_paren: {
    source_code_span left_paren_span = this->peek().span();
    this->skip();

    if (this->peek().type == token_type::right_paren) {
      source_code_span right_paren_span = this->peek().span();
      this->skip();
      bool is_arrow_function = this->peek().type == token_type::equal_greater;
      bool is_arrow_function_without_arrow =
          prec.trailing_curly_is_arrow_body &&
          this->peek().type == token_type::left_curly;
      if (is_arrow_function || is_arrow_function_without_arrow) {
        // Arrow function: () => expression-or-block
        // Arrow function: () { }  // Invalid.
        if (is_arrow_function) {
          this->skip();
        } else {
          this->diag_reporter_->report(
              diag_missing_arrow_operator_in_arrow_function{
                  .where = this->peek().span()});
        }
        expression* ast = this->parse_arrow_function_body(
            v, function_attributes::normal, left_paren_span.begin(),
            /*allow_in_operator=*/prec.in_operator,
            expression_arena::array_ptr<expression*>());
        return ast;
      } else {
        // ()  // Invalid.
        this->diag_reporter_->report(
            diag_missing_expression_between_parentheses{
                .left_paren_to_right_paren = source_code_span(
                    left_paren_span.begin(), right_paren_span.end()),
                .left_paren = left_paren_span,
                .right_paren = right_paren_span,
            });
        return this->make_expression<expression::_invalid>(
            source_code_span(left_paren_span.begin(), right_paren_span.end()));
      }
    }

    expression* child =
        this->parse_expression(v, precedence{.trailing_identifiers = true});
    switch (this->peek().type) {
    case token_type::right_paren:
      this->skip();
      break;
    default:
      this->diag_reporter_->report(diag_unmatched_parenthesis{left_paren_span});
      break;
    }
    return this->make_expression<expression::paren>(
        source_code_span(left_paren_span.begin(),
                         this->lexer_.end_of_previous_token()),
        child);
  }

  // async           // Identifier.
  // async () => {}  // Arrow function.
  case token_type::kw_async: {
    token async_token = this->peek();
    this->skip();
    return this->parse_async_expression(v, async_token, prec);
  }

  // [x, 3, f()]  // Array literal.
  case token_type::left_square: {
    const char8* left_square_begin = this->peek().begin;
    const char8* right_square_end;
    this->skip();

    expression_arena::vector<expression*> children(
        "parse_expression array children", this->expressions_.allocator());
    for (;;) {
      if (this->peek().type == token_type::right_square) {
        right_square_end = this->peek().end;
        this->skip();
        break;
      }
      // TODO(strager): Require commas between expressions.
      if (this->peek().type == token_type::comma) {
        this->skip();
        continue;
      }
      const char8* child_begin = this->peek().begin;
      expression* child =
          this->parse_expression(v, precedence{.commas = false});
      if (this->peek().begin == child_begin) {
        // parse_expression parsed nothing.
        // TODO(strager): Should parse_expression return nullptr if it sees a
        // keyword (instead of returning _invalid and forcing us to check if it
        // parsed anything)?
        const char8* expected_right_square =
            this->lexer_.end_of_previous_token();
        this->diag_reporter_->report(diag_missing_array_close{
            .left_square =
                source_code_span(left_square_begin, left_square_begin + 1),
            .expected_right_square =
                source_code_span(expected_right_square, expected_right_square),
        });
        right_square_end = expected_right_square;
        break;
      }
      children.emplace_back(child);
    }
    expression* ast = this->make_expression<expression::array>(
        this->expressions_.make_array(std::move(children)),
        source_code_span(left_square_begin, right_square_end));
    return ast;
  }

  // {k: v}  // Object literal.
  case token_type::left_curly: {
    expression* ast = this->parse_object_literal(v);
    return ast;
  }

  // function() {}  // Function expression.
  case token_type::kw_function: {
    expression* function = this->parse_function_expression(
        v, function_attributes::normal, this->peek().begin);
    return function;
  }

  // class {}
  case token_type::kw_class: {
    expression* class_expression = this->parse_class_expression(v);
    return class_expression;
  }

  // new XMLHttpRequest()
  // new.target
  case token_type::kw_new: {
    source_code_span operator_span = this->peek().span();
    this->skip();

    switch (this->peek().type) {
    // new XMLHttpRequest()
    default: {
      expression* target = this->parse_expression(v, prec);
      expression_arena::vector<expression*> children(
          "parse_expression new children", this->expressions_.allocator());
      if (target->kind() == expression_kind::call) {
        for (expression* child : target->children()) {
          children.emplace_back(child);
        }
      } else {
        children.emplace_back(target);
      }
      return this->make_expression<expression::_new>(
          this->expressions_.make_array(std::move(children)),
          source_code_span(operator_span.begin(), target->span().end()));
    }

    // new.target
    case token_type::dot: {
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
      // TODO(strager): Check that the given identifier is 'target'.
      // * Are \u{} escapes allowed?
      source_code_span target_span = this->peek().identifier_name().span();
      this->skip();
      expression* ast = this->make_expression<expression::new_target>(
          source_code_span(operator_span.begin(), target_span.end()));
      return ast;
    }
    }
    QLJS_UNREACHABLE();
  }

  // /regexp/    // RegExp literal.
  // /=regexp/  // RegExp literal.
  case token_type::slash:
  case token_type::slash_equal: {
    this->lexer_.reparse_as_regexp();
    expression* regexp =
        this->make_expression<expression::literal>(this->peek().span());
    this->skip();
    return regexp;
  }

  QLJS_CASE_BINARY_ONLY_OPERATOR:
  QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL:
  case token_type::comma:
  case token_type::dot:
  case token_type::equal:
  case token_type::kw_in:
  case token_type::question: {
    if (this->peek().type == token_type::star) {
      token star_token = this->peek();
      std::optional<function_attributes> attributes =
          this->try_parse_function_with_leading_star();
      if (attributes.has_value()) {
        expression* function = this->parse_function_expression(
            v, attributes.value(), star_token.begin);
        return function;
      }
    }
    expression* ast =
        this->make_expression<expression::_missing>(this->peek().span());
    if (prec.binary_operators) {
      this->diag_reporter_->report(
          diag_missing_operand_for_operator{this->peek().span()});
    }
    return ast;
  }

  // <MyComponent /> (JSX)
  case token_type::less:
    return this->parse_jsx_expression(v);

  // => expr  // Invalid. Treat as arrow function.
  // => {}    // Invalid. Treat as arrow function.
  case token_type::equal_greater: {
    source_code_span arrow_span = this->peek().span();
    this->diag_reporter_->report(diag_missing_arrow_function_parameter_list{
        .arrow = arrow_span,
    });
    this->skip();

    expression* arrow_function = this->parse_arrow_function_body(
        v, function_attributes::normal,
        /*parameter_list_begin=*/arrow_span.begin(),
        /*allow_in_operator=*/prec.in_operator,
        expression_arena::array_ptr<expression*>());
    return arrow_function;
  }

  case token_type::private_identifier: {
    this->diag_reporter_->report(
        diag_cannot_refer_to_private_variable_without_object{
            .private_identifier = this->peek().identifier_name(),
        });
    expression* ast = this->make_expression<expression::private_variable>(
        this->peek().identifier_name());
    this->skip();
    return ast;
  }

  case token_type::colon:
  case token_type::kw_debugger: {
    source_code_span token_span = this->peek().span();
    this->diag_reporter_->report(diag_unexpected_token{token_span});
    this->skip();
    return this->make_expression<expression::_invalid>(token_span);
  }

  case token_type::end_of_file:
  case token_type::kw_break:
  case token_type::kw_case:
  case token_type::kw_catch:
  case token_type::kw_const:
  case token_type::kw_continue:
  case token_type::kw_default:
  case token_type::kw_do:
  case token_type::kw_else:
  case token_type::kw_enum:
  case token_type::kw_export:
  case token_type::kw_extends:
  case token_type::kw_finally:
  case token_type::kw_for:
  case token_type::kw_if:
  case token_type::kw_return:
  case token_type::kw_switch:
  case token_type::kw_throw:
  case token_type::kw_try:
  case token_type::kw_var:
  case token_type::kw_while:
  case token_type::kw_with:
  case token_type::right_curly:
  case token_type::right_paren:
  case token_type::right_square:
  case token_type::semicolon:
    return this->make_expression<expression::_missing>(this->peek().span());

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

expression* parser::parse_async_expression(parse_visitor_base& v,
                                           const token& async_token,
                                           precedence prec) {
  expression* ast = this->parse_async_expression_only(
      v, async_token, /*allow_in_operator=*/prec.in_operator);
  if (!prec.binary_operators) {
    return ast;
  }
  return this->parse_expression_remainder(v, ast, prec);
}

expression* parser::parse_async_expression_only(parse_visitor_base& v,
                                                const token& async_token,
                                                bool allow_in_operator) {
  const char8* async_begin = async_token.begin;

  auto parse_arrow_function_arrow_and_body =
      [this, allow_in_operator, async_begin, &v](auto&& parameters) {
        if (this->peek().type == token_type::equal_greater) {
          this->skip();
        } else {
          this->diag_reporter_->report(
              diag_missing_arrow_operator_in_arrow_function{
                  .where = this->peek().span(),
              });
        }

        expression* ast = this->parse_arrow_function_body(
            v, function_attributes::async, async_begin,
            /*allow_in_operator=*/allow_in_operator,
            this->expressions_.make_array(std::move(parameters)));
        return ast;
      };

  switch (this->peek().type) {
  // async () => {}  // Arrow function.
  // async()         // Function call.
  case token_type::left_paren: {
    bool newline_after_async = this->peek().has_leading_newline;

    expression_arena::vector<expression*> parameters(
        "parse_expression async arrow function parameters",
        this->expressions_.allocator());
    source_code_span left_paren_span = this->peek().span();
    this->skip();

    while (this->peek().type != token_type::right_paren) {
      if (this->peek().type == token_type::comma) {
        // TODO(strager): Emit a different error if this is an arrow function.
        // diag_extra_comma_not_allowed_between_arguments only makes sense if
        // this is a function call.
        this->diag_reporter_->report(
            diag_extra_comma_not_allowed_between_arguments{
                .comma = this->peek().span(),
            });
        this->skip();
        continue;
      }
      parameters.emplace_back(
          this->parse_expression(v, precedence{.commas = false}));
      if (this->peek().type != token_type::comma) {
        break;
      }
      this->skip();
    }

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
    source_code_span right_paren_span = this->peek().span();
    this->skip();

    bool is_arrow_function = this->peek().type == token_type::equal_greater;
    bool is_arrow_function_without_arrow =
        !this->peek().has_leading_newline &&
        this->peek().type == token_type::left_curly;
    if (is_arrow_function || is_arrow_function_without_arrow) {
      if (newline_after_async) {
        this->diag_reporter_->report(
            diag_newline_not_allowed_between_async_and_parameter_list{
                .async = async_token.span(),
                .arrow = this->peek().span(),
            });
      }
      for (auto* parameter : parameters) {
        if (parameter->kind() == expression_kind::variable &&
            parameter->variable_identifier_token_type() ==
                token_type::kw_await) {
          // async (await) => {}  // Invalid
          this->diag_reporter_->report(
              diag_cannot_declare_await_in_async_function{
                  .name = parameter->variable_identifier(),
              });
        }
      }
      // TODO(strager): Should we call maybe_wrap_erroneous_arrow_function?
      return parse_arrow_function_arrow_and_body(std::move(parameters));
    } else {
      // async as an identifier (variable reference)
      // Function call: async(arg)
      // TODO(strager): Reduce copying of the arguments.
      expression_arena::vector<expression*> call_children(
          "parse_expression async call children",
          this->expressions_.allocator());
      call_children.emplace_back(this->make_expression<expression::variable>(
          async_token.identifier_name(), async_token.type));
      for (std::size_t i = 0; i < parameters.size(); ++i) {
        if (parameters.data()[i]->kind() != expression_kind::_invalid) {
          call_children.emplace_back(parameters.data()[i]);
        }
      }

      expression* call_ast = this->make_expression<expression::call>(
          this->expressions_.make_array(std::move(call_children)),
          /*left_paren_span=*/left_paren_span,
          /*span_end=*/right_paren_span.end());
      return call_ast;
    }

    QLJS_UNREACHABLE();
  }

  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow parameters named 'protected', 'implements', etc. in
    // strict mode.
    [[fallthrough]];
  // async parameter => expression-or-block  // Arrow function.
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier:
  case token_type::kw_await:
  case token_type::kw_yield: {
    if (this->peek().has_leading_newline) {
      goto variable_reference;
    }

    if (this->peek().type == token_type::kw_await) {
      // async await => {}  // Invalid
      this->diag_reporter_->report(diag_cannot_declare_await_in_async_function{
          .name = this->peek().identifier_name(),
      });
    }
    std::array<expression*, 1> parameters = {
        this->make_expression<expression::variable>(
            identifier(this->peek().span()), this->peek().type)};
    this->skip();
    return parse_arrow_function_arrow_and_body(std::move(parameters));
  }

  // async function f(parameters) { statements; }
  case token_type::kw_function: {
    expression* function = this->parse_function_expression(
        v, function_attributes::async, async_begin);
    return function;
  }

  // async  // Identifier (variable reference).
  variable_reference:
  default: {
    expression* ast = this->make_expression<expression::variable>(
        async_token.identifier_name(), async_token.type);
    return ast;
  }
  }

  QLJS_UNREACHABLE();
}

expression* parser::parse_await_expression(parse_visitor_base& v,
                                           const token& await_token,
                                           precedence prec) {
  bool is_identifier = [&]() -> bool {
    if (this->in_async_function_ ||
        (this->in_top_level_ &&
         this->options_.top_level_await_mode ==
             parser_top_level_await_mode::await_operator)) {
      return false;
    } else {
      // await is a unary operator (in modules) or an identifier (in scripts).
      switch (this->peek().type) {
      QLJS_CASE_BINARY_ONLY_OPERATOR:
      QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL:
      QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
      case token_type::colon:
      case token_type::comma:
      case token_type::dot:
      case token_type::end_of_file:
      case token_type::equal:
      case token_type::equal_greater:
      case token_type::kw_in:
      case token_type::question:
      case token_type::question_dot:
      case token_type::right_curly:
      case token_type::right_paren:
      case token_type::right_square:
      case token_type::semicolon:
        return true;

      // await <x>y</x>/g;  // operator
      // await < other;     // identifier
      // await /regexp/;    // operator
      // await / rhs;       // identifier
      case token_type::less:
      case token_type::slash:
      case token_type::slash_equal: {
        parse_expression_cache_key cache_key = {
            .begin = this->peek().begin,
            .in_top_level = this->in_top_level_,
            .in_async_function = this->in_async_function_,
            .in_generator_function = this->in_generator_function_,
            .in_loop_statement = this->in_loop_statement_,
            .in_switch_statement = this->in_switch_statement_,
            .in_class = this->in_class_,
        };
        auto cache_it = this->await_is_identifier_cache_.find(cache_key);
        if (cache_it != this->await_is_identifier_cache_.end()) {
          return cache_it->second;
        }

        parser_transaction transaction = this->begin_transaction();

        if (this->in_top_level_) {
          // Try to parse the / as a regular expression literal or the < as a
          // JSX element.
          [[maybe_unused]] expression* ast = this->parse_expression(v, prec);
        } else {
          // Try to parse the / or < as a binary operator.
          [[maybe_unused]] expression* ast = this->parse_expression_remainder(
              v,
              this->make_expression<expression::variable>(
                  await_token.identifier_name(), await_token.type),
              prec);
        }
        bool parsed_ok = transaction.reporter.empty() &&
                         !this->lexer_.transaction_has_lex_diagnostics(
                             transaction.lex_transaction);

        this->roll_back_transaction(std::move(transaction));

        bool is_identifier_result;
        if (this->in_top_level_) {
          is_identifier_result = !parsed_ok;
        } else {
          is_identifier_result = parsed_ok;
        }
        auto [_cache_it, inserted] =
            this->await_is_identifier_cache_.try_emplace(cache_key,
                                                         is_identifier_result);
        QLJS_ASSERT(inserted);
        return is_identifier_result;
      }

      case token_type::kw_of:
        // HACK(strager): This works around for-of parsing. Remove this case
        // when for-of parsing is fixed.
        [[fallthrough]];
      case token_type::minus_minus:
      case token_type::plus_plus:
        // TODO(strager): Parse 'await--x' and 'await--;' correctly.
        [[fallthrough]];
      case token_type::complete_template:
      case token_type::incomplete_template:
      case token_type::left_paren:
      case token_type::left_square:
      case token_type::minus:
      case token_type::plus:
        return !this->in_top_level_;

      case token_type::bang:
      case token_type::dot_dot_dot:
      case token_type::identifier:
      case token_type::kw_as:
      case token_type::kw_async:
      case token_type::kw_await:
      case token_type::kw_from:
      case token_type::kw_function:
      case token_type::kw_get:
      case token_type::kw_let:
      case token_type::kw_set:
      case token_type::kw_static:
      case token_type::kw_yield:
      case token_type::left_curly:
      case token_type::number:
      case token_type::private_identifier:
      case token_type::regexp:
      case token_type::reserved_keyword_with_escape_sequence:
      case token_type::string:
      case token_type::tilde:
      default:
        return false;
      }
    }
  }();

  if (is_identifier) {
    return this->make_expression<expression::variable>(
        await_token.identifier_name(), await_token.type);
  } else {
    source_code_span operator_span = await_token.span();

    parser_transaction transaction = this->begin_transaction();

    expression* child = this->parse_expression(v, prec);

    if (child->kind() == expression_kind::_missing) {
      this->diag_reporter_->report(diag_missing_operand_for_operator{
          .where = operator_span,
      });
    } else if (child->kind() == expression_kind::arrow_function &&
               child->attributes() != function_attributes::async) {
      // await (param) => { }  // Invalid.
      this->roll_back_transaction(std::move(transaction));
      this->diag_reporter_->report(diag_await_followed_by_arrow_function{
          .await_operator = operator_span,
      });
      // Re-parse as if the user wrote 'async' instead of 'await'.
      return this->parse_async_expression(v, await_token, prec);
    }

    if (!(this->in_async_function_ || this->in_top_level_)) {
      this->diag_reporter_->report(diag_await_operator_outside_async{
          .await_operator = operator_span,
      });
    }

    this->commit_transaction(std::move(transaction));
    return this->make_expression<expression::await>(child, operator_span);
  }
}

expression* parser::parse_expression_remainder(parse_visitor_base& v,
                                               expression* ast,
                                               precedence prec) {
  if (prec.commas) {
    QLJS_ASSERT(prec.binary_operators);
  }

  binary_expression_builder binary_builder(this->expressions_.allocator(), ast);
  auto build_expression = [&]() {
    if (binary_builder.has_multiple_children()) {
      return this->make_expression<expression::binary_operator>(
          binary_builder.move_expressions(this->expressions_),
          binary_builder.move_operator_spans(this->expressions_));
    } else {
      return binary_builder.last_expression();
    }
  };

next:
  switch (this->peek().type) {
  // x, y, z  // Sequence operator or parameter separator.
  case token_type::comma: {
    if (!prec.commas) {
      break;
    }
    source_code_span comma_span = this->peek().span();
    this->skip();

    if (this->peek().type == token_type::right_paren) {
      // Probably an arrow function with a trailing comma in its parameter list:
      // (parameters, go, here, ) => expression-or-block
      return this->make_expression<expression::trailing_comma>(
          binary_builder.move_expressions(this->expressions_), comma_span);
    } else {
      // Comma expression: a, b, c
      expression* rhs = binary_builder.add_child(
          comma_span, this->parse_expression(v, precedence{.commas = false}));
      if (rhs->kind() == expression_kind::_invalid) {
        this->diag_reporter_->report(
            diag_missing_operand_for_operator{comma_span});
      }
    }
    goto next;
  }

  // x + y
  QLJS_CASE_BINARY_ONLY_OPERATOR:
  case token_type::less:
  case token_type::minus:
  case token_type::plus:
  case token_type::slash: {
    if (!prec.math_or_logical_or_assignment) {
      break;
    }
    bool allow_unary_lhs = this->peek().type != token_type::star_star;
    expression* maybe_unary_lhs = binary_builder.last_expression();
    expression_kind lhs_kind = maybe_unary_lhs->kind();
    source_code_span operator_span = this->peek().span();
    this->skip();

    expression* rhs = binary_builder.add_child(
        operator_span,
        this->parse_expression(
            v, precedence{.binary_operators = false, .commas = false}));

    if (!allow_unary_lhs) {
      switch (lhs_kind) {
      // -a ** b  // Invalid.
      // void a ** b  // Invalid.
      case expression_kind::unary_operator: {
        auto* lhs = static_cast<expression::unary_operator*>(maybe_unary_lhs);
        // HACK(strager): Should we create expression::_void?
        if (lhs->unary_operator_begin_[0] == u8'v') {
          // void a ** b  // Invalid.
          this->diag_reporter_->report(
              diag_missing_parentheses_around_exponent_with_unary_lhs{
                  .exponent_expression = source_code_span(
                      lhs->child_->span().begin(), rhs->span().end()),
                  .unary_operator = source_code_span(
                      lhs->unary_operator_begin_,
                      lhs->unary_operator_begin_ + strlen(u8"void")),
              });
        } else {
          // -a ** b  // Invalid.
          // ~a ** b  // Invalid.
          this->diag_reporter_->report(
              diag_missing_parentheses_around_unary_lhs_of_exponent{
                  .unary_expression = maybe_unary_lhs->span(),
                  .exponent_operator = operator_span,
              });
        }
        break;
      }

      // delete a ** b  // Invalid.
      case expression_kind::_delete: {
        auto* lhs = static_cast<expression::_delete*>(maybe_unary_lhs);
        this->diag_reporter_->report(
            diag_missing_parentheses_around_exponent_with_unary_lhs{
                .exponent_expression = source_code_span(
                    lhs->child_->span().begin(), rhs->span().end()),
                .unary_operator = lhs->unary_operator_span(),
            });
        break;
      }

      // typeof a ** b  // Invalid.
      case expression_kind::_typeof: {
        auto* lhs = static_cast<expression::_typeof*>(maybe_unary_lhs);
        this->diag_reporter_->report(
            diag_missing_parentheses_around_exponent_with_unary_lhs{
                .exponent_expression = source_code_span(
                    lhs->child_->span().begin(), rhs->span().end()),
                .unary_operator = lhs->unary_operator_span(),
            });
        break;
      }

      default:
        break;
      }
    }
    if (rhs->kind() == expression_kind::_missing) {
      this->diag_reporter_->report(
          diag_missing_operand_for_operator{operator_span});
    }
    goto next;
  }

  // f(x, y, z)  // Function call.
  case token_type::left_paren:
    if (binary_builder.last_expression()->kind() ==
        expression_kind::arrow_function) {
      // () => {}() // Invalid.
      auto func_span = binary_builder.last_expression()->span();
      auto func_start_span =
          source_code_span(func_span.begin(), func_span.begin());
      this->diag_reporter_->report(
          diag_missing_parentheses_around_self_invoked_function{
              .invocation = this->peek().span(),
              .func_start = func_start_span});
    }
    binary_builder.replace_last(this->parse_call_expression_remainder(
        v, binary_builder.last_expression()));
    goto next;

  // x += y
  // f().prop = other
  // x[y] &&= z
  QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
  QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
  case token_type::equal: {
    if (!prec.math_or_logical_or_assignment) {
      break;
    }
    expression_kind kind;
    switch (this->peek().type) {
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
      kind = expression_kind::compound_assignment;
      break;
    QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
      kind = expression_kind::conditional_assignment;
      break;
    case token_type::equal:
      kind = expression_kind::assignment;
      break;
    default:
      QLJS_UNREACHABLE();
    }
    source_code_span operator_span = this->peek().span();
    this->skip();
    expression* lhs = build_expression();
    switch (lhs->without_paren()->kind()) {
    default:
      this->diag_reporter_->report(
          diag_invalid_expression_left_of_assignment{lhs->span()});
      break;
    case expression_kind::_invalid:
    case expression_kind::_missing:
      // An error should have been reported elsewhere.
      break;
    case expression_kind::array:
    case expression_kind::dot:
    case expression_kind::index:
    case expression_kind::object:
    case expression_kind::variable:
    case expression_kind::private_variable:
      break;
    case expression_kind::paren:
      QLJS_UNREACHABLE();
    }
    expression* rhs = this->parse_expression(
        v, precedence{.commas = false, .in_operator = prec.in_operator});
    if (rhs->kind() == expression_kind::_missing) {
      this->diag_reporter_->report(diag_missing_operand_for_operator{
          .where = operator_span,
      });
    }
    binary_builder.reset_after_build(
        this->make_expression<expression::assignment>(kind, lhs, rhs,
                                                      operator_span));
    goto next;
  }

  // x.y
  case token_type::dot: {
    source_code_span dot_span = this->peek().span();
    this->skip();
    switch (this->peek().type) {
    case token_type::identifier:
    case token_type::private_identifier:
    case token_type::reserved_keyword_with_escape_sequence:
    QLJS_CASE_KEYWORD:
      if (this->peek().type == token_type::private_identifier &&
          !this->in_class_) {
        this->diag_reporter_->report(
            diag_cannot_access_private_identifier_outside_class{
                .private_identifier = this->peek().identifier_name(),
            });
      }
      binary_builder.replace_last(this->make_expression<expression::dot>(
          binary_builder.last_expression(), this->peek().identifier_name()));
      this->skip();
      goto next;

    case token_type::string: {
      this->diag_reporter_->report(diag_invalid_rhs_for_dot_operator{
          .dot = dot_span,
      });
      binary_builder.add_child(
          dot_span,
          this->make_expression<expression::literal>(this->peek().span()));
      this->skip();
      goto next;
    }

    QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL:
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
    QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
    case token_type::colon:
    case token_type::comma:
    case token_type::end_of_file:
    case token_type::equal:
    case token_type::left_paren:
    case token_type::less:
    case token_type::minus:
    case token_type::plus:
    case token_type::question:
    case token_type::right_paren:
    case token_type::semicolon: {
      source_code_span empty_property_name(dot_span.end(), dot_span.end());
      binary_builder.replace_last(this->make_expression<expression::dot>(
          binary_builder.last_expression(), identifier(empty_property_name)));
      this->diag_reporter_->report(diag_missing_property_name_for_dot_operator{
          .dot = dot_span,
      });
      goto next;
    }

    // x .. y
    case token_type::dot: {
      source_code_span second_dot = this->peek().span();
      this->diag_reporter_->report(diag_dot_dot_is_not_an_operator{
          .dots = source_code_span(dot_span.begin(), second_dot.end()),
      });
      // Treat '..' as if it was a binary operator.
      this->skip();
      binary_builder.add_child(
          second_dot,
          this->parse_expression(
              v, precedence{.binary_operators = false, .commas = false}));
      goto next;
    }

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    break;
  }

  // x?.y
  // x?.#y
  // array?.[index]
  // f?.(x, y)
  case token_type::question_dot: {
    this->skip();
    switch (this->peek().type) {
    // x?.y
    case token_type::identifier:
    case token_type::private_identifier:
    case token_type::reserved_keyword_with_escape_sequence:
    QLJS_CASE_KEYWORD:
      binary_builder.replace_last(this->make_expression<expression::dot>(
          binary_builder.last_expression(), this->peek().identifier_name()));
      this->skip();
      goto next;

    // tag?.`template`
    // tag?.`template${goes}here`
    case token_type::complete_template:
    case token_type::incomplete_template:
      binary_builder.replace_last(
          this->parse_tagged_template(v, binary_builder.last_expression()));
      goto next;

    // f?.(x, y)
    case token_type::left_paren:
      binary_builder.replace_last(this->parse_call_expression_remainder(
          v, binary_builder.last_expression()));
      goto next;

    // array?.[index]
    case token_type::left_square:
      binary_builder.replace_last(this->parse_index_expression_remainder(
          v, binary_builder.last_expression()));
      goto next;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    break;
  }

  // o[key]  // Indexing expression.
  case token_type::left_square: {
    binary_builder.replace_last(this->parse_index_expression_remainder(
        v, binary_builder.last_expression()));
    goto next;
  }

  // x++  // Suffix unary operator.
  case token_type::minus_minus:
  case token_type::plus_plus:
    if (this->peek().has_leading_newline) {
      // Newline is not allowed before suffix ++ or --. Let
      // parse_and_visit_statement insert a semicolon for us.
      break;
    } else {
      source_code_span operator_span = this->peek().span();
      this->skip();
      binary_builder.replace_last(
          this->make_expression<expression::rw_unary_suffix>(
              binary_builder.last_expression(), operator_span));
    }
    goto next;

  // key in o
  case token_type::kw_in: {
    if (!prec.in_operator) {
      break;
    }
    source_code_span in_operator = this->peek().span();
    this->skip();
    binary_builder.add_child(in_operator, this->parse_expression(v, prec));
    goto next;
  }

  // x ? y : z  // Conditional operator.
  case token_type::question: {
    if (!prec.conditional_operator) {
      break;
    }
    source_code_span question_span = this->peek().span();
    this->skip();

    expression* condition = build_expression();

    expression* true_expression;
    if (this->peek().type == token_type::colon) {
      this->diag_reporter_->report(diag_missing_operand_for_operator{
          .where = question_span,
      });
      true_expression =
          this->make_expression<expression::_missing>(source_code_span(
              this->lexer_.end_of_previous_token(), this->peek().begin));
    } else {
      true_expression = this->parse_expression(v);
    }

    if (this->peek().type != token_type::colon) {
      source_code_span expected_colon(this->lexer_.end_of_previous_token(),
                                      this->lexer_.end_of_previous_token());
      this->diag_reporter_->report(diag_missing_colon_in_conditional_expression{
          .expected_colon = expected_colon,
          .question = question_span,
      });
      expression* false_expression =
          this->make_expression<expression::_missing>(expected_colon);
      return this->make_expression<expression::conditional>(
          condition, true_expression, false_expression);
    }
    source_code_span colon_span = this->peek().span();
    this->skip();

    expression* false_expression = this->parse_expression(v, prec);
    if (false_expression->kind() == expression_kind::_missing) {
      this->diag_reporter_->report(diag_missing_operand_for_operator{
          .where = colon_span,
      });
    }

    return this->make_expression<expression::conditional>(
        condition, true_expression, false_expression);
  }

  // (parameters, go, here) => expression-or-block // Arrow function.
  case token_type::equal_greater: {
    this->parse_arrow_function_expression_remainder(
        v, binary_builder, /*allow_in_operator=*/prec.in_operator);
    goto next;
  }

  // html`<h1>My Website</h1>  // Template call.
  // html`<h1>${title}</h1>`   // Template call.
  case token_type::complete_template:
  case token_type::incomplete_template: {
    expression* tag = binary_builder.last_expression();
    binary_builder.replace_last(this->parse_tagged_template(v, tag));
    goto next;
  }

  // x y    // Invalid.
  //
  // x    // ASI.
  // y
  case token_type::identifier:
    if (prec.trailing_identifiers) {
      const char8* expected_operator = this->lexer_.end_of_previous_token();
      this->diag_reporter_->report(diag_unexpected_identifier_in_expression{
          .unexpected = this->peek().identifier_name(),
      });

      // Behave as if a comma appeared before the identifier.
      expression* rhs = binary_builder.add_child(
          source_code_span(expected_operator, expected_operator),
          this->parse_expression(
              v, precedence{.binary_operators = false, .commas = false}));
      QLJS_ASSERT(rhs->kind() != expression_kind::_invalid);
      goto next;
    }
    break;

  case token_type::left_curly: {
    bool looks_like_arrow_function_body =
        prec.trailing_curly_is_arrow_body &&
        !this->peek().has_leading_newline &&
        !binary_builder.has_multiple_children() &&
        // TODO(strager): Check for ',' operator explicitly, not any binary
        // operator.
        binary_builder.last_expression()->without_paren()->kind() ==
            expression_kind::binary_operator;
    if (looks_like_arrow_function_body) {
      source_code_span arrow_span = this->peek().span();
      // (a, b) { return a + b; }  // Invalid.
      this->diag_reporter_->report(
          diag_missing_arrow_operator_in_arrow_function{
              .where = arrow_span,
          });
      this->parse_arrow_function_expression_remainder(
          v, /*arrow_span=*/arrow_span, binary_builder,
          /*allow_in_operator=*/prec.in_operator);
    }
    break;
  }

  case token_type::bang:
  case token_type::colon:
  case token_type::end_of_file:
  case token_type::kw_as:
  case token_type::kw_async:
  case token_type::kw_await:
  case token_type::kw_break:
  case token_type::kw_case:
  case token_type::kw_class:
  case token_type::kw_const:
  case token_type::kw_continue:
  case token_type::kw_debugger:
  case token_type::kw_default:
  case token_type::kw_delete:
  case token_type::kw_do:
  case token_type::kw_else:
  case token_type::kw_enum:
  case token_type::kw_export:
  case token_type::kw_false:
  case token_type::kw_for:
  case token_type::kw_from:
  case token_type::kw_function:
  case token_type::kw_get:
  case token_type::kw_if:
  case token_type::kw_implements:
  case token_type::kw_import:
  case token_type::kw_interface:
  case token_type::kw_let:
  case token_type::kw_new:
  case token_type::kw_null:
  case token_type::kw_of:
  case token_type::kw_package:
  case token_type::kw_private:
  case token_type::kw_protected:
  case token_type::kw_public:
  case token_type::kw_return:
  case token_type::kw_set:
  case token_type::kw_static:
  case token_type::kw_super:
  case token_type::kw_switch:
  case token_type::kw_this:
  case token_type::kw_throw:
  case token_type::kw_true:
  case token_type::kw_try:
  case token_type::kw_typeof:
  case token_type::kw_var:
  case token_type::kw_void:
  case token_type::kw_while:
  case token_type::kw_with:
  case token_type::kw_yield:
  case token_type::number:
  case token_type::private_identifier:
  case token_type::right_curly:
  case token_type::right_paren:
  case token_type::right_square:
  case token_type::semicolon:
  case token_type::string:
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  return build_expression();
}

void parser::parse_arrow_function_expression_remainder(
    parse_visitor_base& v, binary_expression_builder& children,
    bool allow_in_operator) {
  QLJS_ASSERT(this->peek().type == token_type::equal_greater);
  source_code_span arrow_span = this->peek().span();
  this->skip();
  this->parse_arrow_function_expression_remainder(v, arrow_span, children,
                                                  allow_in_operator);
}

void parser::parse_arrow_function_expression_remainder(
    parse_visitor_base& v, source_code_span arrow_span,
    binary_expression_builder& binary_builder, bool allow_in_operator) {
  if (binary_builder.has_multiple_children()) {
    // TODO(strager): We should report an error for code like this:
    // a + b => c
  }
  expression* lhs = binary_builder.last_expression();
  function_attributes attributes = function_attributes::normal;

  if (lhs->kind() == expression_kind::paren) {
    lhs = static_cast<expression::paren*>(lhs)->child_;
  }

  expression_arena::vector<expression*> parameters(
      "parse_arrow_function_expression_remainder",
      this->expressions_.allocator());
  const char8* left_paren_begin = nullptr;
  switch (lhs->kind()) {
  case expression_kind::binary_operator:
  case expression_kind::trailing_comma:
    // TODO(strager): Only allow comma expressions, not '(2+3) => 5', for
    // example.
    for (expression* parameter : lhs->children()) {
      parameters.emplace_back(parameter);
    }
    break;

  case expression_kind::_class:
  case expression_kind::_delete:
  case expression_kind::_invalid:
  case expression_kind::_missing:
  case expression_kind::_new:
  case expression_kind::_template:
  case expression_kind::_typeof:
  case expression_kind::arrow_function:
  case expression_kind::await:
  case expression_kind::compound_assignment:
  case expression_kind::conditional:
  case expression_kind::conditional_assignment:
  case expression_kind::function:
  case expression_kind::index:
  case expression_kind::jsx_element:
  case expression_kind::jsx_element_with_members:
  case expression_kind::jsx_element_with_namespace:
  case expression_kind::jsx_fragment:
  case expression_kind::named_function:
  case expression_kind::new_target:
  case expression_kind::private_variable:
  case expression_kind::rw_unary_prefix:
  case expression_kind::rw_unary_suffix:
  case expression_kind::super:
  case expression_kind::tagged_template_literal:
  case expression_kind::unary_operator:
  case expression_kind::yield_many:
  case expression_kind::yield_none:
  case expression_kind::yield_one:
    // The code is invalid. An error is reported elsewhere.
    [[fallthrough]];
  case expression_kind::array:
  case expression_kind::assignment:
  case expression_kind::object:
  case expression_kind::spread:
  case expression_kind::variable:
    parameters.emplace_back(lhs);
    break;

  // ((x)) => {}  // Invalid.
  case expression_kind::paren:
    // TODO(strager): Report an error.
    break;

  // f(x, y) => {}
  case expression_kind::call: {
    auto* call = expression_cast<expression::call>(lhs);
    // FIXME(strager): This check is duplicated.
    bool is_async_arrow_using_with_await_operator =
        call->child_0()->kind() == expression_kind::variable &&
        call->child_0()->variable_identifier_token_type() ==
            token_type::kw_await;
    if (this->peek().type == token_type::left_curly ||
        is_async_arrow_using_with_await_operator) {
      if (is_async_arrow_using_with_await_operator) {
        // await (x) => {}   // Invalid.
        // await () => expr  // Invalid.
        this->diag_reporter_->report(diag_await_followed_by_arrow_function{
            .await_operator = call->child_0()->span(),
        });
        // Parse as if the user wrote 'async' instead of 'await'.
        attributes = function_attributes::async;
      } else {
        // f() => {}         // Invalid.
      }
      left_paren_begin = call->left_paren_span().begin();
      for (int i = 1; i < call->child_count(); ++i) {
        parameters.emplace_back(call->child(i));
      }
      // We will report
      // diag_missing_operator_between_expression_and_arrow_function
      // elsewhere.
      break;
    }
    [[fallthrough]];
  }

  // f.x => z  // Invalid.
  // 42 => {}  // Invalid.
  case expression_kind::dot:
  case expression_kind::literal: {
    source_code_span lhs_span = lhs->span();
    left_paren_begin = lhs_span.begin();
    switch (lhs->kind()) {
    case expression_kind::call:
    case expression_kind::dot:
      this->diag_reporter_->report(diag_unexpected_arrow_after_expression{
          .arrow = arrow_span,
          .expression = lhs_span,
      });
      break;
    case expression_kind::literal:
      this->diag_reporter_->report(diag_unexpected_arrow_after_literal{
          .arrow = arrow_span,
          .literal_parameter = lhs_span,
      });
      break;
    default:
      QLJS_UNREACHABLE();
    }

    if (this->peek().type != token_type::left_curly) {
      // Treat the '=>' as if it was a binary operator (like '>=').
      binary_builder.add_child(
          arrow_span,
          this->parse_expression(
              v, precedence{.binary_operators = false, .commas = false}));
      return;
    }
    break;
  }

  case expression_kind::import:
    QLJS_UNIMPLEMENTED();
    break;
  }

  expression* arrow_function = this->parse_arrow_function_body(
      v, /*attributes=*/attributes,
      /*parameter_list_begin=*/left_paren_begin,
      /*allow_in_operator=*/allow_in_operator,
      this->expressions_.make_array(std::move(parameters)));
  binary_builder.replace_last(
      this->maybe_wrap_erroneous_arrow_function(arrow_function, /*lhs=*/lhs));
}

expression* parser::parse_call_expression_remainder(parse_visitor_base& v,
                                                    expression* callee) {
  source_code_span left_paren_span = this->peek().span();
  expression_arena::vector<expression*> call_children(
      "parse_expression_remainder call children",
      this->expressions_.allocator());
  call_children.reserve(4);
  call_children.emplace_back(callee);
  this->skip();
  while (this->peek().type != token_type::right_paren) {
    if (this->peek().type == token_type::comma) {
      this->diag_reporter_->report(
          diag_extra_comma_not_allowed_between_arguments{
              .comma = this->peek().span(),
          });
      this->skip();
      continue;
    }
    call_children.emplace_back(this->parse_expression(
        v, precedence{.commas = false, .trailing_identifiers = true}));
    if (this->peek().type != token_type::comma) {
      break;
    }
    this->skip();
  }
  const char8* call_span_end;
  if (this->peek().type == token_type::right_paren) {
    call_span_end = this->peek().end;
    this->skip();
  } else {
    // { f(x }  // Invalid.
    // f(x;     // Invalid.
    call_span_end = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(diag_expected_right_paren_for_function_call{
        .expected_right_paren = source_code_span(call_span_end, call_span_end),
        .left_paren = left_paren_span,
    });
  }
  return this->make_expression<expression::call>(
      this->expressions_.make_array(std::move(call_children)),
      /*left_paren_span=*/left_paren_span,
      /*span_end=*/call_span_end);
}

expression* parser::parse_index_expression_remainder(parse_visitor_base& v,
                                                     expression* lhs) {
  QLJS_ASSERT(this->peek().type == token_type::left_square);
  source_code_span left_square_span = this->peek().span();
  this->skip();
  expression* subscript =
      this->parse_expression(v, precedence{.trailing_identifiers = true});
  const char8* end;
  switch (this->peek().type) {
  case token_type::right_square:
    if (subscript->kind() == expression_kind::_missing) {
      // expr[]  // Invalid.
      source_code_span right_square_span = this->peek().span();
      this->diag_reporter_->report(diag_indexing_requires_expression{
          .squares = source_code_span(left_square_span.begin(),
                                      right_square_span.end()),
      });
    }
    end = this->peek().end;
    this->skip();
    break;
  case token_type::end_of_file:
  default:
    this->diag_reporter_->report(
        diag_unmatched_indexing_bracket{.left_square = left_square_span});
    end = this->lexer_.end_of_previous_token();
    break;
  }
  return this->make_expression<expression::index>(lhs, subscript, end);
}

expression* parser::parse_arrow_function_body(
    parse_visitor_base& v, function_attributes attributes,
    const char8* parameter_list_begin, bool allow_in_operator,
    expression_arena::array_ptr<expression*>&& parameters) {
  function_guard guard = this->enter_function(attributes);

  v.visit_enter_function_scope();

  for (expression* parameter : parameters) {
    this->visit_binding_element(parameter, v, variable_kind::_parameter,
                                /*declaring_token=*/std::nullopt,
                                /*init_kind=*/variable_init_kind::normal);
  }
  v.visit_enter_function_scope_body();

  if (this->peek().type == token_type::left_curly) {
    this->parse_and_visit_statement_block_no_scope(v);
  } else {
    this->parse_and_visit_expression(v, precedence{
                                            .commas = false,
                                            .in_operator = allow_in_operator,
                                        });
  }
  v.visit_exit_function_scope();

  const char8* span_end = this->lexer_.end_of_previous_token();
  return this->make_expression<expression::arrow_function>(
      attributes, std::move(parameters), parameter_list_begin, span_end);
}

expression* parser::parse_function_expression(parse_visitor_base& v,
                                              function_attributes attributes,
                                              const char8* span_begin) {
  QLJS_ASSERT(this->peek().type == token_type::kw_function);
  this->skip();
  attributes = this->parse_generator_star(attributes);

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
  std::optional<identifier> function_name = std::nullopt;
  QLJS_WARNING_POP
  switch (this->peek().type) {
  case token_type::kw_await:
  case token_type::kw_yield:
    // NOTE(strager): A function expression named 'await' or 'yield' is allowed
    // even within async functions and generator functions.
    [[fallthrough]];
  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    [[fallthrough]];
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier:
    function_name = this->peek().identifier_name();
    this->skip();
    break;
  default:
    break;
  }

  if (function_name.has_value()) {
    v.visit_enter_named_function_scope(*function_name);
  } else {
    v.visit_enter_function_scope();
  }
  this->parse_and_visit_function_parameters_and_body_no_scope(
      v,
      /*name=*/function_name.has_value()
          ? std::optional<source_code_span>(function_name->span())
          : std::nullopt,
      attributes);
  v.visit_exit_function_scope();

  const char8* span_end = this->lexer_.end_of_previous_token();
  return function_name.has_value()
             ? this->make_expression<expression::named_function>(
                   attributes, *function_name,
                   source_code_span(span_begin, span_end))
             : this->make_expression<expression::function>(
                   attributes, source_code_span(span_begin, span_end));
}

expression* parser::parse_object_literal(parse_visitor_base& v) {
  QLJS_ASSERT(this->peek().type == token_type::left_curly);
  const char8* left_curly_begin = this->peek().begin;
  const char8* right_curly_end;
  this->skip();

  expression_arena::vector<object_property_value_pair> entries(
      "parse_object_literal entries", this->expressions_.allocator());
  auto parse_value_expression = [&]() {
    return this->parse_expression(v, precedence{.commas = false});
  };
  auto parse_computed_property_name = [this, &v]() -> expression* {
    QLJS_ASSERT(this->peek().type == token_type::left_square);
    this->skip();
    expression* property_name = this->parse_expression(v);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_square);
    this->skip();
    return property_name;
  };
  auto parse_equal = [&](const token& key_token, expression* key) {
    expression* lhs;
    bool missing_key;
    switch (key_token.type) {
    case token_type::number:
    case token_type::string:
      lhs = this->make_expression<expression::literal>(key_token.span());
      missing_key = true;
      break;
    default:
      lhs = this->make_expression<expression::variable>(
          key_token.identifier_name(), key_token.type);
      missing_key = false;
      break;
    }

    QLJS_ASSERT(this->peek().type == token_type::equal);
    source_code_span operator_span = this->peek().span();
    this->skip();

    expression* rhs = this->parse_expression(v, precedence{.commas = false});
    expression* value = this->make_expression<expression::assignment>(
        expression_kind::assignment, lhs, rhs, operator_span);
    if (missing_key) {
      this->diag_reporter_->report(diag_missing_key_for_object_entry{
          .expression = value->span(),
      });
    }
    entries.emplace_back(key, value);
  };
  auto parse_method_entry = [&](const char8* key_span_begin, expression* key,
                                function_attributes attributes) -> void {
    switch (this->peek().type) {
    default: {
      this->parse_and_visit_function_parameters_and_body(
          v,
          /*name=*/
          source_code_span(key_span_begin,
                           this->lexer_.end_of_previous_token()),
          attributes);
      break;
    }
    case token_type::right_curly:
      this->diag_reporter_->report(diag_missing_function_parameter_list{
          .expected_parameter_list =
              source_code_span(this->lexer_.end_of_previous_token(),
                               this->lexer_.end_of_previous_token()),
      });
      break;
    }

    const char8* span_end = this->lexer_.end_of_previous_token();
    expression* func = this->make_expression<expression::function>(
        attributes, source_code_span(key_span_begin, span_end));
    entries.emplace_back(key, func);
  };

  bool expect_comma_or_end = false;
  for (;;) {
    if (this->peek().type == token_type::right_curly) {
      right_curly_end = this->peek().end;
      this->skip();
      break;
    }
    if (this->peek().type == token_type::comma) {
      this->skip();
      expect_comma_or_end = false;
      continue;
    }
    if (this->peek().type == token_type::less ||
        this->peek().type == token_type::semicolon) {
      // { k1: v1; k2() {}< k3: v3 }  // Invalid.
      this->diag_reporter_->report(
          diag_expected_comma_to_separate_object_literal_entries{
              .unexpected_token = this->peek().span(),
          });
      this->skip();
      expect_comma_or_end = false;
      continue;
    }
    switch (this->peek().type) {
    // ({x) // Invalid.
    case token_type::end_of_file:
    case token_type::right_paren:
    case token_type::right_square:
      right_curly_end = this->lexer_.end_of_previous_token();
      this->diag_reporter_->report(diag_unclosed_object_literal{
          .object_open =
              source_code_span(left_curly_begin, left_curly_begin + 1),
          .expected_object_close =
              source_code_span(right_curly_end, right_curly_end),
      });
      goto done;

    default:
      break;
    }
    if (expect_comma_or_end) {
      const char8* comma_location = this->lexer_.end_of_previous_token();
      this->diag_reporter_->report(
          diag_missing_comma_between_object_literal_entries{
              source_code_span(comma_location, comma_location)});
    }

  parse_entry:
    switch (this->peek().type) {
    case token_type::comma:
    case token_type::end_of_file:
    case token_type::right_curly:
    case token_type::semicolon:
      QLJS_ASSERT(false);
      break;

    // {#key: value}
    case token_type::private_identifier:
      this->diag_reporter_->report(
          diag_private_properties_are_not_allowed_in_object_literals{
              .private_identifier = this->peek().identifier_name(),
          });
      [[fallthrough]];
    // {key: value}
    // {"key": value}
    // {10: value}
    // {keyAndValue}
    QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET:
    QLJS_CASE_STRICT_RESERVED_KEYWORD:
    case token_type::identifier:
    case token_type::number:
    case token_type::reserved_keyword_with_escape_sequence:
    case token_type::string: {
      token key_token = this->peek();
      expression* key =
          this->make_expression<expression::literal>(key_token.span());
      this->skip();
      switch (this->peek().type) {
      // {x y}  // Invalid.
      // {function f() {}}  // Invalid.
      case token_type::identifier:
        if (key_token.type == token_type::kw_function) {
          this->diag_reporter_->report(
              diag_methods_should_not_use_function_keyword{
                  .function_token = key_token.span(),
              });
          goto parse_entry;
        } else {
          // We'll report diag_missing_comma_between_object_literal_entries on
          // the next iteration of the loop.
          goto single_token_key_and_value;
        }

      single_token_key_and_value:
      case token_type::comma:
      case token_type::less:
      case token_type::right_curly:
      case token_type::semicolon: {
        // Name and value are the same: {keyandvalue}

        switch (key_token.type) {
        case token_type::number:
        case token_type::string: {
          expression* value =
              this->make_expression<expression::_missing>(key_token.span());
          this->diag_reporter_->report(
              diag_invalid_lone_literal_in_object_literal{key_token.span()});
          entries.emplace_back(key, value);
          break;
        }

        QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD : {
          expression* value =
              this->make_expression<expression::_missing>(key_token.span());
          this->diag_reporter_->report(
              diag_missing_value_for_object_literal_entry{
                  .key = key_token.span()});
          entries.emplace_back(key, value);
          break;
        }

        case token_type::kw_await:
        case token_type::kw_yield:
          // TODO(strager): Disallow referencing a variable named 'await' for
          // async functions, or a variable named 'yield' for generator
          // functions.
          goto single_token_key_and_value_identifier;

        single_token_key_and_value_identifier:
        QLJS_CASE_CONTEXTUAL_KEYWORD:
        case token_type::identifier: {
          expression* value = this->make_expression<expression::variable>(
              key_token.identifier_name(), key_token.type);
          entries.emplace_back(key, value);
          break;
        }

        // { protected }
        QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
          // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
          goto single_token_key_and_value_identifier;

        // { \u{69}f }  // Invalid.
        case token_type::reserved_keyword_with_escape_sequence:
          key_token.report_errors_for_escape_sequences_in_keyword(
              this->diag_reporter_);
          goto single_token_key_and_value_identifier;

        // { #privateName }  // Invalid.
        case token_type::private_identifier:
          // We already reported an error. Ignore.
          break;

        default:
          QLJS_UNIMPLEMENTED();
          break;
        }
        break;
      }
      case token_type::colon:
        this->skip();
        entries.emplace_back(key, parse_value_expression());
        break;

      case token_type::equal: {
        parse_equal(key_token, key);
        break;
      }

      // {x += y}  // Invalid.
      expression_without_key:
      QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_STAR:
      QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
      QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
      case token_type::dot:
      case token_type::minus:
      case token_type::minus_minus:
      case token_type::plus:
      case token_type::plus_plus:
      case token_type::question_dot: {
        expression* lhs;
        switch (key_token.type) {
        case token_type::number:
        case token_type::string:
          lhs = this->make_expression<expression::literal>(key_token.span());
          break;
        default:
          lhs = this->make_expression<expression::variable>(
              key_token.identifier_name(), key_token.type);
          break;
        }
        expression* value = this->parse_expression_remainder(
            v, lhs, precedence{.commas = false});
        entries.emplace_back(key, value);
        this->diag_reporter_->report(diag_missing_key_for_object_entry{
            .expression = value->span(),
        });
        break;
      }

      case token_type::left_paren:
        parse_method_entry(key_token.begin, key, function_attributes::normal);
        break;

      case token_type::star:
        if (key_token.type == token_type::kw_function) {
          // { function *f() {} }  // Invalid.
          this->diag_reporter_->report(
              diag_methods_should_not_use_function_keyword{
                  .function_token = key_token.span(),
              });
          this->skip();
          switch (this->peek().type) {
          QLJS_CASE_KEYWORD:
          case token_type::identifier:
          case token_type::number:
          case token_type::reserved_keyword_with_escape_sequence:
          case token_type::string: {
            source_code_span real_key_span = this->peek().span();
            expression* real_key =
                this->make_expression<expression::literal>(real_key_span);
            this->skip();
            parse_method_entry(real_key_span.begin(), real_key,
                               function_attributes::generator);
            break;
          }

          // { get [expr]() {} }
          case token_type::left_square: {
            source_code_span left_square_span = this->peek().span();
            expression* real_key = parse_computed_property_name();
            parse_method_entry(left_square_span.begin(), real_key,
                               function_attributes::generator);
            break;
          }

          default:
            QLJS_PARSER_UNIMPLEMENTED();
            break;
          }
        } else {
          lexer_transaction transaction = this->lexer_.begin_transaction();
          this->skip();
          if (this->peek().type == token_type::left_paren) {
            // {method*() {}}  // Invalid.
            this->lexer_.roll_back_transaction(std::move(transaction));
            parse_method_entry(key_token.begin, key,
                               function_attributes::normal);
          } else {
            this->skip();
            if (this->peek().type == token_type::left_paren) {
              // {someName *method() {}}  // Invalid.
              this->lexer_.roll_back_transaction(std::move(transaction));
              // We'll report diag_missing_comma_between_object_literal_entries
              // on the next iteration of the loop.
              goto single_token_key_and_value;
            } else {
              // {a * b + c}  // Invalid.
              this->lexer_.roll_back_transaction(std::move(transaction));
              goto expression_without_key;
            }
          }
        }
        break;

      // {x  // Invalid.
      case token_type::end_of_file:
        // We'll report diag_unclosed_object_literal later when we look for the
        // comma or closing '}'.
        goto single_token_key_and_value;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }

    // { async methodName() { } }
    // { async *generatorName() { } }
    // { get propertyName() { } }
    case token_type::kw_async:
    case token_type::kw_get:
    case token_type::kw_set: {
      bool is_async = this->peek().type == token_type::kw_async;
      function_attributes method_attributes =
          is_async ? function_attributes::async : function_attributes::normal;
      token keyword_token = this->peek();
      source_code_span keyword_span = this->peek().span();
      token_type keyword_type = this->peek().type;
      this->skip();

      if (this->peek().type == token_type::kw_function) {
        // { set function() { } }
        // { async function f() { } }  // Invalid.
        lexer_transaction transaction = this->lexer_.begin_transaction();
        source_code_span function_keyword_span = this->peek().span();
        this->skip();
        switch (this->peek().type) {
        // { set function() { } }
        case token_type::left_paren:
          this->lexer_.roll_back_transaction(std::move(transaction));
          break;

        // { async function f() { } }  // Invalid.
        case token_type::identifier:
        default:
          this->lexer_.commit_transaction(std::move(transaction));
          this->diag_reporter_->report(
              diag_methods_should_not_use_function_keyword{
                  .function_token = function_keyword_span,
              });
          break;
        }
      }

      if (is_async && this->peek().type == token_type::star) {
        // { async *generatorName() { } }
        method_attributes = is_async ? function_attributes::async_generator
                                     : function_attributes::generator;
        this->skip();
      }

      switch (this->peek().type) {
      // get #method() {}
      case token_type::private_identifier:
        this->diag_reporter_->report(
            diag_private_properties_are_not_allowed_in_object_literals{
                .private_identifier = this->peek().identifier_name(),
            });
        [[fallthrough]];
      // get method() {}
      QLJS_CASE_KEYWORD:
      case token_type::identifier:
      case token_type::number:
      case token_type::reserved_keyword_with_escape_sequence:
      case token_type::string: {
        source_code_span key_span = this->peek().span();
        expression* key = this->make_expression<expression::literal>(key_span);
        this->skip();
        parse_method_entry(keyword_span.begin(), key, method_attributes);
        break;
      }

      // { get [expr]() {} }
      case token_type::left_square: {
        source_code_span left_square_span = this->peek().span();
        expression* key = parse_computed_property_name();
        parse_method_entry(left_square_span.begin(), key, method_attributes);
        break;
      }

      case token_type::equal: {
        expression* key =
            this->make_expression<expression::literal>(keyword_span);
        parse_equal(keyword_token, key);
        break;
      }

      // { get: value }
      // { async: value }
      case token_type::colon: {
        this->skip();
        expression* key =
            this->make_expression<expression::literal>(keyword_span);
        entries.emplace_back(key, parse_value_expression());
        break;
      }

      // { get() {} }
      case token_type::left_paren: {
        expression* key =
            this->make_expression<expression::literal>(keyword_span);
        parse_method_entry(keyword_span.begin(), key,
                           function_attributes::normal);
        break;
      }

      // { get }
      case token_type::comma:
      case token_type::less:
      case token_type::right_curly:
      case token_type::semicolon: {
        expression* key =
            this->make_expression<expression::literal>(keyword_span);
        expression* value = this->make_expression<expression::variable>(
            identifier(keyword_span), keyword_type);
        entries.emplace_back(key, value);
        break;
      }

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }

    // {[keyExpression]: value}
    case token_type::left_square: {
      source_code_span left_square_span = this->peek().span();
      expression* key = parse_computed_property_name();
      switch (this->peek().type) {
      case token_type::colon:
        this->skip();
        entries.emplace_back(key, parse_value_expression());
        break;

      case token_type::left_paren:
        parse_method_entry(left_square_span.begin(), key,
                           function_attributes::normal);
        break;

      case token_type::comma:
      case token_type::less:
      case token_type::right_curly:
      case token_type::semicolon: {
        source_code_span key_span(left_square_span.begin(),
                                  this->lexer_.end_of_previous_token());
        expression* value =
            this->make_expression<expression::_missing>(key_span);
        this->diag_reporter_->report(
            diag_missing_value_for_object_literal_entry{.key = key_span});
        entries.emplace_back(key, value);
        break;
      }

      // {[key]*() {}}  // Invalid.
      case token_type::star:
        parse_method_entry(left_square_span.begin(), key,
                           function_attributes::normal);
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }
      break;
    }

    // *generatorMethod() {}
    case token_type::star: {
      this->skip();
      switch (this->peek().type) {
      // *#method() {}
      case token_type::private_identifier:
        this->diag_reporter_->report(
            diag_private_properties_are_not_allowed_in_object_literals{
                .private_identifier = this->peek().identifier_name(),
            });
        [[fallthrough]];
      // *method() {}
      QLJS_CASE_KEYWORD:
      case token_type::identifier:
      case token_type::number:
      case token_type::reserved_keyword_with_escape_sequence:
      case token_type::string: {
        source_code_span method_name_span = this->peek().span();
        expression* method_name =
            this->make_expression<expression::literal>(method_name_span);
        this->skip();
        parse_method_entry(method_name_span.begin(), method_name,
                           function_attributes::generator);
        break;
      }

      case token_type::left_square: {
        source_code_span left_square_span = this->peek().span();
        expression* key = parse_computed_property_name();
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
        parse_method_entry(left_square_span.begin(), key,
                           function_attributes::generator);
        break;
      }

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }

    // {...other}  // Spread operator.
    case token_type::dot_dot_dot:
      entries.emplace_back(parse_value_expression());
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    expect_comma_or_end = true;
  }
done:
  return this->make_expression<expression::object>(
      this->expressions_.make_array(std::move(entries)),
      source_code_span(left_curly_begin, right_curly_end));
}

expression* parser::parse_class_expression(parse_visitor_base& v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_class);
  const char8* span_begin = this->peek().begin;

  v.visit_enter_class_scope();
  this->parse_and_visit_class_heading(
      v, /*require_name=*/name_requirement::optional);

  if (this->peek().type == token_type::left_curly) {
    this->parse_and_visit_class_body(v);
  } else {
    this->diag_reporter_->report(diag_missing_body_for_class{
        .class_keyword_and_name_and_heritage =
            source_code_span(span_begin, this->lexer_.end_of_previous_token()),
    });
  }
  const char8* span_end = this->lexer_.end_of_previous_token();

  v.visit_exit_class_scope();
  return this->make_expression<expression::_class>(
      source_code_span(span_begin, span_end));
}

expression* parser::parse_jsx_expression(parse_visitor_base& v) {
  QLJS_ASSERT(this->peek().type == token_type::less);

  if (!this->options_.jsx) {
    this->diag_reporter_->report(
        diag_jsx_not_yet_implemented{.jsx_start = this->peek().span()});
#if QLJS_HAVE_SETJMP
    if (this->have_fatal_parse_error_jmp_buf_) {
      std::longjmp(this->fatal_parse_error_jmp_buf_, 1);
      QLJS_UNREACHABLE();
    }
#endif
    return this->make_expression<expression::_missing>(this->peek().span());
  }

  const char8* jsx_begin = this->peek().begin;
  expression* ast = this->parse_jsx_element_or_fragment(v);

  // Check for adjacent elements:
  // <div /><div />  // Invalid.
  if (this->peek().type == token_type::less) {
    const char8* begin_of_second_element = this->peek().begin;
    expression_arena::vector<expression*> elements(
        "parse_jsx_expression adjacent elements",
        this->expressions_.allocator());
    elements.emplace_back(ast);
    do {
      elements.emplace_back(this->parse_jsx_element_or_fragment(v));
    } while (this->peek().type == token_type::less);
    const char8* end = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(diag_adjacent_jsx_without_parent{
        .begin = source_code_span(jsx_begin, jsx_begin),
        .begin_of_second_element =
            source_code_span(begin_of_second_element, begin_of_second_element),
        .end = source_code_span(end, end),
    });
    ast = this->make_expression<expression::jsx_fragment>(
        /*span=*/source_code_span(jsx_begin, end),
        /*children=*/this->expressions_.make_array(std::move(elements)));
  }

  return ast;
}

expression* parser::parse_jsx_element_or_fragment(parse_visitor_base& v) {
  QLJS_ASSERT(this->options_.jsx);
  QLJS_ASSERT(this->peek().type == token_type::less);

  const char8* jsx_begin = this->peek().begin;
  this->lexer_.skip_in_jsx();
  switch (this->peek().type) {
  // <div>
  case token_type::identifier: {
    identifier tag = this->peek().identifier_name();
    this->lexer_.skip_in_jsx();
    expression* ast = this->parse_jsx_element_or_fragment(
        v, /*tag=*/&tag, /*less_begin=*/jsx_begin);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::greater);
    this->skip();
    return ast;
  }

  // <> </>
  case token_type::greater: {
    expression* ast = this->parse_jsx_element_or_fragment(
        v, /*tag=*/nullptr, /*less_begin=*/jsx_begin);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::greater);
    this->lexer_.skip();
    return ast;
  }

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

expression* parser::parse_jsx_element_or_fragment(parse_visitor_base& v,
                                                  identifier* tag,
                                                  const char8* less_begin) {
  depth_guard d_guard(this);

  // If temp_tag_storage is nullopt, then there is no namespace. If
  // temp_tag_storage is not nullopt, then it stores the tag name.
  //
  // Invariant: temp_tag_storage.has_value() == (tag_namespace != nullptr)
  std::optional<identifier> temp_tag_storage = std::nullopt;
  identifier* tag_namespace = nullptr;

  // For <div>, this is empty. For <module.submodule.Component>, this contains
  // {"module", "submodule", "Component"}.
  expression_arena::vector<identifier> tag_members(
      "jsx_member_element parts", this->expressions_.allocator());

  expression_arena::vector<expression*> children(
      "jsx_element children", this->expressions_.allocator());

  auto make_jsx_expression = [&](const char8* greater_end) -> expression* {
    source_code_span span(less_begin, greater_end);
    if (tag_namespace) {
      return this->make_expression<expression::jsx_element_with_namespace>(
          /*span=*/span,
          /*ns=*/*tag_namespace,
          /*tag=*/*tag,
          /*children=*/this->expressions_.make_array(std::move(children)));
    } else if (tag) {
      return this->make_expression<expression::jsx_element>(
          /*span=*/span,
          /*tag=*/*tag,
          /*children=*/this->expressions_.make_array(std::move(children)));
    } else if (!tag_members.empty()) {
      return this->make_expression<expression::jsx_element_with_members>(
          /*span=*/span,
          /*members=*/this->expressions_.make_array(std::move(tag_members)),
          /*children=*/this->expressions_.make_array(std::move(children)));
    } else {
      return this->make_expression<expression::jsx_fragment>(
          /*span=*/span,
          /*children=*/this->expressions_.make_array(std::move(children)));
    }
  };

  if (this->peek().type == token_type::colon) {
    // <namespace:current />
    if (!tag) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->lexer_.skip_in_jsx();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
    temp_tag_storage = this->peek().identifier_name();
    tag_namespace = tag;
    tag = &*temp_tag_storage;
    this->lexer_.skip_in_jsx();
  } else if (this->peek().type == token_type::dot) {
    // <module.Component>
    this->lexer_.skip_in_jsx();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
    tag_members.emplace_back(*tag);
    tag = nullptr;  // Just in case. We don't want to accidentally use 'tag'.
    tag_members.emplace_back(this->peek().identifier_name());
    this->lexer_.skip_in_jsx();
    while (this->peek().type == token_type::dot) {
      // <module.submodule.Component>
      this->lexer_.skip_in_jsx();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
      tag_members.emplace_back(this->peek().identifier_name());
      this->lexer_.skip_in_jsx();
    }
  }
  const char8* tag_end = this->lexer_.end_of_previous_token();

  bool is_intrinsic =
      tag && (tag_namespace || expression::jsx_element::is_intrinsic(*tag));

next_attribute:
  switch (this->peek().type) {
  // <current attribute='value'>
  case token_type::identifier: {
    identifier attribute = this->peek().identifier_name();
    bool has_namespace = false;

    this->lexer_.skip_in_jsx();
    if (this->peek().type == token_type::colon) {
      // <current namespace:attribute>
      has_namespace = true;
      this->lexer_.skip_in_jsx();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
      this->lexer_.skip_in_jsx();
    }
    if (is_intrinsic && !has_namespace && !tag_namespace) {
      this->check_jsx_attribute(attribute);
    }
    if (this->peek().type == token_type::equal) {
      this->lexer_.skip_in_jsx();
      switch (this->peek().type) {
      // <current attribute='value'>
      case token_type::string:
        this->lexer_.skip_in_jsx();
        break;

      // <current attribute={expression}>
      case token_type::left_curly: {
        this->lexer_.skip();
        expression* ast = this->parse_expression(v);
        children.emplace_back(ast);
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
        this->lexer_.skip_in_jsx();
        break;
      }

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    } else {
      // <current attribute>
    }
    goto next_attribute;
  }

  // <current {...attributes}>
  case token_type::left_curly: {
    this->lexer_.skip();
    expression* ast = this->parse_expression(v);
    if (ast->kind() != expression_kind::spread) {
      const char8* ast_begin = ast->span().begin();
      this->diag_reporter_->report(diag_missing_dots_for_attribute_spread{
          .expected_dots = source_code_span(ast_begin, ast_begin),
      });
    }
    children.emplace_back(ast);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
    this->lexer_.skip_in_jsx();
    goto next_attribute;
  }

  // <current>
  case token_type::greater:
    this->lexer_.skip_in_jsx_children();
    goto next;

  // <current />
  case token_type::slash: {
    this->lexer_.skip_in_jsx();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::greater);
    const char8* greater_end = this->peek().end;
    return make_jsx_expression(greater_end);
  }

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

next:
  switch (this->peek().type) {
  // </current>
  // <child>
  case token_type::less: {
    const char8* child_begin = this->peek().begin;
    this->lexer_.skip_in_jsx();
    switch (this->peek().type) {
    // </current>
    // </namespace:current>
    case token_type::slash: {
      this->lexer_.skip_in_jsx();
      const char8* closing_tag_begin = this->peek().begin;

      std::optional<identifier> closing_tag_namespace;
      std::optional<identifier> closing_tag;
      expression_arena::vector<identifier> closing_tag_members(
          "jsx closing_tag_members", this->expressions_.allocator());
      if (this->peek().type == token_type::identifier) {
        closing_tag = this->peek().identifier_name();
        this->lexer_.skip_in_jsx();
      }
      if (this->peek().type == token_type::colon) {
        // </namespace:current>
        this->lexer_.skip_in_jsx();
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
        closing_tag_namespace = std::move(closing_tag);
        closing_tag = this->peek().identifier_name();
        this->lexer_.skip_in_jsx();
      }
      if (this->peek().type == token_type::dot) {
        // <module.submodule.Component>
        if (!closing_tag.has_value()) {
          QLJS_PARSER_UNIMPLEMENTED();
        }
        closing_tag_members.emplace_back(*closing_tag);
        closing_tag.reset();
        do {
          this->lexer_.skip_in_jsx();
          QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
          closing_tag_members.emplace_back(this->peek().identifier_name());
          this->lexer_.skip_in_jsx();
        } while (this->peek().type == token_type::dot);
      }

      bool mismatch = false;
      if ((tag_namespace != nullptr) != closing_tag_namespace.has_value()) {
        mismatch = true;
      }
      if ((tag_namespace && closing_tag_namespace.has_value()) &&
          (tag_namespace->normalized_name() !=
           closing_tag_namespace->normalized_name())) {
        mismatch = true;
      }
      if ((tag != nullptr) != closing_tag.has_value()) {
        mismatch = true;
      }
      if ((tag && closing_tag.has_value()) &&
          (tag->normalized_name() != closing_tag->normalized_name())) {
        mismatch = true;
      }
      if (!std::equal(tag_members.begin(), tag_members.end(),
                      closing_tag_members.begin(), closing_tag_members.end(),
                      [](const identifier& tag_member,
                         const identifier& closing_tag_member) {
                        return tag_member.normalized_name() ==
                               closing_tag_member.normalized_name();
                      })) {
        mismatch = true;
      }
      if (mismatch) {
        bump_vector<char8, monotonic_allocator> opening_tag_name_pretty(
            "opening_tag_name_pretty", &this->diagnostic_memory_);
        if (tag_namespace) {
          opening_tag_name_pretty += tag_namespace->span().string_view();
          opening_tag_name_pretty += u8':';
          QLJS_ASSERT(tag);
        }
        if (tag) {
          opening_tag_name_pretty += tag->span().string_view();
        }
        for (const identifier& member : tag_members) {
          if (!opening_tag_name_pretty.empty()) {
            opening_tag_name_pretty += u8'.';
          }
          opening_tag_name_pretty += member.span().string_view();
        }

        const char8* closing_tag_end = this->lexer_.end_of_previous_token();
        string8_view opening_tag_name_pretty_view(opening_tag_name_pretty);
        opening_tag_name_pretty.release();
        this->diag_reporter_->report(diag_mismatched_jsx_tags{
            .opening_tag_name =
                tag_namespace
                    ? source_code_span(tag_namespace->span().begin(), tag_end)
                    : !tag_members.empty()
                          ? source_code_span(tag_members.front().span().begin(),
                                             tag_end)
                          : tag ? tag->span()
                                : source_code_span(tag_end, tag_end),
            .closing_tag_name =
                closing_tag_begin <= closing_tag_end
                    ? source_code_span(closing_tag_begin, closing_tag_end)
                    :
                    // This happens for </> (fragment close).
                    source_code_span(closing_tag_begin, closing_tag_begin),
            .opening_tag_name_pretty = opening_tag_name_pretty_view,
        });
      }

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::greater);
      const char8* greater_end = this->peek().end;
      return make_jsx_expression(greater_end);
    }

      // <child>
    case token_type::identifier: {
      identifier child_tag = this->peek().identifier_name();
      this->lexer_.skip_in_jsx();
      children.emplace_back(this->parse_jsx_element_or_fragment(
          v, /*tag=*/&child_tag, /*greater_begin=*/child_begin));

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::greater);
      this->lexer_.skip_in_jsx_children();
      goto next;
    }

      // <>
    case token_type::greater: {
      children.emplace_back(this->parse_jsx_element_or_fragment(
          v, /*tag=*/nullptr, /*greater_begin=*/child_begin));

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::greater);
      this->lexer_.skip_in_jsx_children();
      goto next;
    }

    default:
      QLJS_PARSER_UNIMPLEMENTED();
    }
    break;
  }

  // {expression}
  case token_type::left_curly: {
    this->skip();
    expression* ast = this->parse_expression(v);
    children.emplace_back(ast);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
    this->lexer_.skip_in_jsx_children();
    goto next;
  }

  default:
    QLJS_PARSER_UNIMPLEMENTED();
  }

  QLJS_UNREACHABLE();
}

expression* parser::parse_tagged_template(parse_visitor_base& v,
                                          expression* tag) {
  if (this->peek().type == token_type::complete_template) {
    source_code_span template_span = this->peek().span();
    this->skip();
    return this->make_expression<expression::tagged_template_literal>(
        this->expressions_.make_array(&tag, &tag + 1), template_span);
  }

  const char8* template_begin = this->peek().begin;
  expression_arena::vector<expression*> children(
      "parse_tagged_template children", this->expressions_.allocator());
  children.emplace_back(tag);
  for (;;) {
    QLJS_ASSERT(this->peek().type == token_type::incomplete_template);
    this->skip();
    children.emplace_back(this->parse_expression(v));
    switch (this->peek().type) {
    case token_type::right_curly:
      this->lexer_.skip_in_template(template_begin);
      switch (this->peek().type) {
      case token_type::complete_template: {
        const char8* template_end = this->peek().end;
        this->skip();

        expression_arena::array_ptr<expression*> children_array =
            this->expressions_.make_array(std::move(children));
        source_code_span template_span(template_begin, template_end);
        return this->make_expression<expression::tagged_template_literal>(
            children_array, template_span);
      }

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

expression* parser::parse_untagged_template(parse_visitor_base& v) {
  if (this->peek().type == token_type::complete_template) {
    QLJS_UNIMPLEMENTED();
  }

  const char8* template_begin = this->peek().begin;
  expression_arena::vector<expression*> children(
      "parse_untagged_template children", this->expressions_.allocator());
  for (;;) {
    QLJS_ASSERT(this->peek().type == token_type::incomplete_template);
    this->peek().report_errors_for_escape_sequences_in_template(
        this->diag_reporter_);
    this->skip();
    children.emplace_back(this->parse_expression(v));
    switch (this->peek().type) {
    case token_type::right_curly:
      this->lexer_.skip_in_template(template_begin);
      switch (this->peek().type) {
      case token_type::complete_template: {
        this->peek().report_errors_for_escape_sequences_in_template(
            this->diag_reporter_);
        const char8* template_end = this->peek().end;
        this->skip();

        expression_arena::array_ptr<expression*> children_array =
            this->expressions_.make_array(std::move(children));
        source_code_span template_span(template_begin, template_end);
        return this->make_expression<expression::_template>(children_array,
                                                            template_span);
      }

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
