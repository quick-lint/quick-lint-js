// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <memory>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/diag/buffering-diag-reporter.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/buffering-visitor.h>
#include <quick-lint-js/fe/expression.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/enum.h>
#include <utility>

// For Parser::binding_element_info.
QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")

namespace quick_lint_js {
void Parser::visit_expression(Expression* ast, Parse_Visitor_Base& v,
                              Parser::Variable_Context context) {
  auto visit_children = [&] {
    for (Expression* child : ast->children()) {
      this->visit_expression(child, v, context);
    }
  };
  switch (ast->kind()) {
  case Expression_Kind::Class:
  case Expression_Kind::Invalid:
  case Expression_Kind::Missing:
  case Expression_Kind::Arrow_Function:
  case Expression_Kind::Function:
  case Expression_Kind::Import:
  case Expression_Kind::Literal:
  case Expression_Kind::Named_Function:
  case Expression_Kind::New_Target:
  case Expression_Kind::Private_Variable:
  case Expression_Kind::Super:
  case Expression_Kind::This_Variable:
  case Expression_Kind::Yield_None:
    break;

  case Expression_Kind::New:
  case Expression_Kind::Template:
  case Expression_Kind::Array:
  case Expression_Kind::Call:
  case Expression_Kind::JSX_Element_With_Namespace:
  case Expression_Kind::JSX_Fragment:
  case Expression_Kind::Tagged_Template_Literal:
    visit_children();
    break;
  case Expression_Kind::Binary_Operator:
    visit_children();
    this->error_on_pointless_compare_against_literal(
        expression_cast<Expression::Binary_Operator*>(ast));
    error_on_pointless_string_compare(
        expression_cast<Expression::Binary_Operator*>(ast));
    this->error_on_pointless_nullish_coalescing_operator(
        expression_cast<Expression::Binary_Operator*>(ast));
    this->warn_on_xor_operator_as_exponentiation(
        expression_cast<Expression::Binary_Operator*>(ast));
    this->warn_on_unintuitive_bitshift_precedence(
        expression_cast<Expression::Binary_Operator*>(ast));
    break;
  case Expression_Kind::Trailing_Comma: {
    auto& trailing_comma_ast =
        expression_cast<Expression::Trailing_Comma&>(*ast);
    this->diag_reporter_->report(Diag_Missing_Operand_For_Operator{
        .where = trailing_comma_ast.comma_span(),
    });
    visit_children();
    break;
  }
  case Expression_Kind::Assignment: {
    Expression* lhs = ast->child_0();
    Expression* rhs = ast->child_1();
    this->visit_assignment_expression(lhs, rhs, v);

    Expression* lhs_without_paren = lhs->without_paren();
    Expression* rhs_without_paren = rhs->without_paren();

    bool both_sides_are_variables =
        lhs_without_paren->kind() == Expression_Kind::Variable &&
        rhs_without_paren->kind() == Expression_Kind::Variable;
    if (both_sides_are_variables) {
      String8_View lhs_variable_name =
          lhs_without_paren->variable_identifier().normalized_name();
      String8_View rhs_variable_name =
          rhs_without_paren->variable_identifier().normalized_name();
      if (lhs_variable_name == rhs_variable_name) {
        this->diag_reporter_->report(Diag_Variable_Assigned_To_Self_Is_Noop{
            .assignment_statement = ast->span(),
        });
      }
    }
    break;
  }
  case Expression_Kind::Compound_Assignment:
  case Expression_Kind::Conditional_Assignment: {
    Expression* lhs = ast->child_0();
    Expression* rhs = ast->child_1();
    this->visit_compound_or_conditional_assignment_expression(lhs, rhs, v);
    break;
  }
  case Expression_Kind::Typeof: {
    Expression* child = ast->child_0()->without_paren();
    if (child->kind() == Expression_Kind::Variable) {
      v.visit_variable_typeof_use(child->variable_identifier());
    } else {
      this->visit_expression(child, v, context);
    }
    break;
  }
  case Expression_Kind::Delete: {
    Expression* child = ast->child_0();
    if (child->kind() == Expression_Kind::Variable) {
      v.visit_variable_delete_use(
          child->variable_identifier(),
          expression_cast<Expression::Delete*>(ast)->unary_operator_span());
    } else {
      this->visit_expression(child, v, context);
    }
    break;
  }
  case Expression_Kind::Angle_Type_Assertion: {
    Expression* child = ast->child_0();
    if (child->kind() == Expression_Kind::Missing) {
      this->diag_reporter_->report(
          Diag_Missing_Expression_After_Angle_Type_Assertion{
              .expected_expression = Source_Code_Span::unit(
                  expression_cast<Expression::Angle_Type_Assertion*>(ast)
                      ->bracketed_type_span_.end()),
          });
    }
    this->visit_expression(child, v, context);
    break;
  }
  case Expression_Kind::Spread: {
    if (ast->child_0()->kind() == Expression_Kind::Missing) {
      this->diag_reporter_->report(
          Diag_Spread_Must_Precede_Expression{this->peek().span()});
    }
    [[fallthrough]];
  }
  case Expression_Kind::As_Type_Assertion:
  case Expression_Kind::Await:
  case Expression_Kind::Satisfies:
  case Expression_Kind::Unary_Operator:
  case Expression_Kind::Yield_Many:
  case Expression_Kind::Yield_One:
    this->visit_expression(ast->child_0(), v, context);
    break;
  case Expression_Kind::Conditional:
    this->visit_expression(ast->child_0(), v, context);
    this->visit_expression(ast->child_1(), v, context);
    this->visit_expression(ast->child_2(), v, context);
    break;
  case Expression_Kind::Dot:
    this->visit_expression(ast->child_0(), v, Variable_Context::rhs);
    break;
  case Expression_Kind::Index:
    this->visit_expression(ast->child_0(), v, Variable_Context::rhs);
    this->visit_expression(ast->child_1(), v, Variable_Context::rhs);
    this->warn_on_comma_operator_in_index(
        ast->child_1(),
        expression_cast<Expression::Index*>(ast)->left_square_span);
    break;

  case Expression_Kind::JSX_Element: {
    auto* element = expression_cast<Expression::JSX_Element*>(ast);
    if (!element->is_intrinsic()) {
      v.visit_variable_use(element->tag);
    }
    visit_children();
    break;
  }
  case Expression_Kind::JSX_Element_With_Members: {
    auto* element = expression_cast<Expression::JSX_Element_With_Members*>(ast);
    QLJS_ASSERT(element->members.size() >= 1);
    v.visit_variable_use(element->members[0]);
    visit_children();
    break;
  }
  case Expression_Kind::Object:
    for (Span_Size i = 0; i < ast->object_entry_count(); ++i) {
      auto entry = ast->object_entry(i);

      if (entry.init && context == Variable_Context::rhs &&
          entry.is_merged_property_and_value_shorthand()) {
        // { key = value }  // Invalid.
        this->diag_reporter_->report(Diag_Object_Literal_Default_In_Expression{
            .equal = entry.init_equals_span(),
        });
        // Behave as if the code was instead: { key: value }
        this->visit_expression(entry.property, v, Variable_Context::rhs);
        this->visit_expression(entry.init, v, context);
        continue;
      }

      if (entry.property) {
        this->visit_expression(entry.property, v, Variable_Context::rhs);
      }

      if (entry.init) {
        // { key: value = init }
        switch (context) {
        case Variable_Context::rhs:
          // Visit as if: { key: (value = init) }
          this->visit_expression(entry.init, v, Variable_Context::rhs);
          this->visit_expression(entry.value, v, Variable_Context::lhs);
          this->maybe_visit_assignment(entry.value, v,
                                       Variable_Assignment_Flags::none);
          break;
        case Variable_Context::lhs:
          // ({ key: value = init } = obj);
          // The caller will call maybe_visit_assignment for us.
          this->visit_expression(entry.init, v, Variable_Context::rhs);
          this->visit_expression(entry.value, v, Variable_Context::lhs);
          break;
        }
      } else {
        // { key: value }
        this->visit_expression(entry.value, v, context);
      }
    }
    break;
  case Expression_Kind::Optional: {
    auto* optional = expression_cast<const Expression::Optional*>(ast);
    this->visit_expression(optional->child_, v, context);
    this->diag_reporter_->report(Diag_Unexpected_Question_In_Expression{
        .question = optional->question_span(),
    });
    break;
  }
  case Expression_Kind::Paren:
    this->visit_expression(ast->child_0(), v, context);
    break;
  case Expression_Kind::Paren_Empty: {
    Expression::Paren_Empty* paren_empty =
        expression_cast<Expression::Paren_Empty*>(ast);
    paren_empty->report_missing_expression_error(this->diag_reporter_);
    break;
  }
  case Expression_Kind::RW_Unary_Prefix:
  case Expression_Kind::RW_Unary_Suffix: {
    Expression* child = ast->child_0();
    this->visit_expression(child, v, Variable_Context::rhs);
    this->maybe_visit_assignment(child, v, Variable_Assignment_Flags::none);
    break;
  }
  case Expression_Kind::Non_Null_Assertion:
    this->visit_expression(ast->child_0(), v, context);
    break;
  case Expression_Kind::Type_Annotated: {
    Expression::Type_Annotated* annotated =
        expression_cast<Expression::Type_Annotated*>(ast);
    this->visit_expression(annotated->child_, v, context);
    this->diag_reporter_->report(Diag_TypeScript_Type_Annotation_In_Expression{
        .type_colon = annotated->colon_span(),
    });
    break;
  }
  case Expression_Kind::Variable:
    switch (context) {
    case Variable_Context::lhs:
      break;
    case Variable_Context::rhs:
      if (ast->variable_identifier_token_type() ==
          Token_Type::reserved_keyword_with_escape_sequence) {
        v.visit_keyword_variable_use(ast->variable_identifier());
      } else {
        v.visit_variable_use(ast->variable_identifier());
      }
      break;
    }
    break;
  }
}

void Parser::visit_assignment_expression(Expression* lhs, Expression* rhs,
                                         Parse_Visitor_Base& v) {
  this->visit_expression(lhs, v, Variable_Context::lhs);
  this->visit_expression(rhs, v, Variable_Context::rhs);
  this->maybe_visit_assignment(lhs, v, Variable_Assignment_Flags::none);
}

void Parser::visit_compound_or_conditional_assignment_expression(
    Expression* lhs, Expression* rhs, Parse_Visitor_Base& v) {
  this->visit_expression(lhs, v, Variable_Context::rhs);
  this->visit_expression(rhs, v, Variable_Context::rhs);
  this->maybe_visit_assignment(lhs, v, Variable_Assignment_Flags::none);
}

void Parser::maybe_visit_assignment(Expression* ast, Parse_Visitor_Base& v,
                                    Variable_Assignment_Flags flags) {
  switch (ast->kind()) {
  case Expression_Kind::Array:
    for (Expression* child : ast->children()) {
      this->maybe_visit_assignment(child, v, flags);
    }
    break;
  case Expression_Kind::Object:
    for (Span_Size i = 0; i < ast->object_entry_count(); ++i) {
      Expression* value = ast->object_entry(i).value;
      this->maybe_visit_assignment(value, v, flags);
    }
    break;
  case Expression_Kind::Angle_Type_Assertion:
  case Expression_Kind::As_Type_Assertion:
  case Expression_Kind::Satisfies:
    this->maybe_visit_assignment(
        ast->child_0(), v,
        enum_set_flags(flags, Variable_Assignment_Flags::type_asserted));
    break;
  case Expression_Kind::Non_Null_Assertion:
  case Expression_Kind::Paren:
    this->maybe_visit_assignment(ast->child_0(), v, flags);
    break;
  case Expression_Kind::Variable:
    v.visit_variable_assignment(ast->variable_identifier(), flags);
    break;
  default:
    break;
  }
}

Expression* Parser::parse_expression(Parse_Visitor_Base& v, Precedence prec) {
  Depth_Guard guard(this);
  Expression* ast = this->parse_primary_expression(v, prec);
  if (!prec.binary_operators && prec.math_or_logical_or_assignment) {
    return ast;
  }
  return this->parse_expression_remainder(v, ast, prec);
}

// TODO(strager): Why do we need precedence here? Could we get rid of prec?
Expression* Parser::parse_primary_expression(Parse_Visitor_Base& v,
                                             Precedence prec) {
  switch (this->peek().type) {
  // f  // Variable name.
  QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD:
  identifier:
  case Token_Type::identifier:
  case Token_Type::kw_accessor:
  case Token_Type::kw_as:
  case Token_Type::kw_from:
  case Token_Type::kw_get:
  case Token_Type::kw_let:
  case Token_Type::kw_of:
  case Token_Type::kw_satisfies:
  case Token_Type::kw_set:
  case Token_Type::kw_static: {
    Expression* ast = this->make_expression<Expression::Variable>(
        this->peek().identifier_name(), this->peek().type);
    this->skip();
    return ast;
  }

  // \u{69}\u{66} // 'if', but escaped.
  case Token_Type::reserved_keyword_with_escape_sequence:
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
  case Token_Type::kw_false:
  case Token_Type::kw_null:
  case Token_Type::kw_true:
  case Token_Type::number:
  case Token_Type::string: {
    Expression* ast =
        this->make_expression<Expression::Literal>(this->peek().span());
    this->skip();
    return ast;
  }

  case Token_Type::kw_this: {
    Expression* ast =
        this->make_expression<Expression::This_Variable>(this->peek().span());
    this->skip();
    return ast;
  }

  // `hello`
  case Token_Type::complete_template: {
    this->peek().report_errors_for_escape_sequences_in_template(
        this->diag_reporter_);
    Expression* ast =
        this->make_expression<Expression::Literal>(this->peek().span());
    this->skip();
    return ast;
  }

  // import.meta
  case Token_Type::kw_import: {
    Expression* ast =
        this->make_expression<Expression::Import>(this->peek().span());
    this->skip();
    return ast;
  }

  // super()
  case Token_Type::kw_super: {
    Expression* ast =
        this->make_expression<Expression::Super>(this->peek().span());
    this->skip();
    return ast;
  }

  // `hello${world}`
  case Token_Type::incomplete_template: {
    Expression* ast = this->parse_untagged_template(v);
    return ast;
  }

  // await            // Identifier.
  // await myPromise
  case Token_Type::kw_await: {
    Token await_token = this->peek();
    this->skip();
    return this->parse_await_expression(v, await_token, prec);
  }

  // yield       // Identifier.
  // yield       // Operator.
  // yield item
  case Token_Type::kw_yield: {
    if (this->in_generator_function_) {
      // yield is a unary operator.
      Source_Code_Span operator_span = this->peek().span();
      this->skip();
      switch (this->peek().type) {
      case Token_Type::colon:
      case Token_Type::comma:
      case Token_Type::end_of_file:
      case Token_Type::kw_in:
      case Token_Type::question:
      case Token_Type::right_curly:
      case Token_Type::right_paren:
      case Token_Type::right_square:
      case Token_Type::semicolon:
        return this->make_expression<Expression::Yield_None>(operator_span);

      case Token_Type::star: {
        this->skip();
        Expression* child = this->parse_expression(v, prec);
        return this->make_expression<Expression::Yield_Many>(child,
                                                             operator_span);
      }

      default: {
        Expression* child = this->parse_expression(v, prec);
        return this->make_expression<Expression::Yield_One>(child,
                                                            operator_span);
      }
      }
    } else {
      // yield is an identifier.
      goto identifier;
    }
  }

  // ...args  // Spread operator.
  case Token_Type::dot_dot_dot: {
    Source_Code_Span operator_span = this->peek().span();
    this->skip();
    Expression* child = this->parse_expression(v, Precedence{.commas = false});
    return this->make_expression<Expression::Spread>(child, operator_span);
  }

  // !x
  // delete o[key]
  case Token_Type::bang:
  case Token_Type::kw_delete:
  case Token_Type::kw_typeof:
  case Token_Type::kw_void:
  case Token_Type::minus:
  case Token_Type::plus:
  case Token_Type::tilde: {
    Token_Type type = this->peek().type;
    Source_Code_Span operator_span = this->peek().span();
    this->skip();
    Expression* child = this->parse_expression(
        v, Precedence{
               .binary_operators = true,
               .math_or_logical_or_assignment = false,
               .commas = false,
               .in_operator = prec.in_operator,
               .colon_type_annotation = Allow_Type_Annotations::never,
               .conditional_operator = false,
           });
    if (child->kind() == Expression_Kind::Missing) {
      this->diag_reporter_->report(Diag_Missing_Operand_For_Operator{
          .where = operator_span,
      });
    }
    Expression* ast =
        type == Token_Type::kw_delete
            ? this->make_expression<Expression::Delete>(child, operator_span)
            : type == Token_Type::kw_typeof
                  ? this->make_expression<Expression::Typeof>(child,
                                                              operator_span)
                  : this->make_expression<Expression::Unary_Operator>(
                        child, operator_span);
    return ast;
  }

  // --x
  case Token_Type::minus_minus:
  case Token_Type::plus_plus: {
    Source_Code_Span operator_span = this->peek().span();
    this->skip();
    Expression* child =
        this->parse_expression(v, Precedence{
                                      .binary_operators = false,
                                      .math_or_logical_or_assignment = false,
                                      .commas = false,
                                      .in_operator = prec.in_operator,
                                      .conditional_operator = false,
                                  });
    if (child->kind() == Expression_Kind::Missing) {
      this->diag_reporter_->report(Diag_Missing_Operand_For_Operator{
          .where = operator_span,
      });
    }
    return this->make_expression<Expression::RW_Unary_Prefix>(child,
                                                              operator_span);
  }

  // () => {}     // Arrow function.
  // (x) => {}    // Arrow function.
  // (x + y * z)  // Parenthesized expression.
  case Token_Type::left_paren: {
    Source_Code_Span left_paren_span = this->peek().span();
    this->skip();

    if (this->peek().type == Token_Type::right_paren) {
      // ()        // Invalid.
      // () => {}
      Source_Code_Span right_paren_span = this->peek().span();
      this->skip();
      return this->make_expression<Expression::Paren_Empty>(
          Source_Code_Span(left_paren_span.begin(), right_paren_span.end()));
    }

    // (x) => {}
    // (x + y * z)
    Expression* child = this->parse_expression(
        v, Precedence{
               .colon_type_annotation = Allow_Type_Annotations::always,
               .trailing_identifiers = true,
               .colon_question_is_typescript_optional_with_type_annotation =
                   this->options_.typescript,
           });
    switch (this->peek().type) {
    case Token_Type::right_paren:
      this->skip();
      break;
    default:
      this->diag_reporter_->report(Diag_Unmatched_Parenthesis{left_paren_span});
      break;
    }
    return this->make_expression<Expression::Paren>(
        Source_Code_Span(left_paren_span.begin(),
                         this->lexer_.end_of_previous_token()),
        child);
  }

  // async           // Identifier.
  // async () => {}  // Arrow function.
  case Token_Type::kw_async: {
    Token async_token = this->peek();
    this->skip();
    return this->parse_async_expression(v, async_token, prec);
  }

  // [x, 3, f()]  // Array literal.
  case Token_Type::left_square: {
    const Char8* left_square_begin = this->peek().begin;
    const Char8* right_square_end;
    bool met_expression = false;
    this->skip();

    Expression_Arena::Vector<Expression*> children(
        "parse_expression array children", this->expressions_.allocator());
    for (;;) {
      if (this->peek().type == Token_Type::right_square) {
        right_square_end = this->peek().end;
        this->skip();
        break;
      }

      if (this->peek().type == Token_Type::comma) {
        met_expression = false;
        this->skip();
        continue;
      }
      const Char8* previous_expession_end =
          this->lexer_.end_of_previous_token();
      const Char8* child_begin = this->peek().begin;
      Expression* child =
          this->parse_expression(v, Precedence{.commas = false});
      if (this->peek().begin == child_begin) {
        // parse_expression parsed nothing.
        // TODO(strager): Should parse_expression return nullptr if it sees a
        // keyword (instead of returning _invalid and forcing us to check if it
        // parsed anything)?
        const Char8* expected_right_square =
            this->lexer_.end_of_previous_token();
        this->diag_reporter_->report(Diag_Missing_Array_Close{
            .left_square =
                Source_Code_Span(left_square_begin, left_square_begin + 1),
            .expected_right_square =
                Source_Code_Span::unit(expected_right_square),
        });
        right_square_end = expected_right_square;
        break;
      }
      if (met_expression) {
        this->diag_reporter_->report(Diag_Missing_Comma_Between_Array_Elements{
            .expected_comma = Source_Code_Span::unit(previous_expession_end),
        });
      }
      met_expression = true;
      children.emplace_back(child);
    }
    Expression* ast = this->make_expression<Expression::Array>(
        this->expressions_.make_array(std::move(children)),
        Source_Code_Span(left_square_begin, right_square_end));
    return ast;
  }

  // {k: v}  // Object literal.
  case Token_Type::left_curly: {
    Expression* ast = this->parse_object_literal(v);
    return ast;
  }

  // function() {}  // Function expression.
  case Token_Type::kw_function: {
    Expression* function = this->parse_function_expression(
        v, Function_Attributes::normal, this->peek().begin);
    return function;
  }

  // class {}
  parse_class:
  case Token_Type::kw_class: {
    Expression* class_expression = this->parse_class_expression(v);
    return class_expression;
  }

  // @myDecorator class {}
  case Token_Type::at:
    // TODO(strager): Should we make a Decorated expression type?
    this->parse_and_visit_one_or_more_decorators(v);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_class);
    goto parse_class;

  // new XMLHttpRequest()
  // new.target
  case Token_Type::kw_new: {
    Source_Code_Span operator_span = this->peek().span();
    this->skip();

    switch (this->peek().type) {
    // new XMLHttpRequest()
    default: {
      Expression* target = this->parse_expression(v, prec);
      Expression_Arena::Vector<Expression*> children(
          "parse_expression new children", this->expressions_.allocator());
      if (target->kind() == Expression_Kind::Call) {
        for (Expression* child : target->children()) {
          children.emplace_back(child);
        }
      } else {
        children.emplace_back(target);
      }
      return this->make_expression<Expression::New>(
          this->expressions_.make_array(std::move(children)),
          Source_Code_Span(operator_span.begin(), target->span_end()));
    }

    // new.target
    case Token_Type::dot: {
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::identifier);
      // TODO(strager): Check that the given identifier is 'target'.
      // * Are \u{} escapes allowed?
      Source_Code_Span target_span = this->peek().identifier_name().span();
      this->skip();
      Expression* ast = this->make_expression<Expression::New_Target>(
          Source_Code_Span(operator_span.begin(), target_span.end()));
      return ast;
    }
    }
    QLJS_UNREACHABLE();
  }

  // /regexp/    // RegExp literal.
  // /=regexp/  // RegExp literal.
  case Token_Type::slash:
  case Token_Type::slash_equal: {
    this->lexer_.reparse_as_regexp();
    Expression* regexp =
        this->make_expression<Expression::Literal>(this->peek().span());
    this->skip();
    return regexp;
  }

  QLJS_CASE_BINARY_ONLY_OPERATOR:
  QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL:
  QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
  case Token_Type::comma:
  case Token_Type::dot:
  case Token_Type::equal:
  case Token_Type::kw_in:
  case Token_Type::question: {
    if (this->peek().type == Token_Type::star) {
      Token star_token = this->peek();
      std::optional<Function_Attributes> attributes =
          this->try_parse_function_with_leading_star();
      if (attributes.has_value()) {
        Expression* function = this->parse_function_expression(
            v, attributes.value(), star_token.begin);
        return function;
      }
    }
    Expression* ast =
        this->make_expression<Expression::Missing>(this->peek().span());
    if (prec.binary_operators) {
      this->diag_reporter_->report(
          Diag_Missing_Operand_For_Operator{this->peek().span()});
    }
    return ast;
  }

  // <MyComponent />     // JSX only.
  // <T>(params) => {}   // TypeScript only.
  // <T,>(params) => {}  // TypeScript only.
  case Token_Type::less:
    return this->parse_jsx_or_typescript_generic_expression(v, prec);

  // => expr  // Invalid. Treat as arrow function.
  // => {}    // Invalid. Treat as arrow function.
  case Token_Type::equal_greater: {
    Source_Code_Span arrow_span = this->peek().span();
    this->diag_reporter_->report(Diag_Missing_Arrow_Function_Parameter_List{
        .arrow = arrow_span,
    });
    this->skip();

    Expression* arrow_function = this->parse_arrow_function_body(
        v, Function_Attributes::normal,
        /*parameter_list_begin=*/arrow_span.begin(),
        /*prec=*/prec, Expression_Arena::Array_Ptr<Expression*>(),
        /*return_type_visits=*/nullptr);
    return arrow_function;
  }

  case Token_Type::private_identifier: {
    Expression* ast = this->make_expression<Expression::Private_Variable>(
        this->peek().identifier_name());
    this->skip();
    if (this->peek().type == Token_Type::kw_in && prec.in_operator) {
      // #prop in obj
    } else {
      // #prop()             // Invalid
      // #prop.subprop       // Invalid
      // for (#prop in obj)  // Invalid
      this->diag_reporter_->report(
          Diag_Cannot_Refer_To_Private_Variable_Without_Object{
              .private_identifier = ast->span(),
          });
    }
    return ast;
  }

  case Token_Type::colon:
  case Token_Type::kw_debugger: {
    Source_Code_Span token_span = this->peek().span();
    this->diag_reporter_->report(Diag_Unexpected_Token{token_span});
    this->skip();
    return this->make_expression<Expression::Invalid>(token_span);
  }

  case Token_Type::end_of_file:
  case Token_Type::kw_break:
  case Token_Type::kw_case:
  case Token_Type::kw_catch:
  case Token_Type::kw_const:
  case Token_Type::kw_continue:
  case Token_Type::kw_default:
  case Token_Type::kw_do:
  case Token_Type::kw_else:
  case Token_Type::kw_enum:
  case Token_Type::kw_export:
  case Token_Type::kw_extends:
  case Token_Type::kw_finally:
  case Token_Type::kw_for:
  case Token_Type::kw_if:
  case Token_Type::kw_return:
  case Token_Type::kw_switch:
  case Token_Type::kw_throw:
  case Token_Type::kw_try:
  case Token_Type::kw_var:
  case Token_Type::kw_while:
  case Token_Type::kw_with:
  case Token_Type::right_curly:
  case Token_Type::right_paren:
  case Token_Type::right_square:
  case Token_Type::semicolon:
    return this->make_expression<Expression::Missing>(this->peek().span());

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

Expression* Parser::parse_async_expression(Parse_Visitor_Base& v,
                                           const Token& async_token,
                                           Precedence prec) {
  Expression* ast =
      this->parse_async_expression_only(v, async_token, /*prec=*/prec);
  if (!prec.binary_operators) {
    return ast;
  }
  return this->parse_expression_remainder(v, ast, prec);
}

Expression* Parser::parse_async_expression_only(
    Parse_Visitor_Base& v, const Token& async_or_await_token, Precedence prec) {
  bool is_async = async_or_await_token.type == Token_Type::kw_async;
  bool is_await = async_or_await_token.type == Token_Type::kw_await;
  QLJS_ASSERT(is_async || is_await);
  const Char8* async_begin = async_or_await_token.begin;

  auto parse_arrow_function_arrow_and_body =
      [&](auto&& parameters, Buffering_Visitor* parameter_visits,
          Buffering_Visitor* return_type_visits) {
        if (this->peek().type == Token_Type::equal_greater) {
          this->skip();
        } else {
          this->diag_reporter_->report(
              Diag_Missing_Arrow_Operator_In_Arrow_Function{
                  .where = this->peek().span(),
              });
        }
        if (is_await) {
          this->diag_reporter_->report(Diag_Await_Followed_By_Arrow_Function{
              .await_operator = async_or_await_token.span(),
          });
        }

        v.visit_enter_function_scope();
        if (parameter_visits) {
          std::move(*parameter_visits).move_into(v);
        }
        Expression* ast = this->parse_arrow_function_body_no_scope(
            v, Function_Attributes::async, async_begin,
            /*prec=*/prec, this->expressions_.make_array(std::move(parameters)),
            /*return_type_visits=*/return_type_visits);
        v.visit_exit_function_scope();
        return ast;
      };

  switch (this->peek().type) {
  // async () => {}       // Arrow function.
  // async()              // Function call.
  // await (promise)      // Unary operator.
  // await (param) => {}  // Arrow function (invalid).
  case Token_Type::left_paren: {
    bool newline_after_async = this->peek().has_leading_newline;

    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    Source_Code_Span left_paren_span = this->peek().span();
    this->skip();
    Expression_Arena::Vector<Expression*> parameters =
        this->parse_arrow_function_parameters_or_call_arguments(v);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_paren);
    Source_Code_Span right_paren_span = this->peek().span();
    this->skip();

    Buffering_Visitor return_type_visits(&this->type_expression_memory_);
    if (this->peek().type == Token_Type::colon && this->options_.typescript) {
      // async (params): ReturnType => {}  // TypeScript only.
      this->parse_and_visit_typescript_colon_type_expression(
          return_type_visits,
          TypeScript_Type_Parse_Options{
              .allow_assertion_signature_or_type_predicate = true,
          });
    }

    bool is_arrow_function = this->peek().type == Token_Type::equal_greater;
    bool is_arrow_function_without_arrow =
        !this->peek().has_leading_newline &&
        this->peek().type == Token_Type::left_curly &&
        !prec.in_class_extends_clause /* See NOTE[extends-await-paren]. */;
    if (is_arrow_function || is_arrow_function_without_arrow) {
      this->lexer_.commit_transaction(std::move(transaction));
      if (newline_after_async) {
        this->diag_reporter_->report(
            Diag_Newline_Not_Allowed_Between_Async_And_Parameter_List{
                .async = async_or_await_token.span(),
                .arrow = this->peek().span(),
            });
      }
      for (auto* parameter : parameters) {
        if (parameter->kind() == Expression_Kind::Variable &&
            parameter->variable_identifier_token_type() ==
                Token_Type::kw_await) {
          // async (await) => {}  // Invalid
          this->diag_reporter_->report(
              Diag_Cannot_Declare_Await_In_Async_Function{
                  .name = parameter->variable_identifier().span(),
              });
        }
      }
      // TODO(strager): Should we call maybe_wrap_erroneous_arrow_function?
      return parse_arrow_function_arrow_and_body(std::move(parameters),
                                                 /*parameter_visits=*/nullptr,
                                                 &return_type_visits);
    } else if (is_async) {
      // async as an identifier (variable reference)
      // Function call: async(arg)
      this->lexer_.commit_transaction(std::move(transaction));
      // TODO(strager): Reduce copying of the arguments.
      Expression_Arena::Vector<Expression*> call_children(
          "parse_expression async call children",
          this->expressions_.allocator());
      call_children.emplace_back(this->make_expression<Expression::Variable>(
          async_or_await_token.identifier_name(), async_or_await_token.type));
      for (Vector_Size i = 0; i < parameters.size(); ++i) {
        if (parameters.data()[i]->kind() != Expression_Kind::Invalid) {
          call_children.emplace_back(parameters.data()[i]);
        }
      }

      Expression* call_ast = this->make_expression<Expression::Call>(
          this->expressions_.make_array(std::move(call_children)),
          /*left_paren_span=*/left_paren_span,
          /*span_end=*/right_paren_span.end());
      return call_ast;
    } else if (is_await) {
      // await is an operator.
      this->lexer_.roll_back_transaction(std::move(transaction));
      goto await_operator;
    }

    QLJS_UNREACHABLE();
  }

  // async < 42          // Variable.
  // async <T,>() => {}  // Arrow function. TypeScript only.
  // async <T>() => {}   // Invalid in TypeScript.
  // async<T>()          // Function call. TypeScript only.
  // async<T>            // Variable with generic arguments. TypeScript only.
  // TODO(#832): Parse generic arrow with 'await' keyword.
  case Token_Type::less:
    if (this->options_.typescript) {
      Source_Code_Span less = this->peek().span();
      bool newline_before_async_token = this->peek().has_leading_newline;

      // If illegal_lone_parameter is set, then the code is like
      // 'async <T>() => {}' which TypeScript rejects in JSX mode. Therefore,
      // we may need to report
      // Diag_TypeScript_Generic_Arrow_Needs_Comma_In_JSX_Mode.
      //
      // TODO(strager): Figure out a more efficient way to do this than with
      // two transactions.
      std::optional<Source_Code_Span> illegal_lone_parameter;
      if (this->options_.jsx) {
        Lexer_Transaction lone_parameter_transaction =
            this->lexer_.begin_transaction();
        this->skip();  // <
        if (this->peek().type == Token_Type::identifier) {
          Source_Code_Span parameter_name = this->peek().span();
          this->skip();  // (name)
          if (this->peek().type == Token_Type::greater) {
            illegal_lone_parameter = parameter_name;
          }
        }
        this->lexer_.roll_back_transaction(
            std::move(lone_parameter_transaction));
      }

      Parser_Transaction transaction = this->begin_transaction();

      Stacked_Buffering_Visitor parameter_visits =
          this->buffering_visitor_stack_.push();
      std::optional<Expression_Arena::Vector<Expression*>> parameters;
      bool parsed_arrow_function_parameters_without_fatal_error =
          this->catch_fatal_parse_errors([&] {
            this->parse_and_visit_typescript_generic_parameters(
                parameter_visits.visitor());

            // NOTE(strager): QLJS_PARSER_UNIMPLEMENTED here will make
            // parsed_arrow_function_parameters_without_fatal_error false.
            QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::left_paren);
            this->skip();
            parameters.emplace(
                this->parse_arrow_function_parameters_or_call_arguments(
                    parameter_visits.visitor()));
            QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_paren);
            this->skip();

            switch (this->peek().type) {
            // async <T>() => {}
            // async <T>(): ReturnType => {}
            case Token_Type::colon:
            case Token_Type::equal_greater:
              break;
            default:
              QLJS_PARSER_UNIMPLEMENTED();
              break;
            }
          });
      if (parsed_arrow_function_parameters_without_fatal_error) {
        // async <T,>() => {}
        // async <T extends U>() => {}
        // async <T>() => {}   // Invalid in TypeScript-JSX;
        //                     // valid in pure TypeScript.
        this->commit_transaction(std::move(transaction));
        // TODO(strager): Error on parameters named 'await' (like non-generic
        // code path).
        Buffering_Visitor return_type_visits(&this->type_expression_memory_);
        if (this->peek().type == Token_Type::colon) {
          this->parse_and_visit_typescript_colon_type_expression(
              return_type_visits,
              TypeScript_Type_Parse_Options{
                  .allow_assertion_signature_or_type_predicate = true,
              });
        }
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::equal_greater);
        Source_Code_Span equal_greater = this->peek().span();
        if (newline_before_async_token) {
          this->diag_reporter_->report(
              Diag_Newline_Not_Allowed_Between_Async_And_Parameter_List{
                  .async = async_or_await_token.span(),
                  .arrow = equal_greater,
              });
        }

        if (illegal_lone_parameter.has_value()) {
          this->diag_reporter_->report(
              Diag_TypeScript_Generic_Arrow_Needs_Comma_In_JSX_Mode{
                  .generic_parameters_less = less,
                  .expected_comma =
                      Source_Code_Span::unit(illegal_lone_parameter->end()),
                  .arrow = this->peek().span(),
              });
        }

        return parse_arrow_function_arrow_and_body(
            std::move(*parameters),
            /*parameter_visits=*/&parameter_visits.visitor(),
            &return_type_visits);
      }
      this->roll_back_transaction(std::move(transaction));
    }
    goto variable_reference;

  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow parameters named 'protected', 'implements', etc. in
    // strict mode.
    [[fallthrough]];
  // async parameter => expression-or-block  // Arrow function.
  // async this => expression-or-block       // Arrow function (invalid).
  // await parameter => expression-or-block  // Arrow function (invalid).
  // await promise                           // Unary operator.
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case Token_Type::identifier:
  case Token_Type::kw_await:
  case Token_Type::kw_this:
  case Token_Type::kw_yield: {
    if (is_async && this->peek().has_leading_newline) {
      goto variable_reference;
    }

    bool parameter_is_await = this->peek().type == Token_Type::kw_await;
    if (parameter_is_await && is_async) {
      // async await => {}  // Invalid
      this->diag_reporter_->report(Diag_Cannot_Declare_Await_In_Async_Function{
          .name = this->peek().span(),
      });
    }

    Lexer_Transaction transaction = this->lexer_.begin_transaction();

    Source_Code_Span parameter_span = this->peek().span();
    std::array<Expression*, 1> parameters = {
        this->peek().type == Token_Type::kw_this
            ? this->make_expression<Expression::This_Variable>(parameter_span)
            : this->make_expression<Expression::Variable>(
                  Identifier(parameter_span), this->peek().type),
    };
    this->skip();

    std::optional<Source_Code_Span> optional_question_span;
    if (this->peek().type == Token_Type::question) {
      if (is_await) {
        // await a ? b : c
        this->lexer_.roll_back_transaction(std::move(transaction));
        goto await_operator;
      } else {
        // async param? => {}  // Invalid.
        optional_question_span = this->peek().span();
        parameters[0] = this->make_expression<Expression::Optional>(
            parameters[0], *optional_question_span);
        this->skip();
      }
    }

    std::optional<Source_Code_Span> type_colon_span;
    if (is_async && this->peek().type == Token_Type::colon &&
        this->options_.typescript) {
      // async param: Type => {}  // Invalid.
      type_colon_span = this->peek().span();
      Buffering_Visitor type_visits(&this->type_expression_memory_);
      this->parse_and_visit_typescript_colon_type_expression(type_visits);
      const Char8* type_end = this->lexer_.end_of_previous_token();

      parameters[0] = this->make_expression<Expression::Type_Annotated>(
          parameters[0], *type_colon_span, std::move(type_visits), type_end);
    }

    if (optional_question_span.has_value() && type_colon_span.has_value()) {
      this->diag_reporter_->report(
          Diag_Optional_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses{
              .parameter_and_annotation = parameters[0]->span(),
              .question = *optional_question_span,
              .type_colon = *type_colon_span,
          });
    } else if (optional_question_span.has_value()) {
      this->diag_reporter_->report(
          Diag_Optional_Arrow_Parameter_Requires_Parentheses{
              .parameter_and_question = parameters[0]->span(),
              .question = *optional_question_span,
          });
    } else if (type_colon_span.has_value()) {
      this->diag_reporter_->report(
          Diag_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses{
              .parameter_and_annotation = parameters[0]->span(),
              .type_colon = *type_colon_span,
          });
    }

    if (this->peek().type != Token_Type::equal_greater && is_await) {
      this->lexer_.roll_back_transaction(std::move(transaction));
      if (parameter_is_await) {
        this->diag_reporter_->report(Diag_Redundant_Await{
            .await_operator = async_or_await_token.span(),
        });
      }
      goto await_operator;
    }
    this->lexer_.commit_transaction(std::move(transaction));
    return parse_arrow_function_arrow_and_body(std::move(parameters),
                                               /*parameter_visits=*/nullptr,
                                               /*return_type_visits=*/nullptr);
  }

  // async function f(parameters) { statements; }
  // await function f() {}
  case Token_Type::kw_function: {
    if (is_await) {
      goto await_operator;
    }
    Expression* function = this->parse_function_expression(
        v, Function_Attributes::async, async_begin);
    return function;
  }

  // async  // Identifier (variable reference).
  // await p
  // await []
  await_operator:
  variable_reference:
  default:
    if (is_async) {
      Expression* ast = this->make_expression<Expression::Variable>(
          async_or_await_token.identifier_name(), async_or_await_token.type);
      return ast;
    } else if (is_await) {
      if (!(this->in_async_function_ || this->in_top_level_)) {
        this->diag_reporter_->report(Diag_Await_Operator_Outside_Async{
            .await_operator = async_or_await_token.span(),
        });
      }

      Expression* child = this->parse_expression(
          v, Precedence{
                 .binary_operators = true,
                 .math_or_logical_or_assignment = false,
                 .equals_assignment = false,
                 .commas = false,
                 .in_operator = false,
                 .colon_type_annotation = Allow_Type_Annotations::never,
                 .trailing_curly_is_arrow_body = false,
                 .conditional_operator = false,
             });
      if (child->kind() == Expression_Kind::Missing) {
        this->diag_reporter_->report(Diag_Missing_Operand_For_Operator{
            .where = async_or_await_token.span(),
        });
      }
      return this->make_expression<Expression::Await>(
          child, async_or_await_token.span());
    }
    QLJS_UNREACHABLE();
  }

  QLJS_UNREACHABLE();
}

Expression* Parser::parse_await_expression(Parse_Visitor_Base& v,
                                           const Token& await_token,
                                           Precedence prec) {
  bool is_identifier = [&]() -> bool {
    if (this->in_async_function_ ||
        (this->in_top_level_ &&
         this->options_.top_level_await_mode ==
             Parser_Top_Level_Await_Mode::await_operator)) {
      return false;
    } else {
      // await is a unary operator (in modules) or an identifier (in scripts).
      switch (this->peek().type) {
      QLJS_CASE_BINARY_ONLY_OPERATOR:
      QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL:
      QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
      case Token_Type::colon:
      case Token_Type::comma:
      case Token_Type::dot:
      case Token_Type::end_of_file:
      case Token_Type::equal:
      case Token_Type::equal_greater:
      case Token_Type::kw_in:
      case Token_Type::question:
      case Token_Type::question_dot:
      case Token_Type::right_curly:
      case Token_Type::right_paren:
      case Token_Type::right_square:
      case Token_Type::semicolon:
        return true;

      // await <x>y</x>/g;  // operator
      // await < other;     // identifier
      // await /regexp/;    // operator
      // await / rhs;       // identifier
      // await !x;          // operator
      // await!.prop;       // identifier (TypeScript)
      case Token_Type::bang:
      case Token_Type::less:
      case Token_Type::slash:
      case Token_Type::slash_equal: {
        Parse_Expression_Cache_Key cache_key =
            this->parse_expression_cache_key_for_current_state();
        auto cache_it = this->await_is_identifier_cache_.find(cache_key);
        if (cache_it != this->await_is_identifier_cache_.end()) {
          return cache_it->second;
        }

        Parser_Transaction transaction = this->begin_transaction();

        bool parsed_ok = this->catch_fatal_parse_errors([&] {
          if (this->in_top_level_) {
            // Try to parse the / as a regular expression literal or the < as a
            // JSX element.
            [[maybe_unused]] Expression* ast =
                this->parse_expression(Null_Visitor::instance, prec);
          } else {
            // Try to parse the / or < as a binary operator.
            [[maybe_unused]] Expression* ast = this->parse_expression_remainder(
                Null_Visitor::instance,
                this->make_expression<Expression::Variable>(
                    await_token.identifier_name(), await_token.type),
                prec);
          }
        });
        parsed_ok = parsed_ok && transaction.reporter.empty() &&
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

      case Token_Type::kw_of:
        // HACK(strager): This works around for-of parsing. Remove this case
        // when for-of parsing is fixed.
        [[fallthrough]];
      case Token_Type::minus_minus:
      case Token_Type::plus_plus:
        // TODO(strager): Parse 'await--x' and 'await--;' correctly.
        [[fallthrough]];
      case Token_Type::complete_template:
      case Token_Type::incomplete_template:
      case Token_Type::left_square:
      case Token_Type::minus:
      case Token_Type::plus:
        return !this->in_top_level_;

      // class A extends await(){} // Identifier. See NOTE[extends-await-paren].
      // await (x);                // Ambiguous.
      case Token_Type::left_paren:
        return prec.in_class_extends_clause || !this->in_top_level_;

      // class A extends await { }  // Identifier.
      // await {};                  // Operator.
      case Token_Type::left_curly:
        return prec.in_class_extends_clause;

      case Token_Type::dot_dot_dot:
      case Token_Type::identifier:
      case Token_Type::kw_as:
      case Token_Type::kw_async:
      case Token_Type::kw_await:
      case Token_Type::kw_from:
      case Token_Type::kw_function:
      case Token_Type::kw_get:
      case Token_Type::kw_let:
      case Token_Type::kw_set:
      case Token_Type::kw_static:
      case Token_Type::kw_yield:
      case Token_Type::number:
      case Token_Type::private_identifier:
      case Token_Type::regexp:
      case Token_Type::reserved_keyword_with_escape_sequence:
      case Token_Type::string:
      case Token_Type::tilde:
      default:
        return false;
      }
    }
  }();

  if (is_identifier) {
    return this->make_expression<Expression::Variable>(
        await_token.identifier_name(), await_token.type);
  } else {
    return this->parse_async_expression_only(v, await_token, /*prec=*/prec);
  }
}

Expression* Parser::parse_expression_remainder(Parse_Visitor_Base& v,
                                               Expression* ast,
                                               Precedence prec) {
  if (prec.commas) {
    QLJS_ASSERT(prec.binary_operators);
  }

  Binary_Expression_Builder binary_builder(this->expressions_.allocator(), ast);

next:
  switch (this->peek().type) {
  // x, y, z  // Sequence operator or parameter separator.
  case Token_Type::comma: {
    if (!prec.commas) {
      break;
    }
    Source_Code_Span comma_span = this->peek().span();
    this->skip();

    if (this->peek().type == Token_Type::right_paren) {
      // Probably an arrow function with a trailing comma in its parameter list:
      // (parameters, go, here, ) => expression-or-block
      return this->make_expression<Expression::Trailing_Comma>(
          binary_builder.move_expressions(this->expressions_), comma_span);
    } else {
      // Comma expression: a, b, c
      // FIXME(strager): Should we overwrite the precedence here? Overwriting
      // has caused bugs before.
      Expression* rhs = binary_builder.add_child(
          comma_span,
          this->parse_expression(
              v,
              Precedence{
                  .commas = false,
                  .colon_question_is_typescript_optional_with_type_annotation =
                      prec.colon_question_is_typescript_optional_with_type_annotation,
              }));
      if (rhs->kind() == Expression_Kind::Invalid ||
          rhs->kind() == Expression_Kind::Missing) {
        this->diag_reporter_->report(
            Diag_Missing_Operand_For_Operator{comma_span});
      }
    }
    goto next;
  }

    // x + y
  binary_operator:
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_LESS_LESS_AND_STAR:
  case Token_Type::kw_instanceof:
  case Token_Type::minus:
  case Token_Type::plus:
  case Token_Type::slash:
  case Token_Type::star: {
    if (!prec.math_or_logical_or_assignment) {
      break;
    }
    bool allow_unary_lhs = this->peek().type != Token_Type::star_star;
    Expression* maybe_unary_lhs = binary_builder.last_expression();
    Expression_Kind lhs_kind = maybe_unary_lhs->kind();
    Source_Code_Span operator_span = this->peek().span();
    this->skip();

    Expression* rhs = binary_builder.add_child(
        operator_span,
        this->parse_expression(
            v, Precedence{.binary_operators = false, .commas = false}));

    if (!allow_unary_lhs) {
      switch (lhs_kind) {
      // -a ** b  // Invalid.
      // void a ** b  // Invalid.
      case Expression_Kind::Unary_Operator: {
        auto* lhs =
            expression_cast<Expression::Unary_Operator*>(maybe_unary_lhs);
        if (lhs->is_void_operator()) {
          // void a ** b  // Invalid.
          this->diag_reporter_->report(
              Diag_Missing_Parentheses_Around_Exponent_With_Unary_Lhs{
                  .exponent_expression = Source_Code_Span(
                      lhs->child_->span_begin(), rhs->span_end()),
                  .unary_operator = Source_Code_Span(
                      lhs->unary_operator_begin_,
                      lhs->unary_operator_begin_ + u8"void"_sv.size()),
              });
        } else {
          // -a ** b  // Invalid.
          // ~a ** b  // Invalid.
          this->diag_reporter_->report(
              Diag_Missing_Parentheses_Around_Unary_Lhs_Of_Exponent{
                  .unary_expression = maybe_unary_lhs->span(),
                  .exponent_operator = operator_span,
              });
        }
        break;
      }

      // delete a ** b  // Invalid.
      case Expression_Kind::Delete: {
        auto* lhs = expression_cast<Expression::Delete*>(maybe_unary_lhs);
        this->diag_reporter_->report(
            Diag_Missing_Parentheses_Around_Exponent_With_Unary_Lhs{
                .exponent_expression = Source_Code_Span(
                    lhs->child_->span_begin(), rhs->span_end()),
                .unary_operator = lhs->unary_operator_span(),
            });
        break;
      }

      // typeof a ** b  // Invalid.
      case Expression_Kind::Typeof: {
        auto* lhs = expression_cast<Expression::Typeof*>(maybe_unary_lhs);
        this->diag_reporter_->report(
            Diag_Missing_Parentheses_Around_Exponent_With_Unary_Lhs{
                .exponent_expression = Source_Code_Span(
                    lhs->child_->span_begin(), rhs->span_end()),
                .unary_operator = lhs->unary_operator_span(),
            });
        break;
      }

      default:
        break;
      }
    }
    if (rhs->kind() == Expression_Kind::Missing) {
      this->diag_reporter_->report(
          Diag_Missing_Operand_For_Operator{operator_span});
    }
    goto next;
  }

  // x < y
  // f<x>()              // TypeScript only.
  // f<<T>() => void>()  // TypeScript only.
  case Token_Type::less:
  case Token_Type::less_less:
    if (this->options_.typescript) {
      // class C< <T>() => RetType> // Valid.
      // class C<<T>() => RetType>  // Invalid.
      if (prec.in_class_extends_clause &&
          this->peek().type == Token_Type::less_less) {
        this->diag_reporter_->report(
            Diag_TypeScript_Generic_Less_Less_Not_Split{
                .expected_space =
                    Source_Code_Span::unit(this->peek().begin + 1),
                .context = Statement_Kind::class_extends_clause,
            });
      }

      bool parsed_as_generic_arguments;
      Stacked_Buffering_Visitor generic_arguments_visits =
          this->buffering_visitor_stack_.push();

      this->try_parse(
          [&](Parser_Transaction&) {
            bool parsed_without_fatal_error = this->catch_fatal_parse_errors(
                [this, &generic_arguments_visits] {
                  this->parse_and_visit_typescript_generic_arguments(
                      generic_arguments_visits.visitor());
                });
            if (!parsed_without_fatal_error) {
              return false;
            }
            switch (this->peek().type) {
            QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
            QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
            case Token_Type::colon:
            case Token_Type::comma:
            case Token_Type::complete_template:
            case Token_Type::dot:
            case Token_Type::end_of_file:
            case Token_Type::incomplete_template:
            case Token_Type::kw_break:
            case Token_Type::kw_case:
            case Token_Type::kw_const:
            case Token_Type::kw_continue:
            case Token_Type::kw_debugger:
            case Token_Type::kw_default:
            case Token_Type::kw_do:
            case Token_Type::kw_else:
            case Token_Type::kw_enum:
            case Token_Type::kw_export:
            case Token_Type::kw_for:
            case Token_Type::kw_if:
            case Token_Type::kw_import:
            case Token_Type::kw_return:
            case Token_Type::kw_switch:
            case Token_Type::kw_throw:
            case Token_Type::kw_try:
            case Token_Type::kw_var:
            case Token_Type::kw_while:
            case Token_Type::kw_with:
            case Token_Type::left_paren:
            case Token_Type::question:
            case Token_Type::question_dot:
            case Token_Type::right_curly:
            case Token_Type::right_paren:
            case Token_Type::right_square:
            case Token_Type::semicolon:
              parsed_as_generic_arguments = true;
              return true;

            // class A extends B<C> {}
            // class A extends B<C> implements I {}
            case Token_Type::left_curly:
            case Token_Type::kw_implements:
              if (prec.in_class_extends_clause) {
                parsed_as_generic_arguments = true;
                return true;
              } else {
                return false;
              }

            // C<T> = rhs;
            // C<T>= rhs;   // Invalid.
            // A<B<C<D>>> = rhs;
            // A<B<C<D>>>= rhs;   // Invalid.
            case Token_Type::equal: {
              bool was_split_token = this->peek().begin[-1] == u8'>';
              if (was_split_token) {
                // NOTE[typescript-generic-expression-token-splitting]:
                // parse_and_visit_typescript_generic_arguments split '>=' into
                // '>' and '=', or split '>>>=' into '>' and '>' and '>' and
                // '='. TypeScript's compiler does not split in this situation,
                // so it does not treat the code as having a generic argument
                // list.

                this->diag_reporter_->report(
                    Diag_TypeScript_Requires_Space_Between_Greater_And_Equal{
                        .greater_equal = Source_Code_Span(
                            this->peek().begin - 1, this->peek().end),
                    });
              }

              parsed_as_generic_arguments = true;
              return true;
            }

            default:
              return false;
            }
          },
          [&] { parsed_as_generic_arguments = false; });
      if (parsed_as_generic_arguments) {
        generic_arguments_visits.visitor().move_into(v);
        goto next;
      } else {
        goto binary_operator;
      }
    } else {
      goto binary_operator;
    }

  // f(x, y, z)  // Function call.
  case Token_Type::left_paren: {
    if (binary_builder.last_expression()->kind() ==
        Expression_Kind::Arrow_Function) {
      // () => {} /*ASI*/ (x)
      // () => {}()            // Invalid.
      if (this->peek().has_leading_newline) {
        // ASI; stop parsing the expression.
        break;
      }
      auto func_span = binary_builder.last_expression()->span();
      auto func_start_span = Source_Code_Span::unit(func_span.begin());
      this->diag_reporter_->report(
          Diag_Missing_Parentheses_Around_Self_Invoked_Function{
              .invocation = this->peek().span(),
              .func_start = func_start_span});
    }
    Expression* callee = binary_builder.last_expression();
    Expression::Call* call = this->parse_call_expression_remainder(v, callee);
    binary_builder.replace_last(call);

    if (this->peek().type == Token_Type::equal_greater) {
      bool is_async_arrow_using_with_await_operator =
          callee->kind() == Expression_Kind::Variable &&
          callee->variable_identifier_token_type() == Token_Type::kw_await;
      if (is_async_arrow_using_with_await_operator) {
        // await (x) => {}   // Invalid.
        // await () => expr  // Invalid.
        this->skip();
        this->diag_reporter_->report(Diag_Await_Followed_By_Arrow_Function{
            .await_operator = callee->span(),
        });
        // Parse as if the user wrote 'async' instead of 'await'.
        Expression* arrow_function = this->parse_arrow_function_body(
            v, /*attributes=*/Function_Attributes::async,
            /*parameter_list_begin=*/call->left_paren_span().begin(),
            /*prec=*/prec,
            /*parameters=*/
            Expression_Arena::Array_Ptr<Expression*>(
                call->children_.begin() + 1,  // Drop the callee.
                call->children_.end()),
            /*return_type_visits=*/nullptr);
        binary_builder.replace_last(arrow_function);
        goto next;
      }
      goto arrow_function;
    }

    goto next;
  }

  // f().prop = other
  case Token_Type::equal:
    if (!prec.equals_assignment) {
      break;
    }
    [[fallthrough]];
  // x += y
  // x[y] &&= z
  QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
  QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR : {
    if (!prec.math_or_logical_or_assignment) {
      break;
    }
    Expression_Kind kind;
    switch (this->peek().type) {
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
      kind = Expression_Kind::Compound_Assignment;
      break;
    QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
      kind = Expression_Kind::Conditional_Assignment;
      break;
    case Token_Type::equal:
      kind = Expression_Kind::Assignment;
      break;
    default:
      QLJS_UNREACHABLE();
    }
    Source_Code_Span operator_span = this->peek().span();
    this->skip();
    Expression* lhs = this->build_expression(binary_builder);
    this->check_assignment_lhs(lhs);
    Expression* rhs = this->parse_expression(
        v, Precedence{
               .commas = false,
               .in_operator = prec.in_operator,
               .colon_type_annotation = prec.colon_type_annotation,
           });
    if (rhs->kind() == Expression_Kind::Missing) {
      this->diag_reporter_->report(Diag_Missing_Operand_For_Operator{
          .where = operator_span,
      });
    }
    binary_builder.reset_after_build(
        this->make_expression<Expression::Assignment>(kind, lhs, rhs,
                                                      operator_span));
    goto next;
  }

  // x.y
  case Token_Type::dot: {
    Source_Code_Span dot_span = this->peek().span();
    this->skip();
    switch (this->peek().type) {
    case Token_Type::identifier:
    case Token_Type::private_identifier:
    case Token_Type::reserved_keyword_with_escape_sequence:
    QLJS_CASE_KEYWORD:
      if (this->peek().type == Token_Type::private_identifier &&
          !this->in_class_) {
        this->diag_reporter_->report(
            Diag_Cannot_Access_Private_Identifier_Outside_Class{
                .private_identifier = this->peek().span(),
            });
      }
      binary_builder.replace_last(this->make_expression<Expression::Dot>(
          binary_builder.last_expression(), this->peek().identifier_name()));
      this->skip();
      goto next;

    case Token_Type::string: {
      this->diag_reporter_->report(Diag_Invalid_Rhs_For_Dot_Operator{
          .dot = dot_span,
      });
      binary_builder.add_child(
          dot_span,
          this->make_expression<Expression::Literal>(this->peek().span()));
      this->skip();
      goto next;
    }

    QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL:
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
    QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
    case Token_Type::colon:
    case Token_Type::comma:
    case Token_Type::end_of_file:
    case Token_Type::equal:
    case Token_Type::left_paren:
    case Token_Type::less:
    case Token_Type::minus:
    case Token_Type::plus:
    case Token_Type::question:
    case Token_Type::right_paren:
    case Token_Type::semicolon: {
      Source_Code_Span empty_property_name(dot_span.end(), dot_span.end());
      binary_builder.replace_last(this->make_expression<Expression::Dot>(
          binary_builder.last_expression(), Identifier(empty_property_name)));
      this->diag_reporter_->report(Diag_Missing_Property_Name_For_Dot_Operator{
          .dot = dot_span,
      });
      goto next;
    }

    // x .. y
    case Token_Type::dot: {
      Source_Code_Span second_dot = this->peek().span();
      this->diag_reporter_->report(Diag_Dot_Dot_Is_Not_An_Operator{
          .dots = Source_Code_Span(dot_span.begin(), second_dot.end()),
      });
      // Treat '..' as if it was a binary operator.
      this->skip();
      binary_builder.add_child(
          second_dot,
          this->parse_expression(
              v, Precedence{.binary_operators = false, .commas = false}));
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
  case Token_Type::question_dot: {
    this->skip();
    switch (this->peek().type) {
    // x?.y
    case Token_Type::identifier:
    case Token_Type::private_identifier:
    case Token_Type::reserved_keyword_with_escape_sequence:
    QLJS_CASE_KEYWORD:
      binary_builder.replace_last(this->make_expression<Expression::Dot>(
          binary_builder.last_expression(), this->peek().identifier_name()));
      this->skip();
      goto next;

    // tag?.`template`
    // tag?.`template${goes}here`
    case Token_Type::complete_template:
    case Token_Type::incomplete_template:
      binary_builder.replace_last(
          this->parse_tagged_template(v, binary_builder.last_expression()));
      goto next;

    // f?.(x, y)
    case Token_Type::left_paren:
      binary_builder.replace_last(this->parse_call_expression_remainder(
          v, binary_builder.last_expression()));
      goto next;

    // f?.<T>(x, y)                      // TypeScript only
    // foo?.<<Param>() => ReturnType>()  // TypeScript only
    case Token_Type::less:
    case Token_Type::less_less:
      if (!this->options_.typescript) {
        const Char8* less_begin = this->peek().begin;
        this->diag_reporter_->report(
            Diag_TypeScript_Generics_Not_Allowed_In_JavaScript{
                .opening_less = Source_Code_Span(less_begin, less_begin + 1),
            });
      }
      this->parse_and_visit_typescript_generic_arguments(v);
      binary_builder.replace_last(this->parse_call_expression_remainder(
          v, binary_builder.last_expression()));
      goto next;

    // array?.[index]
    case Token_Type::left_square:
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
  case Token_Type::left_square: {
    binary_builder.replace_last(this->parse_index_expression_remainder(
        v, binary_builder.last_expression()));
    goto next;
  }

  // x++  // Suffix unary operator.
  case Token_Type::minus_minus:
  case Token_Type::plus_plus:
    if (this->peek().has_leading_newline) {
      // Newline is not allowed before suffix ++ or --. Let
      // parse_and_visit_statement insert a semicolon for us.
      break;
    } else {
      Source_Code_Span operator_span = this->peek().span();
      this->skip();
      binary_builder.replace_last(
          this->make_expression<Expression::RW_Unary_Suffix>(
              binary_builder.last_expression(), operator_span));
    }
    goto next;

  // x!  // TypeScript only
  case Token_Type::bang: {
    if (this->peek().has_leading_newline) {
      // Newline is not allowed before suffix !. Let parse_and_visit_statement
      // insert a semicolon for us.
      break;
    } else {
      Source_Code_Span bang_span = this->peek().span();
      this->skip();
      Source_Code_Span whitespace_after_bang(bang_span.end(),
                                             this->peek().begin);
      if (this->peek().type == Token_Type::equal_equal &&
          is_plain_horizontal_whitespace(whitespace_after_bang)) {
        // x! == y
        if (this->options_.typescript) {
          this->diag_reporter_->report(
              Diag_Bang_Equal_Equal_Interpreted_As_Non_Null_Assertion{
                  .unexpected_space = whitespace_after_bang,
                  .bang = bang_span,
              });
        } else {
          this->diag_reporter_->report(
              Diag_Unexpected_Space_Between_Bang_And_Equal_Equal{
                  .unexpected_space = whitespace_after_bang,
              });
          // Parse as 'x == y' by skipping the '!'.
          goto next;
        }
      } else if (!this->options_.typescript) {
        this->diag_reporter_->report(
            Diag_TypeScript_Non_Null_Assertion_Not_Allowed_In_JavaScript{
                .bang = bang_span,
            });
      }
      binary_builder.replace_last(
          this->make_expression<Expression::Non_Null_Assertion>(
              binary_builder.last_expression(), bang_span));
      goto next;
    }
  }

  // key in o
  case Token_Type::kw_in: {
    if (!prec.in_operator) {
      break;
    }
    Source_Code_Span in_operator = this->peek().span();
    this->skip();
    binary_builder.add_child(in_operator, this->parse_expression(v, prec));
    goto next;
  }

  // x ? y : z  // Conditional operator.
  // x?         // TypeScript only.
  case Token_Type::question: {
    if (!prec.conditional_operator) {
      break;
    }
    Source_Code_Span question_span = this->peek().span();
    this->skip();

    bool is_optional;
    switch (this->peek().type) {
    // function(param?)                  // TypeScript only.
    // function(param? = init)           // Invalid.
    // function(param?, ...otherParams)  // TypeScript only.
    // param? => {}                      // Invalid.
    // let [x?] = y;                     // Invalid.
    // let {p: x?} = y;                  // Invalid.
    case Token_Type::comma:
    case Token_Type::equal:
    case Token_Type::equal_greater:
    case Token_Type::right_curly:
    case Token_Type::right_paren:
    case Token_Type::right_square:
      is_optional = true;
      break;

    // function(param?: ParamType)
    case Token_Type::colon:
      is_optional =
          prec.colon_question_is_typescript_optional_with_type_annotation;
      break;

    default:
      is_optional = false;
      break;
    }
    if (is_optional) {
      Expression* lhs = binary_builder.last_expression();
      binary_builder.replace_last(
          this->make_expression<Expression::Optional>(lhs, question_span));
      goto next;
    }

    Expression* condition = this->build_expression(binary_builder);

    Expression* true_expression;
    if (this->peek().type == Token_Type::colon) {
      this->diag_reporter_->report(Diag_Missing_Operand_For_Operator{
          .where = question_span,
      });
      true_expression =
          this->make_expression<Expression::Missing>(Source_Code_Span(
              this->lexer_.end_of_previous_token(), this->peek().begin));
    } else {
      true_expression = this->parse_expression(
          v, Precedence{
                 .colon_type_annotation = Allow_Type_Annotations::
                     typescript_only_if_legal_as_conditional_true_branch,
             });
    }

    if (this->peek().type != Token_Type::colon) {
      Source_Code_Span expected_colon(this->lexer_.end_of_previous_token(),
                                      this->lexer_.end_of_previous_token());
      this->diag_reporter_->report(Diag_Missing_Colon_In_Conditional_Expression{
          .expected_colon = expected_colon,
          .question = question_span,
      });
      Expression* false_expression =
          this->make_expression<Expression::Missing>(expected_colon);
      return this->make_expression<Expression::Conditional>(
          condition, true_expression, false_expression);
    }
    Source_Code_Span colon_span = this->peek().span();
    this->skip();

    Expression* false_expression = this->parse_expression(v, prec);
    if (false_expression->kind() == Expression_Kind::Missing) {
      this->diag_reporter_->report(Diag_Missing_Operand_For_Operator{
          .where = colon_span,
      });
    }

    return this->make_expression<Expression::Conditional>(
        condition, true_expression, false_expression);
  }

  // (parameters, go, here) => expression-or-block // Arrow function.
  arrow_function:
  case Token_Type::equal_greater: {
    Source_Code_Span arrow_span = this->peek().span();
    this->skip();
    Expression* lhs = binary_builder.last_expression();
    switch (this->validate_arrow_function_parameter_list(lhs, arrow_span)) {
    case Arrow_Function_Parameter_List_Validation::ok:
    case Arrow_Function_Parameter_List_Validation::error:
      break;
    case Arrow_Function_Parameter_List_Validation::
        equal_greater_looks_like_operator:
      // Treat the '=>' as if it was a binary operator (like '>=').
      binary_builder.add_child(
          arrow_span,
          this->parse_expression(
              v, Precedence{.binary_operators = false, .commas = false}));
      goto next;
    }

    if (binary_builder.has_multiple_children()) {
      // TODO(strager): We should report an error for code like this:
      // a + b => c
    }

    Buffering_Visitor* return_type_visits = nullptr;
    if (lhs->kind() == Expression_Kind::Type_Annotated) {
      Expression::Type_Annotated* annotated =
          expression_cast<Expression::Type_Annotated*>(lhs);
      if (annotated->child_->kind() == Expression_Kind::Paren ||
          annotated->child_->kind() == Expression_Kind::Paren_Empty) {
        // (): ReturnType => {}  // TypeScript only.
        // (param): ReturnType => {}  // TypeScript only.
        return_type_visits = &annotated->type_visits_;
        lhs = annotated->child_;
      }
    }
    binary_builder.replace_last(this->parse_arrow_function_expression_remainder(
        v, /*generic_parameter_visits=*/nullptr, lhs,
        /*return_type_visits=*/return_type_visits,
        /*prec=*/prec));
    goto next;
  }

  // html`<h1>My Website</h1>  // Template call.
  // html`<h1>${title}</h1>`   // Template call.
  case Token_Type::complete_template:
  case Token_Type::incomplete_template: {
    Expression* tag = binary_builder.last_expression();
    binary_builder.replace_last(this->parse_tagged_template(v, tag));
    goto next;
  }

  // x y    // Invalid.
  //
  // x    // ASI.
  // y
  case Token_Type::identifier:
    if (prec.trailing_identifiers) {
      const Char8* expected_operator = this->lexer_.end_of_previous_token();
      this->diag_reporter_->report(Diag_Unexpected_Identifier_In_Expression{
          .unexpected = this->peek().span(),
      });

      // Behave as if a comma appeared before the identifier.
      Expression* rhs = binary_builder.add_child(
          Source_Code_Span::unit(expected_operator),
          this->parse_expression(
              v, Precedence{.binary_operators = false, .commas = false}));
      QLJS_ASSERT(rhs->kind() != Expression_Kind::Invalid);
      goto next;
    }
    break;

  case Token_Type::left_curly: {
    auto has_comma_operator = [&binary_builder]() -> bool {
      Expression::Binary_Operator* expr =
          expression_cast<Expression::Binary_Operator*>(
              binary_builder.last_expression()->without_paren());
      bool comma = false;
      for (int i = 0; i < expr->children_.size() - 1; i++) {
        if (expr->operator_spans_[i].string_view() == u8","_sv) {
          comma = true;
        }
      }
      return comma;
    };
    bool looks_like_arrow_function_body =
        prec.trailing_curly_is_arrow_body &&
        !binary_builder.has_multiple_children() &&
        ((  // multiple identifiers: (a, b)
             !this->peek().has_leading_newline &&
             binary_builder.last_expression()->without_paren()->kind() ==
                 Expression_Kind::Binary_Operator &&
             has_comma_operator()) ||
         (  // one identifier: (a)
             binary_builder.last_expression()->kind() ==
                 Expression_Kind::Paren &&
             binary_builder.last_expression()->without_paren()->kind() ==
                 Expression_Kind::Variable) ||
         (  // no identifier: ()
             binary_builder.last_expression()->kind() ==
             Expression_Kind::Paren_Empty));
    if (looks_like_arrow_function_body) {
      Source_Code_Span arrow_span = this->peek().span();
      // (a, b) { return a + b; }  // Invalid.
      this->diag_reporter_->report(
          Diag_Missing_Arrow_Operator_In_Arrow_Function{
              .where = arrow_span,
          });
      binary_builder.replace_last(
          this->parse_arrow_function_expression_remainder(
              v,
              /*generic_parameter_visits=*/nullptr,
              binary_builder.last_expression(),
              /*return_type_visits=*/nullptr,
              /*prec=*/prec));
    }
    break;
  }

  // x: Type  // TypeScript only.
  case Token_Type::colon:
    switch (prec.colon_type_annotation) {
    case Allow_Type_Annotations::typescript_only:
      if (this->options_.typescript) {
        goto parse_colon_type_annotation;
      }
      break;

    parse_colon_type_annotation:
    case Allow_Type_Annotations::always: {
      Expression* child = binary_builder.last_expression();
      Source_Code_Span colon_span = this->peek().span();
      Buffering_Visitor type_visitor(&this->type_expression_memory_);
      bool is_possibly_arrow_function_return_type_annotation =
          child->kind() == Expression_Kind::Paren ||
          child->kind() == Expression_Kind::Paren_Empty;
      this->parse_and_visit_typescript_colon_type_expression(
          type_visitor,
          TypeScript_Type_Parse_Options{
              .allow_assertion_signature_or_type_predicate =
                  is_possibly_arrow_function_return_type_annotation,
              .stop_parsing_type_at_newline_before_generic_arguments =
                  prec.stop_parsing_type_at_newline_before_generic_arguments_in_type_annotation,
              .stop_parsing_type_at_newline_after_asserts = false,
              .stop_parsing_type_at_newline_before_is = false,
          });
      const Char8* type_end = this->lexer_.end_of_previous_token();
      binary_builder.replace_last(
          this->make_expression<Expression::Type_Annotated>(
              child, colon_span, std::move(type_visitor), type_end));
      goto next;
    }

    case Allow_Type_Annotations::never:
      break;

    // cond ? (t)    : param   => body
    // cond ? (param): RetType => body : f
    // cond ? (t)    : f
    case Allow_Type_Annotations::
        typescript_only_if_legal_as_conditional_true_branch: {
      Expression* child = binary_builder.last_expression();
      if (!(this->options_.typescript &&
            child->kind() == Expression_Kind::Paren)) {
        break;
      }
      Buffering_Visitor type_visitor(&this->type_expression_memory_);
      Parser_Transaction transaction = this->begin_transaction();

      bool parsed_ok = this->catch_fatal_parse_errors([&] {
        this->parse_and_visit_typescript_colon_type_expression(
            type_visitor,
            TypeScript_Type_Parse_Options{
                // TODO(strager): Should we set
                // stop_parsing_type_at_newline_before_generic_arguments
                // to false?
            });
      });
      if (parsed_ok && this->peek().type == Token_Type::equal_greater) {
        // cond ? (t)    : param   => body
        // cond ? (param): RetType => body : f
        //                         ^^

        // TODO(strager): We should call validate_arrow_function_parameter_list,
        // similar to if we did 'goto arrow_function;'.

        this->skip();
        Stacked_Buffering_Visitor arrow_body_visits =
            this->buffering_visitor_stack_.push();
        Expression* arrow_function =
            this->parse_arrow_function_expression_remainder(
                arrow_body_visits.visitor(),
                /*generic_parameter_visits=*/nullptr, child,
                /*return_type_visits=*/&type_visitor, /*prec=*/prec);

        if (this->peek().type == Token_Type::colon) {
          // (cond ? (param): RetType => body : f)
          //                                  ^
          // We correctly interpreted ': RetType' as a return type annotation.
          this->commit_transaction(std::move(transaction));
          arrow_body_visits.visitor().move_into(v);
          binary_builder.replace_last(arrow_function);
          break;
        }
      }

      // (cond ? (x) : param => body)
      // (cond ? (x) : param)
      //
      // We incorrectly interpreted ': param' as a return type annotation.
      this->roll_back_transaction(std::move(transaction));
      break;
    }
    }
    break;

  // x   // ASI
  // as
  //
  // x as Type    // TypeScript only.
  // {} as const  // TypeScript only.
  case Token_Type::kw_as:
  case Token_Type::kw_satisfies: {
    if (this->peek().has_leading_newline) {
      // ASI. End this expression.
      break;
    }

    Source_Code_Span operator_span = this->peek().span();
    if (!this->options_.typescript) {
      switch (this->peek().type) {
      case Token_Type::kw_as:
        this->diag_reporter_->report(
            Diag_TypeScript_As_Type_Assertion_Not_Allowed_In_JavaScript{
                .as_keyword = operator_span,
            });
        break;
      case Token_Type::kw_satisfies:
        this->diag_reporter_->report(
            Diag_TypeScript_Satisfies_Not_Allowed_In_JavaScript{
                .satisfies_keyword = operator_span,
            });
        break;
      default:
        QLJS_UNREACHABLE();
      }
    }
    bool is_as = this->peek().type == Token_Type::kw_as;
    this->skip();

    bool is_as_const = is_as && this->peek().type == Token_Type::kw_const;
    if (is_as_const) {
      // {} as const
      this->skip();
    } else {
      // x as Type
      // x satisfies Type
      this->parse_and_visit_typescript_type_expression(
          v,
          TypeScript_Type_Parse_Options{
              // A trailing '?' might be the start of a conditional expression:
              // x as Type ? y : z
              .parse_question_as_invalid = false,
          });
    }
    const Char8* type_end = this->lexer_.end_of_previous_token();

    Expression* child = binary_builder.last_expression();
    if (is_as_const) {
      this->error_on_invalid_as_const(
          child,
          /*as_const_span=*/Source_Code_Span(operator_span.begin(), type_end));
    }
    Expression* expr =
        is_as ? this->make_expression<Expression::As_Type_Assertion>(
                    child, operator_span, type_end)
              : this->make_expression<Expression::Satisfies>(
                    child, operator_span, type_end);
    binary_builder.replace_last(expr);
    goto next;
  }

  QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD:
  case Token_Type::at:
  case Token_Type::end_of_file:
  case Token_Type::kw_accessor:
  case Token_Type::kw_async:
  case Token_Type::kw_await:
  case Token_Type::kw_break:
  case Token_Type::kw_case:
  case Token_Type::kw_class:
  case Token_Type::kw_const:
  case Token_Type::kw_continue:
  case Token_Type::kw_debugger:
  case Token_Type::kw_default:
  case Token_Type::kw_delete:
  case Token_Type::kw_do:
  case Token_Type::kw_else:
  case Token_Type::kw_enum:
  case Token_Type::kw_export:
  case Token_Type::kw_false:
  case Token_Type::kw_for:
  case Token_Type::kw_from:
  case Token_Type::kw_function:
  case Token_Type::kw_get:
  case Token_Type::kw_if:
  case Token_Type::kw_implements:
  case Token_Type::kw_import:
  case Token_Type::kw_interface:
  case Token_Type::kw_let:
  case Token_Type::kw_new:
  case Token_Type::kw_null:
  case Token_Type::kw_of:
  case Token_Type::kw_package:
  case Token_Type::kw_private:
  case Token_Type::kw_protected:
  case Token_Type::kw_public:
  case Token_Type::kw_return:
  case Token_Type::kw_set:
  case Token_Type::kw_static:
  case Token_Type::kw_super:
  case Token_Type::kw_switch:
  case Token_Type::kw_this:
  case Token_Type::kw_throw:
  case Token_Type::kw_true:
  case Token_Type::kw_try:
  case Token_Type::kw_typeof:
  case Token_Type::kw_var:
  case Token_Type::kw_void:
  case Token_Type::kw_while:
  case Token_Type::kw_with:
  case Token_Type::kw_yield:
  case Token_Type::number:
  case Token_Type::private_identifier:
  case Token_Type::right_curly:
  case Token_Type::right_paren:
  case Token_Type::right_square:
  case Token_Type::semicolon:
  case Token_Type::string:
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  return this->build_expression(binary_builder);
}

Parser::Arrow_Function_Parameter_List_Validation
Parser::validate_arrow_function_parameter_list(
    Expression* parameters_expression, Source_Code_Span arrow_span) {
  Expression* lhs = parameters_expression;
  Expression* lhs_without_paren = lhs->without_paren();
  switch (lhs_without_paren->kind()) {
  // f(x, y) => {}      // Invalid.
  // f(x, y) => {}      // Invalid.
  // f.x => z           // Invalid.
  // 42 => {}           // Invalid.
  case Expression_Kind::Call:
  case Expression_Kind::Dot:
  case Expression_Kind::Literal: {
    if (lhs_without_paren->kind() == Expression_Kind::Call) {
      if (this->peek().type == Token_Type::left_curly) {
        // f(x, y) => {}
        // Diagnostic reporting is done by
        // parse_arrow_function_expression_remainder.
        return Arrow_Function_Parameter_List_Validation::error;
      } else {
        // f(x, y) => x+y
      }
    }

    Source_Code_Span lhs_span = lhs->span();
    switch (lhs_without_paren->kind()) {
    case Expression_Kind::Call:
    case Expression_Kind::Dot:
      this->diag_reporter_->report(Diag_Unexpected_Arrow_After_Expression{
          .arrow = arrow_span,
          .expression = lhs_span,
      });
      break;
    case Expression_Kind::Literal:
      this->diag_reporter_->report(Diag_Unexpected_Arrow_After_Literal{
          .arrow = arrow_span,
          .literal_parameter = lhs_span,
      });
      break;
    default:
      QLJS_UNREACHABLE();
    }

    if (this->peek().type != Token_Type::left_curly) {
      return Arrow_Function_Parameter_List_Validation::
          equal_greater_looks_like_operator;
    }
    return Arrow_Function_Parameter_List_Validation::error;
  }

  default:
    return Arrow_Function_Parameter_List_Validation::ok;
  }
}

Expression* Parser::parse_arrow_function_expression_remainder(
    Parse_Visitor_Base& v, Buffering_Visitor* generic_parameter_visits,
    Expression* parameters_expression, Buffering_Visitor* return_type_visits,
    Precedence prec) {
  const Char8* parameter_list_begin = nullptr;
  if (parameters_expression->kind() == Expression_Kind::Paren) {
    parameter_list_begin = parameters_expression->span_begin();
    parameters_expression =
        expression_cast<Expression::Paren*>(parameters_expression)->child_;
  }

  Expression_Arena::Vector<Expression*> parameters(
      "parse_arrow_function_expression_remainder",
      this->expressions_.allocator());
  switch (parameters_expression->kind()) {
  case Expression_Kind::Binary_Operator:
  case Expression_Kind::Trailing_Comma:
    // TODO(strager): Only allow comma expressions, not '(2+3) => 5', for
    // example.
    for (Expression* parameter : parameters_expression->children()) {
      parameters.emplace_back(parameter);
    }
    break;

  case Expression_Kind::Class:
  case Expression_Kind::Delete:
  case Expression_Kind::Invalid:
  case Expression_Kind::Missing:
  case Expression_Kind::New:
  case Expression_Kind::Template:
  case Expression_Kind::Typeof:
  case Expression_Kind::Angle_Type_Assertion:
  case Expression_Kind::Arrow_Function:
  case Expression_Kind::As_Type_Assertion:
  case Expression_Kind::Await:
  case Expression_Kind::Compound_Assignment:
  case Expression_Kind::Conditional:
  case Expression_Kind::Conditional_Assignment:
  case Expression_Kind::Function:
  case Expression_Kind::Index:
  case Expression_Kind::JSX_Element:
  case Expression_Kind::JSX_Element_With_Members:
  case Expression_Kind::JSX_Element_With_Namespace:
  case Expression_Kind::JSX_Fragment:
  case Expression_Kind::Named_Function:
  case Expression_Kind::New_Target:
  case Expression_Kind::Non_Null_Assertion:
  case Expression_Kind::Private_Variable:
  case Expression_Kind::RW_Unary_Prefix:
  case Expression_Kind::RW_Unary_Suffix:
  case Expression_Kind::Satisfies:
  case Expression_Kind::Super:
  case Expression_Kind::Tagged_Template_Literal:
  case Expression_Kind::This_Variable:
  case Expression_Kind::Unary_Operator:
  case Expression_Kind::Yield_Many:
  case Expression_Kind::Yield_None:
  case Expression_Kind::Yield_One:
    // The code is invalid. An error is reported elsewhere.
    [[fallthrough]];
  // x => {}
  // (...args) => {}
  // ({start, end}) => {}
  case Expression_Kind::Array:
  case Expression_Kind::Assignment:
  case Expression_Kind::Object:
  case Expression_Kind::Spread:
  case Expression_Kind::Variable:
    parameters.emplace_back(parameters_expression);
    break;

  // (param?) => {}  // TypeScript only.
  // param? => {}    // Invalid.
  case Expression_Kind::Optional:
    if (!parameter_list_begin) {
      // param? => {}  // Invalid.
      Expression::Optional* param =
          expression_cast<Expression::Optional*>(parameters_expression);
      this->diag_reporter_->report(
          Diag_Optional_Arrow_Parameter_Requires_Parentheses{
              .parameter_and_question = param->span(),
              .question = param->question_span(),
          });
    }
    parameters.emplace_back(parameters_expression);
    break;

  // param: Type => {}    // Invalid.
  // param?: Type => {}   // Invalid.
  // (param: Type) => {}  // TypeScript only.
  case Expression_Kind::Type_Annotated: {
    // NOTE(strager): '(param): ReturnType => {}' is handled above.
    if (!parameter_list_begin) {
      Expression::Type_Annotated* param =
          expression_cast<Expression::Type_Annotated*>(parameters_expression);
      if (param->child_->kind() == Expression_Kind::Optional) {
        // param?: Type => {}  // Invalid.
        Expression::Optional* optional =
            expression_cast<Expression::Optional*>(param->child_);
        this->diag_reporter_->report(
            Diag_Optional_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses{
                .parameter_and_annotation = param->span(),
                .question = optional->question_span(),
                .type_colon = param->colon_span(),
            });
      } else {
        // param: Type => {}  // Invalid.
        this->diag_reporter_->report(
            Diag_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses{
                .parameter_and_annotation = param->span(),
                .type_colon = param->colon_span(),
            });
      }
    }
    parameters.emplace_back(parameters_expression);
    break;
  }

  // ((x)) => {}  // Invalid.
  case Expression_Kind::Paren: {
    auto paren = expression_cast<Expression::Paren*>(parameters_expression);
    this->diag_reporter_->report(
        Diag_Unexpected_Function_Parameter_Is_Parenthesized{
            .left_paren_to_right_paren = paren->span()});
    break;
  }

  // () => {}
  // (()) => {}  // Invalid.
  case Expression_Kind::Paren_Empty: {
    Expression::Paren_Empty* paren_empty =
        expression_cast<Expression::Paren_Empty*>(parameters_expression);
    if (parameter_list_begin) {
      // (()) => {}  // Invalid.
      paren_empty->report_missing_expression_error(this->diag_reporter_);
    } else {
      // () => {}
      parameter_list_begin = paren_empty->span_.begin();
    }
    break;
  }

  // f(x, y) => {}
  case Expression_Kind::Call: {
    auto* call = expression_cast<Expression::Call*>(parameters_expression);
    if (this->peek().type == Token_Type::left_curly) {
      parameter_list_begin = call->left_paren_span().begin();
      for (Span_Size i = 1; i < call->child_count(); ++i) {
        parameters.emplace_back(call->child(i));
      }
      // We will report
      // Diag_Missing_Operator_Between_Expression_And_Arrow_Function
      // elsewhere.
    } else {
      // Diag_Unexpected_Arrow_After_Expression is reported elsewhere.
      parameter_list_begin = parameters_expression->span_begin();
    }
    break;
  }

  case Expression_Kind::Dot:
  case Expression_Kind::Literal:
    // The code is invalid. An error is reported elsewhere.
    parameter_list_begin = parameters_expression->span_begin();
    break;

  case Expression_Kind::Import:
    break;
  }

  v.visit_enter_function_scope();
  if (generic_parameter_visits) {
    std::move(*generic_parameter_visits).move_into(v);
  }
  Expression* arrow_function = this->parse_arrow_function_body_no_scope(
      v, /*attributes=*/Function_Attributes::normal,
      /*parameter_list_begin=*/parameter_list_begin,
      /*prec=*/prec, this->expressions_.make_array(std::move(parameters)),
      /*return_type_visits=*/return_type_visits);
  arrow_function = this->maybe_wrap_erroneous_arrow_function(
      arrow_function, /*parameters_expression=*/parameters_expression);
  v.visit_exit_function_scope();
  return arrow_function;
}

Expression::Call* Parser::parse_call_expression_remainder(Parse_Visitor_Base& v,
                                                          Expression* callee) {
  Source_Code_Span left_paren_span = this->peek().span();
  Expression_Arena::Vector<Expression*> call_children(
      "parse_expression_remainder call children",
      this->expressions_.allocator());
  call_children.reserve(4);
  call_children.emplace_back(callee);
  this->skip();
  while (this->peek().type != Token_Type::right_paren) {
    if (this->peek().type == Token_Type::comma) {
      this->diag_reporter_->report(
          Diag_Extra_Comma_Not_Allowed_Between_Arguments{
              .comma = this->peek().span(),
          });
      this->skip();
      continue;
    }
    call_children.emplace_back(this->parse_expression(
        v, Precedence{.commas = false, .trailing_identifiers = true}));
    if (this->peek().type != Token_Type::comma) {
      break;
    }
    this->skip();
  }
  const Char8* call_span_end;
  if (this->peek().type == Token_Type::right_paren) {
    call_span_end = this->peek().end;
    this->skip();
  } else {
    // { f(x }  // Invalid.
    // f(x;     // Invalid.
    call_span_end = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(Diag_Expected_Right_Paren_For_Function_Call{
        .expected_right_paren = Source_Code_Span::unit(call_span_end),
        .left_paren = left_paren_span,
    });
  }
  return expression_cast<Expression::Call*>(
      this->make_expression<Expression::Call>(
          this->expressions_.make_array(std::move(call_children)),
          /*left_paren_span=*/left_paren_span,
          /*span_end=*/call_span_end));
}

Expression* Parser::parse_index_expression_remainder(Parse_Visitor_Base& v,
                                                     Expression* lhs) {
  QLJS_ASSERT(this->peek().type == Token_Type::left_square);
  Source_Code_Span left_square_span = this->peek().span();
  this->skip();
  Expression* subscript =
      this->parse_expression(v, Precedence{.trailing_identifiers = true});
  const Char8* end;
  switch (this->peek().type) {
  case Token_Type::right_square:
    if (subscript->kind() == Expression_Kind::Missing) {
      // expr[]  // Invalid.
      Source_Code_Span right_square_span = this->peek().span();
      this->diag_reporter_->report(Diag_Indexing_Requires_Expression{
          .squares = Source_Code_Span(left_square_span.begin(),
                                      right_square_span.end()),
      });
    }
    end = this->peek().end;
    this->skip();
    break;
  case Token_Type::end_of_file:
  default:
    this->diag_reporter_->report(
        Diag_Unmatched_Indexing_Bracket{.left_square = left_square_span});
    end = this->lexer_.end_of_previous_token();
    break;
  }
  return this->make_expression<Expression::Index>(lhs, subscript,
                                                  left_square_span, end);
}

Expression_Arena::Vector<Expression*>
Parser::parse_arrow_function_parameters_or_call_arguments(
    Parse_Visitor_Base& v) {
  Expression_Arena::Vector<Expression*> parameters(
      "parse_arrow_function_parameters_or_call_arguments parameters",
      this->expressions_.allocator());
  while (this->peek().type != Token_Type::right_paren) {
    if (this->peek().type == Token_Type::comma) {
      // TODO(strager): Emit a different error if this is an arrow function.
      // Diag_Extra_Comma_Not_Allowed_Between_Arguments only makes sense if
      // this is a function call.
      this->diag_reporter_->report(
          Diag_Extra_Comma_Not_Allowed_Between_Arguments{
              .comma = this->peek().span(),
          });
      this->skip();
      continue;
    }
    parameters.emplace_back(this->parse_expression(
        v, Precedence{
               .commas = false,
               .colon_question_is_typescript_optional_with_type_annotation =
                   this->options_.typescript,
           }));
    if (this->peek().type != Token_Type::comma) {
      break;
    }
    this->skip();
  }
  return parameters;
}

Expression* Parser::parse_arrow_function_body(
    Parse_Visitor_Base& v, Function_Attributes attributes,
    const Char8* parameter_list_begin, Precedence prec,
    Expression_Arena::Array_Ptr<Expression*>&& parameters,
    Buffering_Visitor* return_type_visits) {
  v.visit_enter_function_scope();
  Expression* arrow = this->parse_arrow_function_body_no_scope(
      v, attributes, parameter_list_begin, prec, std::move(parameters),
      return_type_visits);
  v.visit_exit_function_scope();
  return arrow;
}

Expression* Parser::parse_arrow_function_body_no_scope(
    Parse_Visitor_Base& v, Function_Attributes attributes,
    const Char8* parameter_list_begin, Precedence prec,
    Expression_Arena::Array_Ptr<Expression*>&& parameters,
    Buffering_Visitor* return_type_visits) {
  Function_Guard guard = this->enter_function(attributes);

  for (Expression* parameter : parameters) {
    this->visit_binding_element(
        parameter, v,
        Binding_Element_Info{
            .declaration_kind = Variable_Kind::_arrow_parameter,
            .declaring_token = std::nullopt,
            .flags = Variable_Declaration_Flags::none,
        });
  }
  if (return_type_visits) {
    std::move(*return_type_visits).move_into(v);
  }
  v.visit_enter_function_scope_body();

  if (this->peek().type == Token_Type::left_curly) {
    this->parse_and_visit_statement_block_no_scope(v);
  } else {
    this->parse_and_visit_expression(
        // FIXME(strager): Should we inherit other things from prec?
        v, Precedence{
               .commas = false,
               .in_operator = prec.in_operator,
               .colon_type_annotation = prec.colon_type_annotation,
           });
  }

  const Char8* span_end = this->lexer_.end_of_previous_token();
  return this->make_expression<Expression::Arrow_Function>(
      attributes, std::move(parameters), parameter_list_begin, span_end);
}

Expression* Parser::parse_function_expression(Parse_Visitor_Base& v,
                                              Function_Attributes attributes,
                                              const Char8* span_begin) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_function);
  this->skip();
  this->parse_generator_star(&attributes);

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
  std::optional<Identifier> function_name = std::nullopt;
  QLJS_WARNING_POP
  switch (this->peek().type) {
  case Token_Type::kw_await:
  case Token_Type::kw_yield:
    // NOTE(strager): A function expression named 'await' or 'yield' is allowed
    // even within async functions and generator functions.
    [[fallthrough]];
  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    [[fallthrough]];
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case Token_Type::identifier:
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
          ? std::optional<Source_Code_Span>(function_name->span())
          : std::nullopt,
      attributes, Parameter_List_Options());
  v.visit_exit_function_scope();

  const Char8* span_end = this->lexer_.end_of_previous_token();
  return function_name.has_value()
             ? this->make_expression<Expression::Named_Function>(
                   attributes, *function_name,
                   Source_Code_Span(span_begin, span_end))
             : this->make_expression<Expression::Function>(
                   attributes, Source_Code_Span(span_begin, span_end));
}

Expression* Parser::parse_object_literal(Parse_Visitor_Base& v) {
  QLJS_ASSERT(this->peek().type == Token_Type::left_curly);
  const Char8* left_curly_begin = this->peek().begin;
  const Char8* right_curly_end;
  this->skip();

  Expression_Arena::Vector<Object_Property_Value_Pair> entries(
      "parse_object_literal entries", this->expressions_.allocator());
  auto parse_value_expression = [&](Expression* property) -> void {
    Expression* value =
        this->parse_expression(v, Precedence{
                                      .equals_assignment = false,
                                      .commas = false,
                                  });
    if (this->peek().type == Token_Type::equal) {
      const Char8* equal_begin = this->peek().begin;
      this->skip();
      Expression* init = this->parse_expression(v, Precedence{.commas = false});
      entries.emplace_back(property, value, /*init=*/init,
                           /*init_equal_begin=*/equal_begin);
    } else {
      entries.emplace_back(property, value);
    }
  };
  auto parse_computed_property_name = [this, &v]() -> Expression* {
    QLJS_ASSERT(this->peek().type == Token_Type::left_square);
    this->skip();
    Expression* property_name = this->parse_expression(v);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_square);
    this->skip();
    return property_name;
  };
  auto parse_equal = [&](const Token& key_token, Expression* key) {
    Expression* lhs;
    bool missing_key;
    switch (key_token.type) {
    case Token_Type::number:
    case Token_Type::string:
      lhs = this->make_expression<Expression::Literal>(key_token.span());
      missing_key = true;
      break;
    default:
      lhs = this->make_expression<Expression::Variable>(
          key_token.identifier_name(), key_token.type);
      missing_key = false;
      break;
    }

    QLJS_ASSERT(this->peek().type == Token_Type::equal);
    const Char8* equal_begin = this->peek().begin;
    this->skip();

    Expression* rhs = this->parse_expression(v, Precedence{.commas = false});
    if (missing_key) {
      this->diag_reporter_->report(Diag_Missing_Key_For_Object_Entry{
          .expression = Source_Code_Span(lhs->span_begin(), rhs->span_end()),
      });
    }
    entries.emplace_back(key, lhs, /*init=*/rhs,
                         /*init_equal_begin=*/equal_begin);
  };
  auto parse_method_entry = [&](const Char8* key_span_begin, Expression* key,
                                Function_Attributes attributes) -> void {
    switch (this->peek().type) {
    default: {
      this->parse_and_visit_function_parameters_and_body(
          v,
          /*name=*/
          Source_Code_Span(key_span_begin,
                           this->lexer_.end_of_previous_token()),
          attributes, Parameter_List_Options());
      break;
    }
    case Token_Type::right_curly:
      this->diag_reporter_->report(Diag_Missing_Function_Parameter_List{
          .expected_parameter_list =
              Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
      });
      break;
    }

    const Char8* span_end = this->lexer_.end_of_previous_token();
    Expression* func = this->make_expression<Expression::Function>(
        attributes, Source_Code_Span(key_span_begin, span_end));
    entries.emplace_back(key, func);
  };

  bool expect_comma_or_end = false;
  for (;;) {
    if (this->peek().type == Token_Type::right_curly) {
      right_curly_end = this->peek().end;
      this->skip();
      break;
    }
    if (this->peek().type == Token_Type::comma) {
      this->skip();
      expect_comma_or_end = false;
      continue;
    }
    if (this->peek().type == Token_Type::less ||
        this->peek().type == Token_Type::semicolon) {
      // { k1: v1; k2() {}< k3: v3 }  // Invalid.
      this->diag_reporter_->report(
          Diag_Expected_Comma_To_Separate_Object_Literal_Entries{
              .unexpected_token = this->peek().span(),
          });
      this->skip();
      expect_comma_or_end = false;
      continue;
    }
    switch (this->peek().type) {
    // ({x) // Invalid.
    case Token_Type::end_of_file:
    case Token_Type::right_paren:
    case Token_Type::right_square:
      right_curly_end = this->lexer_.end_of_previous_token();
      this->diag_reporter_->report(Diag_Unclosed_Object_Literal{
          .object_open =
              Source_Code_Span(left_curly_begin, left_curly_begin + 1),
          .expected_object_close = Source_Code_Span::unit(right_curly_end),
      });
      goto done;

    default:
      break;
    }
    if (expect_comma_or_end) {
      this->diag_reporter_->report(
          Diag_Missing_Comma_Between_Object_Literal_Entries{
              .where =
                  Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
          });
    }

  parse_entry:
    switch (this->peek().type) {
    case Token_Type::comma:
    case Token_Type::end_of_file:
    case Token_Type::right_curly:
    case Token_Type::semicolon:
      QLJS_ASSERT(false);
      break;

    // {#key: value}
    case Token_Type::private_identifier:
      this->diag_reporter_->report(
          Diag_Private_Properties_Are_Not_Allowed_In_Object_Literals{
              .private_identifier = this->peek().span(),
          });
      [[fallthrough]];
    // {key: value}
    // {"key": value}
    // {10: value}
    // {keyAndValue}
    QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET:
    QLJS_CASE_STRICT_RESERVED_KEYWORD:
    case Token_Type::identifier:
    case Token_Type::number:
    case Token_Type::reserved_keyword_with_escape_sequence:
    case Token_Type::string: {
      Token key_token = this->peek();
      Expression* key =
          this->make_expression<Expression::Literal>(key_token.span());
      this->skip();
      switch (this->peek().type) {
      // {x y}  // Invalid.
      // {function f() {}}  // Invalid.
      case Token_Type::identifier:
        if (key_token.type == Token_Type::kw_function) {
          this->diag_reporter_->report(
              Diag_Methods_Should_Not_Use_Function_Keyword{
                  .function_token = key_token.span(),
              });
          goto parse_entry;
        } else {
          // We'll report Diag_Missing_Comma_Between_Object_Literal_Entries on
          // the next iteration of the loop.
          goto single_token_key_and_value;
        }

      single_token_key_and_value:
      case Token_Type::comma:
      case Token_Type::right_curly:
      case Token_Type::semicolon: {
        // Name and value are the same: {keyandvalue}

        switch (key_token.type) {
        case Token_Type::number:
        case Token_Type::string: {
          Expression* value =
              this->make_expression<Expression::Missing>(key_token.span());
          this->diag_reporter_->report(
              Diag_Invalid_Lone_Literal_In_Object_Literal{key_token.span()});
          entries.emplace_back(key, value);
          break;
        }

        QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD : {
          Expression* value =
              this->make_expression<Expression::Missing>(key_token.span());
          this->diag_reporter_->report(
              Diag_Missing_Value_For_Object_Literal_Entry{
                  .key = key_token.span()});
          entries.emplace_back(key, value);
          break;
        }

        case Token_Type::kw_await:
        case Token_Type::kw_yield:
          // TODO(strager): Disallow referencing a variable named 'await' for
          // async functions, or a variable named 'yield' for generator
          // functions.
          goto single_token_key_and_value_identifier;

        single_token_key_and_value_identifier:
        QLJS_CASE_CONTEXTUAL_KEYWORD:
        case Token_Type::identifier: {
          Expression* value = this->make_expression<Expression::Variable>(
              key_token.identifier_name(), key_token.type);
          entries.emplace_back(key, value);
          break;
        }

        // { protected }
        QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
          // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
          goto single_token_key_and_value_identifier;

        // { \u{69}f }  // Invalid.
        case Token_Type::reserved_keyword_with_escape_sequence:
          key_token.report_errors_for_escape_sequences_in_keyword(
              this->diag_reporter_);
          goto single_token_key_and_value_identifier;

        // { #privateName }  // Invalid.
        case Token_Type::private_identifier:
          // We already reported an error. Ignore.
          break;

        default:
          QLJS_UNIMPLEMENTED();
          break;
        }
        break;
      }

      case Token_Type::colon:
        this->skip();
        parse_value_expression(key);
        break;

      case Token_Type::equal: {
        parse_equal(key_token, key);
        break;
      }

      // {x += y}  // Invalid.
      expression_without_key:
      QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_STAR:
      QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
      QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
      case Token_Type::dot:
      case Token_Type::minus:
      case Token_Type::minus_minus:
      case Token_Type::plus:
      case Token_Type::plus_plus:
      case Token_Type::question_dot: {
        Expression* lhs;
        switch (key_token.type) {
        case Token_Type::number:
        case Token_Type::string:
          lhs = this->make_expression<Expression::Literal>(key_token.span());
          break;
        default:
          lhs = this->make_expression<Expression::Variable>(
              key_token.identifier_name(), key_token.type);
          break;
        }
        Expression* value = this->parse_expression_remainder(
            v, lhs, Precedence{.commas = false});
        entries.emplace_back(key, value);
        this->diag_reporter_->report(Diag_Missing_Key_For_Object_Entry{
            .expression = value->span(),
        });
        break;
      }

      // { myVar< }          // Invalid.
      // { method<T>() {} }  // TypeScript only.
      case Token_Type::less:
        if (this->options_.typescript) {
          // { method<T>() {} }
          // TODO(strager): Recognize '<' as ',' sometimes in TypeScript. See
          // NOTE[fat-finger-comma].
          goto method;
        }
        // Treat '<' as if it was ','. We will report
        // Diag_Expected_Comma_To_Separate_Object_Literal_Entries later. See
        // NOTE[fat-finger-comma].
        // TODO[JavaScript-generic-method-in-object]: Report
        // Diag_TypeScript_Generics_Not_Allowed_In_JavaScript if this looks like
        // a generic method.
        goto single_token_key_and_value;

      method:
      case Token_Type::left_paren:
        parse_method_entry(key_token.begin, key, Function_Attributes::normal);
        break;

      case Token_Type::star:
        if (key_token.type == Token_Type::kw_function) {
          // { function *f() {} }  // Invalid.
          this->diag_reporter_->report(
              Diag_Methods_Should_Not_Use_Function_Keyword{
                  .function_token = key_token.span(),
              });
          this->skip();
          switch (this->peek().type) {
          QLJS_CASE_KEYWORD:
          case Token_Type::identifier:
          case Token_Type::number:
          case Token_Type::reserved_keyword_with_escape_sequence:
          case Token_Type::string: {
            Source_Code_Span real_key_span = this->peek().span();
            Expression* real_key =
                this->make_expression<Expression::Literal>(real_key_span);
            this->skip();
            parse_method_entry(real_key_span.begin(), real_key,
                               Function_Attributes::generator);
            break;
          }

          // { get [expr]() {} }
          case Token_Type::left_square: {
            Source_Code_Span left_square_span = this->peek().span();
            Expression* real_key = parse_computed_property_name();
            parse_method_entry(left_square_span.begin(), real_key,
                               Function_Attributes::generator);
            break;
          }

          default:
            QLJS_PARSER_UNIMPLEMENTED();
            break;
          }
        } else {
          Lexer_Transaction transaction = this->lexer_.begin_transaction();
          this->skip();
          if (this->peek().type == Token_Type::left_paren) {
            // {method*() {}}  // Invalid.
            this->lexer_.roll_back_transaction(std::move(transaction));
            parse_method_entry(key_token.begin, key,
                               Function_Attributes::normal);
          } else {
            this->skip();
            if (this->peek().type == Token_Type::left_paren) {
              // {someName *method() {}}  // Invalid.
              this->lexer_.roll_back_transaction(std::move(transaction));
              // We'll report Diag_Missing_Comma_Between_Object_Literal_Entries
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

      case Token_Type::right_paren:
      // {x  // Invalid.
      case Token_Type::end_of_file:
        // We'll report Diag_Unclosed_Object_Literal later when we look for the
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
    case Token_Type::kw_async:
    case Token_Type::kw_get:
    case Token_Type::kw_set: {
      bool is_async = this->peek().type == Token_Type::kw_async;
      Function_Attributes method_attributes =
          is_async ? Function_Attributes::async : Function_Attributes::normal;
      Token keyword_token = this->peek();
      Source_Code_Span keyword_span = this->peek().span();
      Token_Type keyword_type = this->peek().type;
      this->skip();

      if (this->peek().type == Token_Type::kw_function) {
        // { set function() { } }
        // { async function f() { } }  // Invalid.
        Lexer_Transaction transaction = this->lexer_.begin_transaction();
        Source_Code_Span function_keyword_span = this->peek().span();
        this->skip();
        switch (this->peek().type) {
        // { set function() { } }
        case Token_Type::left_paren:
          this->lexer_.roll_back_transaction(std::move(transaction));
          break;

        // { async function f() { } }  // Invalid.
        case Token_Type::identifier:
        default:
          this->lexer_.commit_transaction(std::move(transaction));
          this->diag_reporter_->report(
              Diag_Methods_Should_Not_Use_Function_Keyword{
                  .function_token = function_keyword_span,
              });
          break;
        }
      }

      if (is_async && this->peek().type == Token_Type::star) {
        // { async *generatorName() { } }
        method_attributes = is_async ? Function_Attributes::async_generator
                                     : Function_Attributes::generator;
        this->skip();
      }

      switch (this->peek().type) {
      // get #method() {}
      case Token_Type::private_identifier:
        this->diag_reporter_->report(
            Diag_Private_Properties_Are_Not_Allowed_In_Object_Literals{
                .private_identifier = this->peek().span(),
            });
        [[fallthrough]];
      // get method() {}
      QLJS_CASE_KEYWORD:
      case Token_Type::identifier:
      case Token_Type::number:
      case Token_Type::reserved_keyword_with_escape_sequence:
      case Token_Type::string: {
        Source_Code_Span key_span = this->peek().span();
        Expression* key = this->make_expression<Expression::Literal>(key_span);
        this->skip();
        parse_method_entry(keyword_span.begin(), key, method_attributes);
        break;
      }

      // { get [expr]() {} }
      case Token_Type::left_square: {
        Source_Code_Span left_square_span = this->peek().span();
        Expression* key = parse_computed_property_name();
        parse_method_entry(left_square_span.begin(), key, method_attributes);
        break;
      }

      case Token_Type::equal: {
        Expression* key =
            this->make_expression<Expression::Literal>(keyword_span);
        parse_equal(keyword_token, key);
        break;
      }

      // { get: value }
      // { async: value }
      case Token_Type::colon: {
        this->skip();
        Expression* key =
            this->make_expression<Expression::Literal>(keyword_span);
        parse_value_expression(key);
        break;
      }

      // { get() {} }
      method_named_async_get_set:
      case Token_Type::left_paren: {
        Expression* key =
            this->make_expression<Expression::Literal>(keyword_span);
        parse_method_entry(keyword_span.begin(), key,
                           Function_Attributes::normal);
        break;
      }

      // { get }
      end_after_async_get_set:
      case Token_Type::comma:
      case Token_Type::right_curly:
      case Token_Type::semicolon: {
        Expression* key =
            this->make_expression<Expression::Literal>(keyword_span);
        Expression* value = this->make_expression<Expression::Variable>(
            Identifier(keyword_span), keyword_type);
        entries.emplace_back(key, value);
        break;
      }

      // { get< }         // Invalid.
      // { get<T>() {} }  // TypeScript only.
      case Token_Type::less:
        if (this->options_.typescript) {
          // { get<T>() {} }
          // TODO(strager): Recognize '<' as ',' sometimes in TypeScript. See
          // NOTE[fat-finger-comma].
          goto method_named_async_get_set;
        }
        // Treat '<' as if it was ','. We will report
        // Diag_Expected_Comma_To_Separate_Object_Literal_Entries later. See
        // NOTE[fat-finger-comma].
        goto end_after_async_get_set;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }

    // {[keyExpression]: value}
    case Token_Type::left_square: {
      Source_Code_Span left_square_span = this->peek().span();
      Expression* key = parse_computed_property_name();
      switch (this->peek().type) {
      case Token_Type::colon:
        this->skip();
        parse_value_expression(key);
        break;

      case Token_Type::left_paren:
        parse_method_entry(left_square_span.begin(), key,
                           Function_Attributes::normal);
        break;

      case Token_Type::comma:
      case Token_Type::less:
      case Token_Type::right_curly:
      case Token_Type::semicolon: {
        Source_Code_Span key_span(left_square_span.begin(),
                                  this->lexer_.end_of_previous_token());
        Expression* value =
            this->make_expression<Expression::Missing>(key_span);
        this->diag_reporter_->report(
            Diag_Missing_Value_For_Object_Literal_Entry{.key = key_span});
        entries.emplace_back(key, value);
        break;
      }

      // {[key]*() {}}  // Invalid.
      case Token_Type::star:
        parse_method_entry(left_square_span.begin(), key,
                           Function_Attributes::normal);
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }
      break;
    }

    // *generatorMethod() {}
    case Token_Type::star: {
      this->skip();
      switch (this->peek().type) {
      // *#method() {}
      case Token_Type::private_identifier:
        this->diag_reporter_->report(
            Diag_Private_Properties_Are_Not_Allowed_In_Object_Literals{
                .private_identifier = this->peek().span(),
            });
        [[fallthrough]];
      // *method() {}
      QLJS_CASE_KEYWORD:
      case Token_Type::identifier:
      case Token_Type::number:
      case Token_Type::reserved_keyword_with_escape_sequence:
      case Token_Type::string: {
        Source_Code_Span method_name_span = this->peek().span();
        Expression* method_name =
            this->make_expression<Expression::Literal>(method_name_span);
        this->skip();
        parse_method_entry(method_name_span.begin(), method_name,
                           Function_Attributes::generator);
        break;
      }

      case Token_Type::left_square: {
        Source_Code_Span left_square_span = this->peek().span();
        Expression* key = parse_computed_property_name();
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::left_paren);
        parse_method_entry(left_square_span.begin(), key,
                           Function_Attributes::generator);
        break;
      }

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
      break;
    }

    // {...other}  // Spread operator.
    case Token_Type::dot_dot_dot:
      parse_value_expression(nullptr);
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    expect_comma_or_end = true;
  }
done:
  return this->make_expression<Expression::Object>(
      this->expressions_.make_array(std::move(entries)),
      Source_Code_Span(left_curly_begin, right_curly_end));
}

Expression* Parser::parse_class_expression(Parse_Visitor_Base& v) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_class);
  Source_Code_Span class_keyword_span = this->peek().span();

  v.visit_enter_class_scope();
  std::optional<Identifier> class_name = this->parse_class_and_optional_name();
  this->parse_and_visit_class_heading_after_name(v);
  v.visit_enter_class_scope_body(class_name);

  if (this->peek().type == Token_Type::left_curly) {
    this->parse_and_visit_class_body(
        v, Parse_Class_Body_Options{
               .class_or_interface_keyword_span = class_keyword_span,
               .is_abstract = false,
               .is_interface = false,
           });
  } else {
    this->diag_reporter_->report(Diag_Missing_Body_For_Class{
        .class_keyword_and_name_and_heritage = Source_Code_Span(
            class_keyword_span.begin(), this->lexer_.end_of_previous_token()),
    });
  }
  const Char8* span_end = this->lexer_.end_of_previous_token();

  v.visit_exit_class_scope();
  return this->make_expression<Expression::Class>(
      Source_Code_Span(class_keyword_span.begin(), span_end));
}

Expression* Parser::parse_jsx_or_typescript_generic_expression(
    Parse_Visitor_Base& v, Precedence prec) {
  QLJS_ASSERT(this->peek().type == Token_Type::less);

  // Disambiguate between:
  // <Type,>(params) => {}    // Arrow function.
  // <Component></Component>  // JSX element.
  if (this->options_.typescript) {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();

    // <const T,>(params) => {}  // Arrow function.
    // <const></const>           // Arrow function.
    // TODO(#690): Should we skip 'in' and 'out' too?
    bool had_modifier = false;
    if (this->peek().type == Token_Type::kw_const) {
      this->skip();
      had_modifier = true;
    }

    switch (this->peek().type) {
    // <(Type)>expr
    // < | Type>expr
    // < <T>(param) => ReturnType>expr
    case Token_Type::ampersand:
    case Token_Type::left_paren:
    case Token_Type::left_square:
    case Token_Type::less:
    case Token_Type::pipe:
      this->lexer_.roll_back_transaction(std::move(transaction));
      return this->parse_typescript_angle_type_assertion_expression(
          v, prec,
          /*is_invalid_due_to_jsx_ambiguity=*/false);

    // <{}>expr              // Cast.
    // <const {...props} />  // JSX element.
    case Token_Type::left_curly:
      if (!had_modifier) {
        this->lexer_.roll_back_transaction(std::move(transaction));
        return this->parse_typescript_angle_type_assertion_expression(
            v, prec,
            /*is_invalid_due_to_jsx_ambiguity=*/false);
      }
      break;

    // <Type>expr               // Type assertion.
    // <T,>(params) => {}       // Arrow function.
    // <Component></Component>  // JSX element.
    case Token_Type::identifier:
    case Token_Type::kw_any:
    case Token_Type::kw_bigint:
    case Token_Type::kw_boolean:
    case Token_Type::kw_never:
    case Token_Type::kw_null:
    case Token_Type::kw_number:
    case Token_Type::kw_object:
    case Token_Type::kw_string:
    case Token_Type::kw_symbol:
    case Token_Type::kw_undefined:
    case Token_Type::kw_unknown:
    case Token_Type::kw_void:
      this->skip();
      switch (this->peek().type) {
      // <T,>() => {}                 // Generic arrow function.
      // <T = U>(params) => {}        // Generic arrow function.
      case Token_Type::comma:
      case Token_Type::equal:
        this->lexer_.roll_back_transaction(std::move(transaction));
        return this->parse_typescript_generic_arrow_expression(v,
                                                               /*prec=*/prec);

      // <T extends U>(params) => {}  // Generic arrow function.
      // <T extends />                // JSX element.
      case Token_Type::kw_extends:
        this->skip();
        switch (this->peek().type) {
        // <T extends />
        case Token_Type::equal:
        case Token_Type::greater:
        case Token_Type::slash:
          this->lexer_.roll_back_transaction(std::move(transaction));
          break;
        // <T extends U>(params) => {}
        default:
          this->lexer_.roll_back_transaction(std::move(transaction));
          return this->parse_typescript_generic_arrow_expression(v,
                                                                 /*prec=*/prec);
        }
        break;

      // <ns.T>expr     // Type assertion.
      // <ns.T></ns.T>  // JSX element.
      case Token_Type::dot:
      case Token_Type::less:
      case Token_Type::less_less:
        if (!this->options_.jsx) {
          this->lexer_.roll_back_transaction(std::move(transaction));
          return this->parse_typescript_angle_type_assertion_expression(
              v, prec,
              /*is_invalid_due_to_jsx_ambiguity=*/false);
        }
        break;

      // <T>() => {}  // Generic arrow function.
      // <T>expr      // Type assertion.
      // <T></T>      // JSX element.
      case Token_Type::greater: {
        if (!this->options_.jsx) {
          this->lexer_.roll_back_transaction(std::move(transaction));
          return this->parse_typescript_angle_type_assertion_expression(
              v, prec,
              /*is_invalid_due_to_jsx_ambiguity=*/false);
        }
        this->skip();
        if (this->peek().type == Token_Type::left_paren) {
          const Char8* equal_greater =
              this->lexer_.find_equal_greater_in_jsx_children();
          if (equal_greater) {
            // <T> => </T>             // Invalid.
            // <T>(p: T): RT => p.m()  // Invalid.
            //
            // '>' is invalid in JSX text. Instead of complaining about the '>',
            // parse as a generic arrow function.
            this->lexer_.roll_back_transaction(std::move(transaction));
            return this->parse_typescript_angle_type_assertion_expression(
                v, prec,
                /*is_invalid_due_to_jsx_ambiguity=*/true);
          }
        }
        break;
      }

      // <T | U>expr  // Type assertion.
      // <T[]>expr    // Type assertion.
      case Token_Type::ampersand:
      case Token_Type::left_square:
      case Token_Type::pipe:
        this->lexer_.roll_back_transaction(std::move(transaction));
        return this->parse_typescript_angle_type_assertion_expression(
            v, prec,
            /*is_invalid_due_to_jsx_ambiguity=*/false);

      default:
        break;
      }
      break;

    // <readonly />        // JSX.
    // <readonly T[]>expr  // Type assertion.
    case Token_Type::kw_keyof:
    case Token_Type::kw_this:
    case Token_Type::kw_readonly:
    case Token_Type::kw_typeof:
    case Token_Type::kw_unique:
    case Token_Type::number:
    default:
      // For these uncommon cases, don't try hard to guess what the user
      // meant.
      this->lexer_.roll_back_transaction(std::move(transaction));
      if (this->options_.jsx) {
        return this->parse_jsx_expression(v);
      } else {
        return this->parse_typescript_angle_type_assertion_expression(
            v, prec,
            /*is_invalid_due_to_jsx_ambiguity=*/false);
      }
      break;

    // <>hello<>  // JSX.
    case Token_Type::greater:
      break;
    }
    this->lexer_.roll_back_transaction(std::move(transaction));
  }

  return this->parse_jsx_expression(v);
}

Expression* Parser::parse_jsx_expression(Parse_Visitor_Base& v) {
  QLJS_ASSERT(this->peek().type == Token_Type::less);

  if (!this->options_.jsx) {
    Source_Code_Span jsx_start = this->peek().span();
    if (this->options_.typescript) {
      this->diag_reporter_->report(
          Diag_JSX_Not_Allowed_In_TypeScript{.jsx_start = jsx_start});
    } else {
      this->diag_reporter_->report(
          Diag_JSX_Not_Allowed_In_JavaScript{.jsx_start = jsx_start});
    }
  }

  const Char8* jsx_begin = this->peek().begin;
  Expression* ast = this->parse_jsx_element_or_fragment(v);

  // Check for adjacent elements:
  // <div /><div />  // Invalid.
  if (this->peek().type == Token_Type::less) {
    const Char8* begin_of_second_element = this->peek().begin;
    Expression_Arena::Vector<Expression*> elements(
        "parse_jsx_expression adjacent elements",
        this->expressions_.allocator());
    elements.emplace_back(ast);
    do {
      elements.emplace_back(this->parse_jsx_element_or_fragment(v));
    } while (this->peek().type == Token_Type::less);
    const Char8* end = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(Diag_Adjacent_JSX_Without_Parent{
        .begin = Source_Code_Span::unit(jsx_begin),
        .begin_of_second_element =
            Source_Code_Span::unit(begin_of_second_element),
        .end = Source_Code_Span::unit(end),
    });
    ast = this->make_expression<Expression::JSX_Fragment>(
        /*span=*/Source_Code_Span(jsx_begin, end),
        /*children=*/this->expressions_.make_array(std::move(elements)));
  }

  return ast;
}

Expression* Parser::parse_jsx_element_or_fragment(Parse_Visitor_Base& v) {
  QLJS_ASSERT(this->peek().type == Token_Type::less);

  const Char8* jsx_begin = this->peek().begin;
  this->lexer_.skip_in_jsx();
  switch (this->peek().type) {
  // <div>
  case Token_Type::identifier: {
    Identifier tag = this->peek().identifier_name();
    this->lexer_.skip_in_jsx();
    Expression* ast = this->parse_jsx_element_or_fragment(
        v, /*tag=*/&tag, /*less_begin=*/jsx_begin);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::greater);
    this->skip();
    return ast;
  }

  // <> </>
  case Token_Type::greater: {
    Expression* ast = this->parse_jsx_element_or_fragment(
        v, /*tag=*/nullptr, /*less_begin=*/jsx_begin);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::greater);
    this->lexer_.skip();
    return ast;
  }

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

Expression* Parser::parse_jsx_element_or_fragment(Parse_Visitor_Base& v,
                                                  Identifier* tag,
                                                  const Char8* less_begin) {
  Depth_Guard d_guard(this);

  // If temp_tag_storage is nullopt, then there is no namespace. If
  // temp_tag_storage is not nullopt, then it stores the tag name.
  //
  // Invariant: temp_tag_storage.has_value() == (tag_namespace != nullptr)
  std::optional<Identifier> temp_tag_storage = std::nullopt;
  Identifier* tag_namespace = nullptr;

  // For <div>, this is empty. For <module.submodule.Component>, this contains
  // {"module", "submodule", "Component"}.
  Expression_Arena::Vector<Identifier> tag_members(
      "jsx_member_element parts", this->expressions_.allocator());

  Expression_Arena::Vector<Expression*> children(
      "jsx_element children", this->expressions_.allocator());

  auto make_jsx_expression = [&](const Char8* greater_end) -> Expression* {
    Source_Code_Span span(less_begin, greater_end);
    if (tag_namespace) {
      return this->make_expression<Expression::JSX_Element_With_Namespace>(
          /*span=*/span,
          /*ns=*/*tag_namespace,
          /*tag=*/*tag,
          /*children=*/this->expressions_.make_array(std::move(children)));
    } else if (tag) {
      return this->make_expression<Expression::JSX_Element>(
          /*span=*/span,
          /*tag=*/*tag,
          /*children=*/this->expressions_.make_array(std::move(children)));
    } else if (!tag_members.empty()) {
      return this->make_expression<Expression::JSX_Element_With_Members>(
          /*span=*/span,
          /*members=*/this->expressions_.make_array(std::move(tag_members)),
          /*children=*/this->expressions_.make_array(std::move(children)));
    } else {
      return this->make_expression<Expression::JSX_Fragment>(
          /*span=*/span,
          /*children=*/this->expressions_.make_array(std::move(children)));
    }
  };

  if (this->peek().type == Token_Type::colon) {
    // <namespace:current />
    if (!tag) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->lexer_.skip_in_jsx();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::identifier);
    temp_tag_storage = this->peek().identifier_name();
    tag_namespace = tag;
    tag = &*temp_tag_storage;
    this->lexer_.skip_in_jsx();
  } else if (this->peek().type == Token_Type::dot) {
    // <module.Component>
    this->lexer_.skip_in_jsx();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::identifier);
    tag_members.emplace_back(*tag);
    tag = nullptr;  // Just in case. We don't want to accidentally use 'tag'.
    tag_members.emplace_back(this->peek().identifier_name());
    this->lexer_.skip_in_jsx();
    while (this->peek().type == Token_Type::dot) {
      // <module.submodule.Component>
      this->lexer_.skip_in_jsx();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::identifier);
      tag_members.emplace_back(this->peek().identifier_name());
      this->lexer_.skip_in_jsx();
    }
  }
  const Char8* tag_end = this->lexer_.end_of_previous_token();

  // <Component<T> />  // TypeScript only.
  if (this->peek().type == Token_Type::less ||
      this->peek().type == Token_Type::less_less) {
    this->parse_and_visit_typescript_generic_arguments(v);
  }

  bool is_intrinsic =
      tag && (tag_namespace || Expression::JSX_Element::is_intrinsic(*tag));

next_attribute:
  switch (this->peek().type) {
  // NOTE[JSX-keyword-after-TypeScript-generic]: Normally, JS/TS keywords would
  // appear as Token_Type::identifier here. However, when parsing
  // '<C<T> class="" />' in TypeScript, the 'class' following '>' is parsed as a
  // keyword. This is because parse_and_visit_typescript_generic_arguments calls
  // this->skip() rather than this->skip_in_jsx(). That's why we need to check
  // for keywords here.
  QLJS_CASE_KEYWORD:
  case Token_Type::identifier: {
    Identifier attribute = this->peek().identifier_name();
    bool has_namespace = false;

    this->lexer_.skip_in_jsx();
    if (this->peek().type == Token_Type::colon) {
      // <current namespace:attribute>
      has_namespace = true;
      this->lexer_.skip_in_jsx();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::identifier);
      this->lexer_.skip_in_jsx();
    }
    if (is_intrinsic && !has_namespace && !tag_namespace) {
      this->check_jsx_attribute(attribute);
    }
    if (this->peek().type == Token_Type::equal) {
      this->lexer_.skip_in_jsx();
      switch (this->peek().type) {
      // <current attribute='value'>
      case Token_Type::string:
        this->lexer_.skip_in_jsx();
        break;

      // <current attribute={expression}>
      case Token_Type::left_curly: {
        const Char8* left_curly_brace = this->peek().begin;
        this->lexer_.skip();
        Expression* ast = this->parse_expression(v);
        if (ast->kind() == Expression_Kind::Missing) {
          const Char8* right_curly_brace = this->peek().end;
          this->diag_reporter_->report(Diag_JSX_Prop_Is_Missing_Expression{
              .left_brace_to_right_brace =
                  Source_Code_Span(left_curly_brace, right_curly_brace)});
        }
        children.emplace_back(ast);
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_curly);
        this->lexer_.skip_in_jsx();
        break;
      }

      // <current attribute=<Element />>
      case Token_Type::less:
        children.emplace_back(this->parse_jsx_element_or_fragment(v));
        break;

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
  case Token_Type::left_curly: {
    this->lexer_.skip();
    Expression* ast = this->parse_expression(v);
    if (ast->kind() != Expression_Kind::Spread) {
      this->diag_reporter_->report(Diag_Missing_Dots_For_Attribute_Spread{
          .expected_dots = Source_Code_Span::unit(ast->span_begin()),
      });
    }
    children.emplace_back(ast);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_curly);
    this->lexer_.skip_in_jsx();
    goto next_attribute;
  }

  // <current>
  case Token_Type::greater:
    this->lexer_.skip_in_jsx_children();
    goto next;

  // <current />
  case Token_Type::slash: {
    this->lexer_.skip_in_jsx();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::greater);
    const Char8* greater_end = this->peek().end;
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
  case Token_Type::less: {
    const Char8* child_begin = this->peek().begin;
    this->lexer_.skip_in_jsx();
    switch (this->peek().type) {
    // </current>
    // </namespace:current>
    case Token_Type::slash: {
      this->lexer_.skip_in_jsx();
      const Char8* closing_tag_begin = this->peek().begin;

      std::optional<Identifier> closing_tag_namespace;
      std::optional<Identifier> closing_tag;
      Expression_Arena::Vector<Identifier> closing_tag_members(
          "jsx closing_tag_members", this->expressions_.allocator());
      if (this->peek().type == Token_Type::identifier) {
        closing_tag = this->peek().identifier_name();
        this->lexer_.skip_in_jsx();
      }
      if (this->peek().type == Token_Type::colon) {
        // </namespace:current>
        this->lexer_.skip_in_jsx();
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::identifier);
        closing_tag_namespace = std::move(closing_tag);
        closing_tag = this->peek().identifier_name();
        this->lexer_.skip_in_jsx();
      }
      if (this->peek().type == Token_Type::dot) {
        // <module.submodule.Component>
        if (!closing_tag.has_value()) {
          QLJS_PARSER_UNIMPLEMENTED();
        }
        closing_tag_members.emplace_back(*closing_tag);
        closing_tag.reset();
        do {
          this->lexer_.skip_in_jsx();
          QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::identifier);
          closing_tag_members.emplace_back(this->peek().identifier_name());
          this->lexer_.skip_in_jsx();
        } while (this->peek().type == Token_Type::dot);
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
      if (!ranges_equal(tag_members, closing_tag_members,
                        [](const Identifier& tag_member,
                           const Identifier& closing_tag_member) {
                          return tag_member.normalized_name() ==
                                 closing_tag_member.normalized_name();
                        })) {
        mismatch = true;
      }
      if (mismatch) {
        Vector<Char8> opening_tag_name_pretty("opening_tag_name_pretty",
                                              &this->diagnostic_memory_);
        if (tag_namespace) {
          opening_tag_name_pretty += tag_namespace->span().string_view();
          opening_tag_name_pretty += u8':';
          QLJS_ASSERT(tag);
        }
        if (tag) {
          opening_tag_name_pretty += tag->span().string_view();
        }
        for (const Identifier& member : tag_members) {
          if (!opening_tag_name_pretty.empty()) {
            opening_tag_name_pretty += u8'.';
          }
          opening_tag_name_pretty += member.span().string_view();
        }

        const Char8* closing_tag_end = this->lexer_.end_of_previous_token();
        String8_View opening_tag_name_pretty_view(
            opening_tag_name_pretty.release_to_string_view());
        this->diag_reporter_->report(Diag_Mismatched_JSX_Tags{
            .opening_tag_name =
                tag_namespace
                    ? Source_Code_Span(tag_namespace->span().begin(), tag_end)
                    : !tag_members.empty()
                          ? Source_Code_Span(tag_members.front().span().begin(),
                                             tag_end)
                          : tag ? tag->span() : Source_Code_Span::unit(tag_end),
            .closing_tag_name =
                closing_tag_begin <= closing_tag_end
                    ? Source_Code_Span(closing_tag_begin, closing_tag_end)
                    :
                    // This happens for </> (fragment close).
                    Source_Code_Span::unit(closing_tag_begin),
            .opening_tag_name_pretty = opening_tag_name_pretty_view,
        });
      }

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::greater);
      const Char8* greater_end = this->peek().end;
      return make_jsx_expression(greater_end);
    }

      // <child>
    case Token_Type::identifier: {
      Identifier child_tag = this->peek().identifier_name();
      this->lexer_.skip_in_jsx();
      children.emplace_back(this->parse_jsx_element_or_fragment(
          v, /*tag=*/&child_tag, /*greater_begin=*/child_begin));

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::greater);
      this->lexer_.skip_in_jsx_children();
      goto next;
    }

      // <>
    case Token_Type::greater: {
      children.emplace_back(this->parse_jsx_element_or_fragment(
          v, /*tag=*/nullptr, /*greater_begin=*/child_begin));

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::greater);
      this->lexer_.skip_in_jsx_children();
      goto next;
    }

    default:
      QLJS_PARSER_UNIMPLEMENTED();
    }
    break;
  }

  // {expression}
  case Token_Type::left_curly: {
    this->skip();
    Expression* ast = this->parse_expression(v);
    children.emplace_back(ast);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_curly);
    this->lexer_.skip_in_jsx_children();
    goto next;
  }

  default:
    QLJS_PARSER_UNIMPLEMENTED();
  }

  QLJS_UNREACHABLE();
}

Expression* Parser::parse_typescript_generic_arrow_expression(
    Parse_Visitor_Base& v, Precedence prec) {
  const Char8* begin = this->peek().begin;

  v.visit_enter_function_scope();
  this->parse_and_visit_typescript_generic_parameters(v);

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::left_paren);
  this->skip();

  Expression_Arena::Vector<Expression*> parameters =
      this->parse_arrow_function_parameters_or_call_arguments(v);

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_paren);
  this->skip();

  Buffering_Visitor return_type_visits(&this->type_expression_memory_);
  if (this->peek().type == Token_Type::colon) {
    this->parse_and_visit_typescript_colon_type_expression(
        return_type_visits,
        TypeScript_Type_Parse_Options{
            .allow_assertion_signature_or_type_predicate = true,
        });
  }

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::equal_greater);
  this->skip();

  Expression* ast = this->parse_arrow_function_body_no_scope(
      v, Function_Attributes::normal, begin,
      /*prec=*/prec, this->expressions_.make_array(std::move(parameters)),
      /*return_type_visits=*/&return_type_visits);

  v.visit_exit_function_scope();

  return ast;
}

Expression* Parser::parse_typescript_angle_type_assertion_expression(
    Parse_Visitor_Base& v, Precedence prec,
    bool is_invalid_due_to_jsx_ambiguity) {
  QLJS_ASSERT(this->peek().type == Token_Type::less);
  const Char8* less_begin = this->peek().begin;
  this->skip();

  auto parse_as_type_assertion = [&]() -> Expression* {
    this->parse_and_visit_typescript_type_expression(v);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::greater);
    const Char8* greater_end = this->peek().end;
    Source_Code_Span bracketed_type_span(less_begin, greater_end);

    this->skip();

    Expression* ast = this->parse_primary_expression(v, prec);
    if (this->options_.jsx) {
      this->diag_reporter_->report(
          Diag_TypeScript_Angle_Type_Assertion_Not_Allowed_In_Tsx{
              .bracketed_type = bracketed_type_span,
              .expected_as =
                  Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
          });
    }
    return this->make_expression<Expression::Angle_Type_Assertion>(
        /*bracketed_type_span=*/bracketed_type_span,
        /*child=*/ast);
  };

  Lexer_Transaction transaction = this->lexer_.begin_transaction();
  switch (this->peek().type) {
  // <Type>expr
  // <T>(params) => {}
  case Token_Type::identifier: {
    Identifier type = this->peek().identifier_name();
    this->skip();
    switch (this->peek().type) {
    // <T | U>expr
    // <T[]>expr
    // <ns.T>expr
    case Token_Type::ampersand:
    case Token_Type::dot:
    case Token_Type::left_square:
    case Token_Type::less:
    case Token_Type::less_less:
    case Token_Type::pipe:
      this->lexer_.roll_back_transaction(std::move(transaction));
      return parse_as_type_assertion();

    // <T>(params) => {}
    // <Type>expr
    case Token_Type::greater: {
      this->lexer_.commit_transaction(std::move(transaction));
      Source_Code_Span bracketed_type_span(less_begin, this->peek().end);
      this->skip();

      Expression* ast = this->parse_primary_expression(v, prec);
      if (ast->kind() == Expression_Kind::Paren ||
          ast->kind() == Expression_Kind::Paren_Empty) {
        Buffering_Visitor return_type_visits(&this->type_expression_memory_);
        if (this->peek().type == Token_Type::colon) {
          this->parse_and_visit_typescript_colon_type_expression(
              return_type_visits,
              TypeScript_Type_Parse_Options{
                  .allow_assertion_signature_or_type_predicate = true,
              });
        }
        if (this->peek().type == Token_Type::equal_greater) {
          // <T>(param) => body

          if (is_invalid_due_to_jsx_ambiguity) {
            this->diag_reporter_->report(
                Diag_TypeScript_Generic_Arrow_Needs_Comma_In_JSX_Mode{
                    .generic_parameters_less =
                        Source_Code_Span(less_begin, less_begin + 1),
                    .expected_comma = Source_Code_Span::unit(type.span().end()),
                    .arrow = this->peek().span(),
                });
          }

          this->skip();
          Buffering_Visitor generic_parameter_visits(
              &this->type_expression_memory_);
          generic_parameter_visits.visit_variable_declaration(
              type, Variable_Kind::_generic_parameter,
              Variable_Declaration_Flags::none);
          return this->parse_arrow_function_expression_remainder(
              v,
              /*generic_parameter_visits=*/&generic_parameter_visits, ast,
              /*return_type_visits=*/&return_type_visits,
              /*prec=*/prec);
        }
      }

      v.visit_enter_type_scope();
      v.visit_variable_type_use(type);
      v.visit_exit_type_scope();
      return this->make_expression<Expression::Angle_Type_Assertion>(
          /*bracketed_type_span=*/bracketed_type_span,
          /*child=*/ast);
    }

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }

    return parse_as_type_assertion();
  }

  // <(Type)>expr
  // <typeof Type>expr
  case Token_Type::left_paren:
  default:
    return parse_as_type_assertion();
  }
}

Expression* Parser::parse_tagged_template(Parse_Visitor_Base& v,
                                          Expression* tag) {
  if (this->peek().type == Token_Type::complete_template) {
    Source_Code_Span template_span = this->peek().span();
    this->skip();
    return this->make_expression<Expression::Tagged_Template_Literal>(
        this->expressions_.make_array(&tag, &tag + 1), template_span);
  }

  const Char8* template_begin = this->peek().begin;
  Expression_Arena::Vector<Expression*> children(
      "parse_tagged_template children", this->expressions_.allocator());
  children.emplace_back(tag);
  for (;;) {
    QLJS_ASSERT(this->peek().type == Token_Type::incomplete_template);
    this->skip();
    children.emplace_back(this->parse_expression(v));
    switch (this->peek().type) {
    case Token_Type::right_curly:
      this->lexer_.skip_in_template(template_begin);
      switch (this->peek().type) {
      case Token_Type::complete_template: {
        const Char8* template_end = this->peek().end;
        this->skip();

        Expression_Arena::Array_Ptr<Expression*> children_array =
            this->expressions_.make_array(std::move(children));
        Source_Code_Span template_span(template_begin, template_end);
        return this->make_expression<Expression::Tagged_Template_Literal>(
            children_array, template_span);
      }

      case Token_Type::incomplete_template:
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

Expression* Parser::parse_untagged_template(Parse_Visitor_Base& v) {
  if (this->peek().type == Token_Type::complete_template) {
    QLJS_UNIMPLEMENTED();
  }

  const Char8* template_begin = this->peek().begin;
  Expression_Arena::Vector<Expression*> children(
      "parse_untagged_template children", this->expressions_.allocator());
  for (;;) {
    QLJS_ASSERT(this->peek().type == Token_Type::incomplete_template);
    this->peek().report_errors_for_escape_sequences_in_template(
        this->diag_reporter_);
    this->skip();
    if (this->peek().type == Token_Type::right_curly) {
      this->diag_reporter_->report(Diag_Expected_Expression_In_Template_Literal{
          .placeholder = Source_Code_Span(this->lexer_.end_of_previous_token(),
                                          this->peek().begin)});
    } else {
      children.emplace_back(this->parse_expression(v));
    }
    switch (this->peek().type) {
    case Token_Type::right_curly:
      this->lexer_.skip_in_template(template_begin);
      switch (this->peek().type) {
      case Token_Type::complete_template: {
        this->peek().report_errors_for_escape_sequences_in_template(
            this->diag_reporter_);
        const Char8* template_end = this->peek().end;
        this->skip();

        Expression_Arena::Array_Ptr<Expression*> children_array =
            this->expressions_.make_array(std::move(children));
        Source_Code_Span template_span(template_begin, template_end);
        return this->make_expression<Expression::Template>(children_array,
                                                           template_span);
      }

      case Token_Type::incomplete_template:
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

void Parser::check_assignment_lhs(Expression* ast) {
try_again:
  switch (ast->kind()) {
  default:
    this->diag_reporter_->report(
        Diag_Invalid_Expression_Left_Of_Assignment{ast->span()});
    break;
  case Expression_Kind::Paren:
    ast = expression_cast<Expression::Paren*>(ast)->child_;
    goto try_again;
  case Expression_Kind::Non_Null_Assertion:
    ast = expression_cast<Expression::Non_Null_Assertion*>(ast)->child_;
    goto try_again;
  case Expression_Kind::Angle_Type_Assertion:
    ast = expression_cast<Expression::Angle_Type_Assertion*>(ast)->child_;
    goto try_again;
  case Expression_Kind::As_Type_Assertion:
    ast = expression_cast<Expression::As_Type_Assertion*>(ast)->child_;
    goto try_again;
  case Expression_Kind::Invalid:
  case Expression_Kind::Missing:
  case Expression_Kind::Paren_Empty:
    // An error should have been reported elsewhere.
    break;
  case Expression_Kind::Array:
  case Expression_Kind::Dot:
  case Expression_Kind::Index:
  case Expression_Kind::Object:
  case Expression_Kind::Variable:
  case Expression_Kind::Private_Variable:
    break;
  case Expression_Kind::Optional:
    ast = expression_cast<Expression::Optional*>(ast)->child_;
    goto try_again;
  case Expression_Kind::Satisfies:
    ast = expression_cast<Expression::Satisfies*>(ast)->child_;
    goto try_again;
  case Expression_Kind::Type_Annotated:
    // TODO(strager): Distinguish between the following:
    // const [x]: y = z;  // Valid TypeScript.
    // [x]: y = z;        // Invalid TypeScript.
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
