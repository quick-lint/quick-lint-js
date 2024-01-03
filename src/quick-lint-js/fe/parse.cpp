// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <memory>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/diag/buffering-diag-reporter.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/jsx.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/algorithm.h>
#include <utility>

// Parser is a recursive-descent parser.
//
// The Parser class currently does not build an abstract syntax tree (AST) for
// statements. This allows the parser to send partial information to the lexer
// incrementally, enabling single-pass parsing and linting.
//
// The Parser class currently builds an AST for expressions. (See expression.h.)
// Therefore, parsing and linting are not truly single-pass. This detail is not
// exposed to the variable_analyzer, however; the variable_analyzer does not see
// the expression ASTs.
//
// Each Parser stores a lexer object internally. From the caller's perspective,
// the Parser class takes characters as input.

namespace quick_lint_js {
Parser_Transaction::Parser_Transaction(Lexer* l,
                                       Diag_Reporter** diag_reporter_pointer,
                                       Monotonic_Allocator* allocator)
    : lex_transaction(l->begin_transaction()),
      reporter(allocator),
      old_diag_reporter(
          std::exchange(*diag_reporter_pointer, &this->reporter)) {}

Parser::Parser(Padded_String_View input, Diag_Reporter* diag_reporter,
               Parser_Options options)
    : lexer_(input, diag_reporter,
             Lexer_Options{
                 .typescript = options.typescript,
             }),
      diag_reporter_(diag_reporter),
      options_(options) {}

Parser::Function_Guard Parser::enter_function(Function_Attributes attributes) {
  bool was_in_top_level = this->in_top_level_;
  bool was_in_async_function = this->in_async_function_;
  bool was_in_generator_function = this->in_generator_function_;
  bool was_in_loop_statement = this->in_loop_statement_;
  bool was_in_switch_statement = this->in_switch_statement_;
  switch (attributes) {
  case Function_Attributes::async:
    this->in_async_function_ = true;
    this->in_generator_function_ = false;
    break;
  case Function_Attributes::async_generator:
    this->in_async_function_ = true;
    this->in_generator_function_ = true;
    break;
  case Function_Attributes::generator:
    this->in_async_function_ = false;
    this->in_generator_function_ = true;
    break;
  case Function_Attributes::normal:
    this->in_async_function_ = false;
    this->in_generator_function_ = false;
    break;
  }
  this->in_top_level_ = false;
  this->in_loop_statement_ = false;
  this->in_switch_statement_ = false;
  return Function_Guard(this, was_in_top_level, was_in_async_function,
                        was_in_generator_function, was_in_loop_statement,
                        was_in_switch_statement);
}

Parser::Loop_Guard Parser::enter_loop() {
  return Loop_Guard(this, std::exchange(this->in_loop_statement_, true));
}

Parser::Class_Guard Parser::enter_class() {
  return Class_Guard(this, std::exchange(this->in_class_, true));
}

Parser::TypeScript_Only_Construct_Guard
Parser::enter_typescript_only_construct() {
  return TypeScript_Only_Construct_Guard(
      this, std::exchange(this->in_typescript_only_construct_, true));
}

Parser::Switch_Guard Parser::enter_switch() {
  return Switch_Guard(this, std::exchange(this->in_switch_statement_, true));
}

Parser::Binary_Expression_Builder::Binary_Expression_Builder(
    Monotonic_Allocator* allocator, Expression* first_child)
    : children_("binary_expression_builder children", allocator),
      operator_spans_("binary_expression_builder children", allocator) {
  this->children_.emplace_back(first_child);
}

Expression* Parser::Binary_Expression_Builder::last_expression() const {
  return this->children_.back();
}

bool Parser::Binary_Expression_Builder::has_multiple_children() const {
  return this->children_.size() > 1;
}

Expression* Parser::Binary_Expression_Builder::add_child(
    Source_Code_Span prior_operator_span, Expression* child) {
  this->operator_spans_.emplace_back(prior_operator_span);
  return this->children_.emplace_back(child);
}

void Parser::Binary_Expression_Builder::replace_last(
    Expression* new_last_child) {
  this->children_.back() = new_last_child;
}

void Parser::Binary_Expression_Builder::reset_after_build(
    Expression* new_first_child) {
  this->children_.clear();
  this->children_.emplace_back(new_first_child);
  this->operator_spans_.clear();
}

Expression_Arena::Array_Ptr<Expression*>
Parser::Binary_Expression_Builder::move_expressions(Expression_Arena& arena) {
  return arena.make_array(std::move(this->children_));
}

Expression_Arena::Array_Ptr<Source_Code_Span>
Parser::Binary_Expression_Builder::move_operator_spans(
    Expression_Arena& arena) {
  return arena.make_array(std::move(this->operator_spans_));
}

Expression* Parser::build_expression(Binary_Expression_Builder& builder) {
  if (builder.has_multiple_children()) {
    return this->make_expression<Expression::Binary_Operator>(
        builder.move_expressions(this->expressions_),
        builder.move_operator_spans(this->expressions_));
  } else {
    return builder.last_expression();
  }
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wnull-dereference")
void Parser::check_jsx_attribute(const Identifier& attribute_name) {
  const Hash_Map<String8_View, JSX_Attribute>& aliases =
      jsx_attribute_aliases();
  String8_View name = attribute_name.normalized_name();

  bool is_event_attribute =
      name.size() >= 3 && name[0] == 'o' && name[1] == 'n';

  if (auto alias_it = aliases.find(name); alias_it != aliases.end()) {
    const JSX_Attribute& alias = alias_it->second;
    if (name.size() != alias.expected.size()) {
      this->diag_reporter_->report(Diag_JSX_Attribute_Renamed_By_React{
          .attribute_name = attribute_name.span(),
          .react_attribute_name = alias.expected,
      });
      return;
    } else if (is_event_attribute) {
      this->diag_reporter_->report(
          Diag_JSX_Event_Attribute_Should_Be_Camel_Case{
              .attribute_name = attribute_name.span(),
              .expected_attribute_name = alias.expected,
          });
      return;
    } else {
      this->diag_reporter_->report(Diag_JSX_Attribute_Has_Wrong_Capitalization{
          .attribute_name = attribute_name.span(),
          .expected_attribute_name = alias.expected,
      });
      return;
    }
  }

  bool name_has_upper = any_of(name, isupper);

  if (!name_has_upper && is_event_attribute) {
    Vector<Char8> fixed_name("check_jsx_attribute fixed_name",
                             &this->diagnostic_memory_);
    fixed_name += name;
    fixed_name[2] = toupper(fixed_name[2]);
    this->diag_reporter_->report(Diag_JSX_Event_Attribute_Should_Be_Camel_Case{
        .attribute_name = attribute_name.span(),
        .expected_attribute_name = fixed_name.release_to_string_view(),
    });
  }

  if (name_has_upper) {
    String8 lowered_name(name);
    for (Char8& c : lowered_name) {
      c = tolower(c);
    }

    if (auto alias_it = aliases.find(String8_View(lowered_name));
        alias_it != aliases.end()) {
      if (alias_it->second.expected != name) {
        this->diag_reporter_->report(
            Diag_JSX_Attribute_Has_Wrong_Capitalization{
                .attribute_name = attribute_name.span(),
                .expected_attribute_name = alias_it->second.expected,
            });
      }
    }
  }
}
QLJS_WARNING_POP

std::optional<Source_Code_Span> Parser::parse_generator_star(
    Function_Attributes* attributes) {
  bool is_generator = this->peek().type == Token_Type::star;
  if (is_generator) {
    Source_Code_Span star_span = this->peek().span();
    this->skip();
    switch (*attributes) {
    case Function_Attributes::async:
      *attributes = Function_Attributes::async_generator;
      break;
    case Function_Attributes::async_generator:
      // This can happen if the user puts the generator * before and after the
      // function keyword:
      //
      //   (*async function* f() {})
      *attributes = Function_Attributes::async_generator;
      break;
    case Function_Attributes::generator:
      // This can happen if the user puts the generator * before and after the
      // function keyword:
      //
      //   (*function* f() {})
      *attributes = Function_Attributes::generator;
      break;
    case Function_Attributes::normal:
      *attributes = Function_Attributes::generator;
      break;
    }
    return star_span;
  } else {
    // Don't modify *attributes.
    return std::nullopt;
  }
}

Expression* Parser::maybe_wrap_erroneous_arrow_function(
    Expression* arrow_function, Expression* lhs) {
  switch (lhs->kind()) {
  default:
    return arrow_function;

  case Expression_Kind::Trailing_Comma: {
    auto* parameter_list = expression_cast<Expression::Trailing_Comma*>(lhs);
    Expression* last_parameter =
        parameter_list->child(parameter_list->child_count() - 1);
    if (last_parameter->kind() == Expression_Kind::Spread) {
      this->diag_reporter_->report(
          Diag_Comma_Not_Allowed_After_Spread_Parameter{
              .comma = parameter_list->comma_span(),
              .spread = last_parameter->span(),
          });
    }
    return arrow_function;
  }

  // f() => {}         // Invalid.
  case Expression_Kind::Call: {
    auto* call = expression_cast<Expression::Call*>(lhs);
    Source_Code_Span missing_operator_span(call->span().begin(),
                                           call->left_paren_span().end());
    this->diag_reporter_->report(
        Diag_Missing_Operator_Between_Expression_And_Arrow_Function{
            .where = missing_operator_span,
        });
    std::array<Expression*, 2> children{lhs->child_0(), arrow_function};
    std::array<Source_Code_Span, 1> operators{missing_operator_span};
    return this->make_expression<Expression::Binary_Operator>(
        this->expressions_.make_array(std::move(children)),
        this->expressions_.make_array(std::move(operators)));
  }
  }
}

void Parser::error_on_sketchy_condition(Expression* ast) {
  if (ast->kind() == Expression_Kind::Assignment &&
      ast->child_1()->kind() == Expression_Kind::Literal) {
    auto* assignment = expression_cast<Expression::Assignment*>(ast);
    this->diag_reporter_->report(Diag_Assignment_Makes_Condition_Constant{
        .assignment_operator = assignment->operator_span_,
    });
  }

  // checking if the third operand is either literal (includes 'null') or
  // 'undefined'
  if (ast->kind() == Expression_Kind::Binary_Operator &&
      ast->children().size() == 3 &&
      ((ast->child(2)->kind() == Expression_Kind::Literal) ||
       ((ast->child(2)->kind() == Expression_Kind::Variable) &&
        (expression_cast<Expression::Variable*>(ast->child(2))->type_ ==
         Token_Type::kw_undefined)))) {
    auto* binary = expression_cast<Expression::Binary_Operator*>(ast);
    Source_Code_Span left_operator = binary->operator_spans_[0];
    Source_Code_Span right_operator = binary->operator_spans_[1];
    if (right_operator.string_view() == u8"||"_sv &&
        (left_operator.string_view() == u8"=="_sv ||
         left_operator.string_view() == u8"==="_sv)) {
      this->diag_reporter_->report(Diag_Equals_Does_Not_Distribute_Over_Or{
          .or_operator = right_operator,
          .equals_operator = left_operator,
      });
    }
  }
}

void Parser::warn_on_comma_operator_in_conditional_statement(Expression* ast) {
  if (ast->kind() != Expression_Kind::Binary_Operator) return;

  auto is_comma = [](String8_View s) -> bool { return s == u8","_sv; };

  auto* binary_operator = expression_cast<Expression::Binary_Operator*>(ast);
  for (Span_Size i = binary_operator->child_count() - 2; i >= 0; i--) {
    Source_Code_Span op_span = binary_operator->operator_spans_[i];
    if (is_comma(op_span.string_view())) {
      this->diag_reporter_->report(
          Diag_Misleading_Comma_Operator_In_Conditional_Statement{.comma =
                                                                      op_span});
      return;
    }
  }
}

void Parser::warn_on_comma_operator_in_index(Expression* ast,
                                             Source_Code_Span left_square) {
  if (ast->kind() != Expression_Kind::Binary_Operator) return;

  auto is_comma = [](String8_View s) -> bool { return s == u8","_sv; };

  auto* binary_operator = expression_cast<Expression::Binary_Operator*>(ast);
  for (Span_Size i = binary_operator->child_count() - 2; i >= 0; i--) {
    Source_Code_Span op_span = binary_operator->operator_spans_[i];
    if (is_comma(op_span.string_view())) {
      this->diag_reporter_->report(
          Diag_Misleading_Comma_Operator_In_Index_Operation{
              .comma = op_span, .left_square = left_square});
      return;
    }
  }
}
void Parser::warn_on_unintuitive_bitshift_precedence(Expression* ast) {
  if (ast->kind() != Expression_Kind::Binary_Operator) return;
  if (ast->child_count() <= 2) return;
  auto* binary_op = static_cast<Expression::Binary_Operator*>(ast);
  Source_Code_Span left_op = binary_op->operator_spans_[0];
  Source_Code_Span right_op = binary_op->operator_spans_[1];
  if (left_op.string_view() == u8"&"_sv &&
      (right_op.string_view() == u8">>"_sv ||
       right_op.string_view() == u8"<<"_sv)) {
    if (binary_op->child(0)->kind() == Expression_Kind::Variable &&
        binary_op->child(1)->kind() == Expression_Kind::Literal &&
        binary_op->child(2)->kind() == Expression_Kind::Literal) {
      this->diag_reporter_->report(
          quick_lint_js::Diag_Unintuitive_Bitshift_Precedence{
              .bitshift_operator = right_op, .and_operator = left_op});
    }
  }
}
void Parser::error_on_pointless_string_compare(
    Expression::Binary_Operator* ast) {
  auto is_comparison_operator = [](String8_View s) {
    return s == u8"=="_sv || s == u8"==="_sv || s == u8"!="_sv ||
           s == u8"!=="_sv;
  };
  auto char_is_a_quote = [](const Char8* s) {
    return *s == '"' || *s == '\'' || *s == '`';
  };

  for (Span_Size i = 0; i < ast->child_count() - 1; i++) {
    Expression* lhs = ast->child(i)->without_paren();
    Expression* rhs = ast->child(i + 1)->without_paren();

    if ((lhs->kind() == Expression_Kind::Call &&
         rhs->kind() == Expression_Kind::Literal) ||
        (lhs->kind() == Expression_Kind::Literal &&
         rhs->kind() == Expression_Kind::Call)) {
      Source_Code_Span op_span = ast->operator_spans_[i];
      if (!is_comparison_operator(op_span.string_view())) {
        continue;
      }

      // make sure the call is on the "left" and the literal on the "right"
      if (lhs->kind() == Expression_Kind::Literal) {
        std::swap(lhs, rhs);
      }

      if (lhs->child_0()->kind() != Expression_Kind::Dot) {
        continue;
      }
      // Hack: The literal could also be a number like 0xeF.
      if (!char_is_a_quote(rhs->span().begin())) {
        continue;
      }

      String8_View call =
          lhs->child_0()->variable_identifier().span().string_view();
      String8_View literal = rhs->span().string_view();

      if (literal.find(u8"\\"_sv) != String8_View::npos) {
        continue;
      }

      if (call == u8"toLowerCase"_sv) {
        if (hasupper(literal)) {
          this->diag_reporter_->report(
              Diag_Pointless_String_Comp_Contains_Upper{op_span});
        }
      } else if (call == u8"toUpperCase"_sv) {
        if (haslower(literal)) {
          this->diag_reporter_->report(
              Diag_Pointless_String_Comp_Contains_Lower{op_span});
        }
      }
    }
  }
}

void Parser::error_on_invalid_as_const(Expression* ast,
                                       Source_Code_Span as_const_span) {
  ast = ast->without_paren();
  switch (ast->kind()) {
  case Expression_Kind::Dot:
  case Expression_Kind::Array:
  case Expression_Kind::Object:
  case Expression_Kind::Template:
    break;

  case Expression_Kind::Literal: {
    auto* literal = expression_cast<Expression::Literal*>(ast);
    if (literal->is_null() || literal->is_regexp()) {
      goto invalid;
    }
    break;
  }

  invalid:
  default:
    this->diag_reporter_->report(
        Diag_TypeScript_As_Const_With_Non_Literal_Typeable{
            .expression = ast->span(),
            .as_const = as_const_span,
        });
    break;
  }
}

void Parser::warn_on_dot_operator_after_optional_chain(Expression::Dot* ast) {
  Expression* lhs = ast->child_;
  Source_Code_Span operator_span = ast->operator_span_;
  auto is_optional_chain = [](String8_View s) -> bool { return s[0] == u8'?'; };

  // we know the current node is a dot operator.
  // If it is a '.' and its parent is a '?.' or a '?.(' or a '?.['
  // then we can report a warning
  if (!is_optional_chain(operator_span.string_view())) {
    switch (lhs->kind()) {
    case Expression_Kind::Dot: {
      auto lhs_dot = expression_cast<Expression::Dot*>(lhs);
      Source_Code_Span lhs_operator_span = lhs_dot->operator_span_;
      if (is_optional_chain(lhs_operator_span.string_view())) {
        this->diag_reporter_->report(Diag_Using_Dot_After_Optional_Chaining{
            .dot_op = operator_span,
            .optional_chain_op = lhs_operator_span,
        });
      }
      break;
    }
    case Expression_Kind::Call: {
      auto lhs_call = expression_cast<Expression::Call*>(lhs);
      std::optional<Source_Code_Span> lhs_operator_span =
          lhs_call->optional_chaining_operator_;
      if (lhs_operator_span.has_value()) {
        this->diag_reporter_->report(Diag_Using_Dot_After_Optional_Chaining{
            .dot_op = operator_span,
            .optional_chain_op = *lhs_operator_span,
        });
      }
      break;
    }
    case Expression_Kind::Index: {
      auto lhs_index = expression_cast<Expression::Index*>(lhs);
      std::optional<Source_Code_Span> lhs_operator_span =
          lhs_index->optional_chaining_operator_;
      if (lhs_operator_span.has_value()) {
        this->diag_reporter_->report(Diag_Using_Dot_After_Optional_Chaining{
            .dot_op = operator_span,
            .optional_chain_op = *lhs_operator_span,
        });
      }
      break;
    }
    default:
      break;
    }
  }
}

void Parser::warn_on_xor_operator_as_exponentiation(
    Expression::Binary_Operator* ast) {
  auto is_xor_operator = [](String8_View s) -> bool { return s == u8"^"_sv; };

  auto is_warnable_literal = [](String8_View s) -> bool {
    return s == u8"2"_sv || s == u8"10"_sv;
  };

  Source_Code_Span op_span = ast->operator_spans_[0];
  if (is_xor_operator(op_span.string_view())) {
    bool report_diag = false;
    switch (ast->child(0)->kind()) {
    case Expression_Kind::Literal:
      report_diag = is_warnable_literal(ast->child(0)->span().string_view()) &&
                    (ast->child(1)->kind() == Expression_Kind::Literal);
      break;

    default:
      break;
    }
    if (report_diag) {
      this->diag_reporter_->report(
          Diag_Xor_Used_As_Exponentiation{.xor_operator = op_span});
    }
  }
}

void Parser::error_on_pointless_compare_against_literal(
    Expression::Binary_Operator* ast) {
  auto is_comparison_operator = [](String8_View s) -> bool {
    return s == u8"=="_sv || s == u8"==="_sv || s == u8"!="_sv ||
           s == u8"!=="_sv;
  };

  for (Span_Size i = 0; i < ast->child_count() - 1; i++) {
    Source_Code_Span op_span = ast->operator_spans_[i];
    if (is_comparison_operator(op_span.string_view())) {
      this->check_compare_against_literal(ast->child(i), ast->child(i + 1),
                                          op_span);
    }
  }
}

void Parser::check_compare_against_literal(Expression* lhs, Expression* rhs,
                                           Source_Code_Span op_span) {
  auto get_comparison_result =
      [](String8_View equals_operator) -> String8_View {
    return (equals_operator == u8"==="_sv || equals_operator == u8"=="_sv)
               ? u8"false"_sv
               : u8"true"_sv;
  };
  auto is_strict_operator = [](String8_View op) -> bool {
    return op == u8"==="_sv || op == u8"!=="_sv;
  };

  for (Expression* child : {lhs, rhs}) {
    String8_View comparison_result =
        get_comparison_result(op_span.string_view());
    child = child->without_paren();
    switch (child->kind()) {
    case Expression_Kind::Class:
      this->diag_reporter_->report(Diag_Pointless_Comp_Against_Class_Literal{
          .equals_operator = op_span, .comparison_result = comparison_result});
      return;
    case Expression_Kind::Array:
      if (is_strict_operator(op_span.string_view())) {
        if (child->child_count() == 0) {
          this->diag_reporter_->report(
              Diag_Pointless_Strict_Comp_Against_Empty_Array_Literal{
                  .equals_operator = op_span,
                  .comparison_result = comparison_result});
        } else {
          this->diag_reporter_->report(
              Diag_Pointless_Strict_Comp_Against_Array_Literal{
                  .equals_operator = op_span});
        }
        return;
      }
      break;
    case Expression_Kind::Arrow_Function:
      this->diag_reporter_->report(Diag_Pointless_Comp_Against_Arrow_Function{
          .equals_operator = op_span, .comparison_result = comparison_result});
      return;
    case Expression_Kind::Object:
      this->diag_reporter_->report(Diag_Pointless_Comp_Against_Object_Literal{
          .equals_operator = op_span, .comparison_result = comparison_result});
      return;
    case Expression_Kind::Literal:
      if (expression_cast<Expression::Literal*>(child)->is_regexp()) {
        this->diag_reporter_->report(
            Diag_Pointless_Comp_Against_Regular_Expression_Literal{
                .equals_operator = op_span,
                .comparison_result = comparison_result});
        return;
      }
      break;
    default:
      break;
    }
  }
}

void Parser::error_on_class_statement(Statement_Kind statement_kind) {
  if (this->peek().type == Token_Type::kw_class) {
    this->diag_reporter_->report(Diag_Class_Statement_Not_Allowed_In_Body{
        .kind_of_statement = statement_kind,
        .expected_body =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
        .class_keyword = this->peek().span(),
    });
  }
}

void Parser::error_on_lexical_declaration(Statement_Kind statement_kind) {
  bool is_lexical_declaration;
  switch (this->peek().type) {
  case Token_Type::kw_const: {
    if (this->options_.typescript) {
      Lexer_Transaction transaction = this->lexer_.begin_transaction();
      this->skip();
      if (this->peek().type == Token_Type::kw_enum) {
        // const enum E {}
        is_lexical_declaration = false;
      } else {
        // const v = null;
        is_lexical_declaration = true;
      }
      this->lexer_.roll_back_transaction(std::move(transaction));
    } else {
      // const v = null;
      is_lexical_declaration = true;
    }
    break;
  }

  case Token_Type::kw_let: {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    is_lexical_declaration = !this->is_let_token_a_variable_reference(
        this->peek(), /*allow_declarations=*/false);
    this->lexer_.roll_back_transaction(std::move(transaction));
    break;
  }

  default:
    is_lexical_declaration = false;
    break;
  }
  if (is_lexical_declaration) {
    this->diag_reporter_->report(Diag_Lexical_Declaration_Not_Allowed_In_Body{
        .kind_of_statement = statement_kind,
        .expected_body =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
        .declaring_keyword = this->peek().span(),
    });
  }
}

void Parser::error_on_function_statement(Statement_Kind statement_kind) {
  std::optional<Source_Code_Span> function_keywords =
      this->is_maybe_function_statement();
  if (function_keywords.has_value()) {
    this->diag_reporter_->report(Diag_Function_Statement_Not_Allowed_In_Body{
        .kind_of_statement = statement_kind,
        .expected_body =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
        .function_keywords = *function_keywords,
    });
  }
}

std::optional<Source_Code_Span> Parser::is_maybe_function_statement() {
  switch (this->peek().type) {
    // function f() {}
  case Token_Type::kw_function:
    return this->peek().span();

    // async;
    // async function f() {}
  case Token_Type::kw_async: {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    const Char8* async_begin = this->peek().begin;
    this->skip();
    if (this->peek().type == Token_Type::kw_function) {
      Source_Code_Span span(async_begin, this->peek().end);
      this->lexer_.roll_back_transaction(std::move(transaction));
      return span;
    } else {
      this->lexer_.roll_back_transaction(std::move(transaction));
      return std::nullopt;
    }
  }

  default:
    return std::nullopt;
  }
}

std::optional<Function_Attributes>
Parser::try_parse_function_with_leading_star() {
  QLJS_ASSERT(this->peek().type == Token_Type::star);
  Token star_token = this->peek();
  Lexer_Transaction transaction = this->lexer_.begin_transaction();
  this->skip();
  if (this->peek().has_leading_newline) {
    this->lexer_.roll_back_transaction(std::move(transaction));
    return std::nullopt;
  }

  Function_Attributes attributes = Function_Attributes::generator;
  bool has_leading_async = this->peek().type == Token_Type::kw_async;
  // *async
  if (has_leading_async) {
    attributes = Function_Attributes::async_generator;
    this->skip();
  }

  if (this->peek().type != Token_Type::kw_function) {
    this->lexer_.roll_back_transaction(std::move(transaction));
    return std::nullopt;
  }

  // *function f() {}
  this->skip();
  if (this->peek().type == Token_Type::identifier) {
    this->diag_reporter_->report(
        Diag_Generator_Function_Star_Belongs_Before_Name{
            .function_name = this->peek().span(),
            .star = star_token.span(),
        });
  } else {
    this->diag_reporter_->report(
        Diag_Generator_Function_Star_Belongs_After_Keyword_Function{
            .star = star_token.span()});
  }
  this->lexer_.roll_back_transaction(std::move(transaction));
  this->skip();
  // *async function f() {}
  if (has_leading_async) {
    this->skip();
  }
  return attributes;
}

bool Parser::is_let_token_a_variable_reference(const Token& following_token,
                                               bool allow_declarations) {
  switch (following_token.type) {
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL:
  QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
  QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
  case Token_Type::comma:
  case Token_Type::complete_template:
  case Token_Type::dot:
  case Token_Type::end_of_file:
  case Token_Type::equal:
  case Token_Type::equal_greater:
  case Token_Type::incomplete_template:
  case Token_Type::left_paren:
  case Token_Type::less:
  case Token_Type::minus:
  case Token_Type::minus_minus:
  case Token_Type::plus:
  case Token_Type::plus_plus:
  case Token_Type::question:
  case Token_Type::semicolon:
  case Token_Type::slash:
    return true;

  QLJS_CASE_RESERVED_KEYWORD:
    if (following_token.type == Token_Type::kw_in ||
        following_token.type == Token_Type::kw_instanceof) {
      return true;
    } else {
      return following_token.has_leading_newline;
    }

  case Token_Type::left_square:
    return false;

  default:
    if (!allow_declarations) {
      return this->peek().has_leading_newline;
    } else {
      return false;
    }
  }
}

void Parser::consume_semicolon_after_statement() {
  this->consume_semicolon<Diag_Missing_Semicolon_After_Statement>();
}

void Parser::check_body_after_label() {
  this->error_on_class_statement(Statement_Kind::labelled_statement);
  this->error_on_lexical_declaration(Statement_Kind::labelled_statement);
}

template <class Missing_Semicolon_Diagnostic>
void Parser::consume_semicolon() {
  // See also: Parser::consume_semicolon_or_comma
  switch (this->peek().type) {
  case Token_Type::semicolon:
    this->skip();
    break;
  case Token_Type::end_of_file:
  case Token_Type::right_curly:
    // Automatically insert a semicolon, then consume it.
    break;
  default:
    if (this->peek().has_leading_newline) {
      // Automatically insert a semicolon, then consume it.
    } else {
      this->lexer_.insert_semicolon();
      this->diag_reporter_->report(
          Missing_Semicolon_Diagnostic{this->peek().span()});
      this->skip();
    }
    break;
  }
}

template <class Missing_Semicolon_Diagnostic>
void Parser::consume_semicolon_or_comma() {
  // See also: Parser::consume_semicolon
  switch (this->peek().type) {
  case Token_Type::comma:
  case Token_Type::semicolon:
    this->skip();
    break;
  case Token_Type::end_of_file:
  case Token_Type::right_curly:
    // Automatically insert a semicolon, then consume it.
    break;
  default:
    if (this->peek().has_leading_newline) {
      // Automatically insert a semicolon, then consume it.
    } else {
      this->lexer_.insert_semicolon();
      this->diag_reporter_->report(
          Missing_Semicolon_Diagnostic{this->peek().span()});
      this->skip();
    }
    break;
  }
}

void Parser::error_on_pointless_nullish_coalescing_operator(
    Expression::Binary_Operator* ast) {
  auto is_nullish_operator = [](String8_View s) -> bool {
    return s == u8"??"_sv;
  };

  Source_Code_Span op_span = ast->operator_spans_[0];
  if (is_nullish_operator(op_span.string_view())) {
    this->check_lhs_for_null_potential(ast->child(0)->without_paren(), op_span);
  }
}

void Parser::check_lhs_for_null_potential(Expression* lhs,
                                          Source_Code_Span op_span) {
  auto binary_operator_is_never_null =
      [](Expression::Binary_Operator* expr) -> bool {
    // these 4 binary operators can resolve to a null value
    String8_View can_resolve_to_null[4] = {u8"&&"_sv, u8"??"_sv, u8","_sv,
                                           u8"||"_sv};
    for (Span_Size i = 0; i < expr->child_count() - 1; i++) {
      String8_View expr_op_span = expr->operator_spans_[i].string_view();
      if (contains(can_resolve_to_null, expr_op_span)) {
        return false;
      }
    }
    return true;
  };

  bool report_diag = false;
  switch (lhs->kind()) {
  case Expression_Kind::Literal:
    if (lhs->span().string_view() != u8"null"_sv) {
      report_diag = true;
    }
    break;
  case Expression_Kind::RW_Unary_Suffix:
    report_diag = true;
    break;
  case Expression_Kind::Unary_Operator: {
    auto* maybe_void_lhs = expression_cast<Expression::Unary_Operator*>(lhs);
    if (!maybe_void_lhs->is_void_operator()) {
      report_diag = true;
    }
    break;
  }
  case Expression_Kind::Typeof:
    report_diag = true;
    break;
  case Expression_Kind::Binary_Operator: {
    auto* operator_lhs = expression_cast<Expression::Binary_Operator*>(lhs);
    report_diag = binary_operator_is_never_null(operator_lhs);
    break;
  }
  default:
    break;
  }
  if (report_diag) {
    this->diag_reporter_->report(Diag_Pointless_Nullish_Coalescing_Operator{
        .question_question = op_span});
  }
}

template void
Parser::consume_semicolon<Diag_Missing_Semicolon_After_Abstract_Method>();
template void
Parser::consume_semicolon<Diag_Missing_Semicolon_After_Declare_Class_Method>();
template void Parser::consume_semicolon<Diag_Missing_Semicolon_After_Field>();
template void
Parser::consume_semicolon<Diag_Missing_Semicolon_After_Statement>();
template void Parser::consume_semicolon<
    Diag_Missing_Semicolon_After_TypeScript_Method_Overload_Signature>();

template void
Parser::consume_semicolon_or_comma<Diag_Missing_Semicolon_After_Field>();
template void Parser::consume_semicolon_or_comma<
    Diag_Missing_Semicolon_After_Index_Signature>();
template void Parser::consume_semicolon_or_comma<
    Diag_Missing_Semicolon_After_Interface_Method>();
template void Parser::consume_semicolon_or_comma<
    Diag_Missing_Separator_Between_Object_Type_Entries>();

Parser_Transaction Parser::begin_transaction() {
  return Parser_Transaction(&this->lexer_, &this->diag_reporter_,
                            &this->temporary_memory_);
}

void Parser::commit_transaction(Parser_Transaction&& transaction) {
  auto* buffered_diagnostics =
      derived_cast<Buffering_Diag_Reporter*>(this->diag_reporter_);
  buffered_diagnostics->move_into(transaction.old_diag_reporter);
  this->diag_reporter_ = transaction.old_diag_reporter;

  this->lexer_.commit_transaction(std::move(transaction.lex_transaction));
}

void Parser::roll_back_transaction(Parser_Transaction&& transaction) {
  this->diag_reporter_ = transaction.old_diag_reporter;
  this->lexer_.roll_back_transaction(std::move(transaction.lex_transaction));
}

void Parser::crash_on_unimplemented_token(const char* qljs_file_name,
                                          int qljs_line,
                                          const char* qljs_function_name) {
  this->fatal_parse_error_stack_.raise_if_have_handler(Fatal_Parse_Error{
      this->peek().span(),
      Fatal_Parse_Error_Kind::unexpected_token,
  });

  std::fprintf(stderr, "%s:%d: fatal: token not implemented in %s: %s",
               qljs_file_name, qljs_line, qljs_function_name,
               to_string(this->peek().type));
  CLI_Locator locator(this->lexer_.original_input());
  CLI_Source_Position token_position = locator.position(this->peek().begin);
  std::fprintf(stderr, " on line %d column %d", token_position.line_number,
               token_position.column_number);
  std::fprintf(stderr, "\n");
  std::fflush(stderr);

  QLJS_CRASH_ALLOWING_CORE_DUMP();
}

void Parser::crash_on_depth_limit_exceeded() {
  this->fatal_parse_error_stack_.raise_if_have_handler(Fatal_Parse_Error{
      this->peek().span(),
      Fatal_Parse_Error_Kind::depth_limit_exceeded,
  });

  std::fprintf(stderr, "Error: parser depth limit exceeded\n");
  std::fflush(stderr);

  QLJS_CRASH_ALLOWING_CORE_DUMP();
}

Parser::Function_Guard::Function_Guard(Parser* p, bool was_in_top_level,
                                       bool was_in_async_function,
                                       bool was_in_generator_function,
                                       bool was_in_loop_statement,
                                       bool was_in_switch_statement)
    : parser_(p),
      was_in_top_level_(was_in_top_level),
      was_in_async_function_(was_in_async_function),
      was_in_generator_function_(was_in_generator_function),
      was_in_loop_statement_(was_in_loop_statement),
      was_in_switch_statement_(was_in_switch_statement) {}

Parser::Function_Guard::~Function_Guard() {
  this->parser_->in_top_level_ = this->was_in_top_level_;
  this->parser_->in_async_function_ = this->was_in_async_function_;
  this->parser_->in_generator_function_ = this->was_in_generator_function_;
  this->parser_->in_loop_statement_ = this->was_in_loop_statement_;
  this->parser_->in_switch_statement_ = this->was_in_switch_statement_;
}

Parser::Depth_Guard::Depth_Guard(Parser* p)
    : parser_(p), old_depth_(p->depth_) {
  if (p->depth_ + 1 > p->stack_limit) {
    p->crash_on_depth_limit_exceeded();
  }
  p->depth_++;
}

Parser::Depth_Guard::~Depth_Guard() {
  QLJS_ASSERT(this->parser_->depth_ == this->old_depth_ + 1);
  this->parser_->depth_ = this->old_depth_;
}

Source_Code_Span Parser::TypeScript_Declare_Context::declare_keyword_span()
    const {
  QLJS_ASSERT(this->declare_namespace_declare_keyword.has_value() ||
              this->direct_declare_keyword.has_value());
  std::optional<Source_Code_Span> span = this->maybe_declare_keyword_span();
  QLJS_ASSERT(span.has_value());
  return *span;
}

std::optional<Source_Code_Span>
Parser::TypeScript_Declare_Context::maybe_declare_keyword_span() const {
  if (this->direct_declare_keyword.has_value()) {
    return this->direct_declare_keyword;
  }
  if (this->declare_namespace_declare_keyword.has_value()) {
    return this->declare_namespace_declare_keyword;
  }
  return std::nullopt;
}

Parser::TypeScript_Namespace_Or_Module_Guard
Parser::enter_typescript_namespace_or_module(
    Source_Code_Span namespace_or_module_keyword_span, bool entering_module) {
  return TypeScript_Namespace_Or_Module_Guard(
      this,
      std::exchange(this->in_typescript_namespace_or_module_,
                    namespace_or_module_keyword_span),
      std::exchange(this->in_typescript_module_, entering_module),
      std::exchange(this->in_loop_statement_, false),
      std::exchange(this->in_switch_statement_, false));
}

Parser::TypeScript_Namespace_Or_Module_Guard::
    TypeScript_Namespace_Or_Module_Guard(
        Parser* parser,
        std::optional<Source_Code_Span> old_in_typescript_namespace_or_module,
        bool old_in_typescript_module, bool old_in_loop_statement,
        bool old_in_switch_statement)
    : parser_(parser),
      old_in_typescript_namespace_or_module_(
          old_in_typescript_namespace_or_module),
      old_in_typescript_module_(old_in_typescript_module),
      old_in_loop_statement_(old_in_loop_statement),
      old_in_switch_statement_(old_in_switch_statement) {}

Parser::TypeScript_Namespace_Or_Module_Guard::
    ~TypeScript_Namespace_Or_Module_Guard() {
  this->parser_->in_typescript_namespace_or_module_ =
      this->old_in_typescript_namespace_or_module_;
  this->parser_->in_typescript_module_ = this->old_in_typescript_module_;
  this->parser_->in_loop_statement_ = this->old_in_loop_statement_;
  this->parser_->in_switch_statement_ = this->old_in_switch_statement_;
}

bool Parser::Parse_Expression_Cache_Key::operator==(
    const Parser::Parse_Expression_Cache_Key& rhs) const {
  return this->begin == rhs.begin && this->in_top_level == rhs.in_top_level &&
         this->in_async_function == rhs.in_async_function &&
         this->in_generator_function == rhs.in_generator_function &&
         this->in_loop_statement == rhs.in_loop_statement &&
         this->in_switch_statement == rhs.in_switch_statement &&
         this->in_class == rhs.in_class;
}

bool Parser::Parse_Expression_Cache_Key::operator!=(
    const Parser::Parse_Expression_Cache_Key& rhs) const {
  return !(*this == rhs);
}

std::size_t Parser::Parse_Expression_Cache_Key::Hash::operator()(
    const Parse_Expression_Cache_Key& x) const {
  return std::hash<const Char8*>()(x.begin);
}

Parser::Parse_Expression_Cache_Key
Parser::parse_expression_cache_key_for_current_state() const {
  return Parse_Expression_Cache_Key{
      .begin = this->peek().begin,
      .in_top_level = this->in_top_level_,
      .in_async_function = this->in_async_function_,
      .in_generator_function = this->in_generator_function_,
      .in_loop_statement = this->in_loop_statement_,
      .in_switch_statement = this->in_switch_statement_,
      .in_class = this->in_class_,
  };
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
