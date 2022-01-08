// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <memory>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-error-reporter.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <utility>

#if QLJS_HAVE_SETJMP
#include <csetjmp>
#endif

// parser is a recursive-descent parser.
//
// The parser class currently does not build an abstract syntax tree (AST) for
// statements. This allows the parser to send partial information to the lexer
// incrementally, enabling single-pass parsing and linting.
//
// The parser class currently builds an AST for expressions. (See expression.h.)
// Therefore, parsing and linting are not truly single-pass. This detail is not
// exposed to the linter, however; the linter does not see the expression ASTs.
//
// Each parser stores a lexer object internally. From the caller's perspective,
// the parser class takes characters as input.

namespace quick_lint_js {
parser_transaction::parser_transaction(lexer* l,
                                       error_reporter** error_reporter_pointer,
                                       monotonic_allocator* allocator)
    : lex_transaction(l->begin_transaction()),
      reporter(allocator),
      old_error_reporter(
          std::exchange(*error_reporter_pointer, &this->reporter)) {}

parser::parser(padded_string_view input, error_reporter* error_reporter)
    : parser(input, error_reporter, parser_options()) {}

parser::parser(padded_string_view input, error_reporter* error_reporter,
               parser_options options)
    : lexer_(input, error_reporter),
      error_reporter_(error_reporter),
      options_(options) {}

parser::function_guard parser::enter_function(function_attributes attributes) {
  bool was_in_top_level = this->in_top_level_;
  bool was_in_async_function = this->in_async_function_;
  bool was_in_generator_function = this->in_generator_function_;
  bool was_in_loop_statement = this->in_loop_statement_;
  bool was_in_switch_statement = this->in_switch_statement_;
  switch (attributes) {
  case function_attributes::async:
    this->in_async_function_ = true;
    this->in_generator_function_ = false;
    break;
  case function_attributes::async_generator:
    this->in_async_function_ = true;
    this->in_generator_function_ = true;
    break;
  case function_attributes::generator:
    this->in_async_function_ = false;
    this->in_generator_function_ = true;
    break;
  case function_attributes::normal:
    this->in_async_function_ = false;
    this->in_generator_function_ = false;
    break;
  }
  this->in_top_level_ = false;
  this->in_loop_statement_ = false;
  this->in_switch_statement_ = false;
  return function_guard(this, was_in_top_level, was_in_async_function,
                        was_in_generator_function, was_in_loop_statement,
                        was_in_switch_statement);
}

parser::loop_guard parser::enter_loop() {
  return loop_guard(this, std::exchange(this->in_loop_statement_, true));
}

parser::class_guard parser::enter_class() {
  return class_guard(this, std::exchange(this->in_class_, true));
}

function_attributes parser::parse_generator_star(
    function_attributes original_attributes) {
  bool is_generator = this->peek().type == token_type::star;
  if (is_generator) {
    this->skip();
    switch (original_attributes) {
    case function_attributes::async:
      return function_attributes::async_generator;
    case function_attributes::async_generator:
      // This can happen if the user puts the generator * before and after the
      // function keyword:
      //
      //   (*async function* f() {})
      return function_attributes::async_generator;
    case function_attributes::generator:
      // This can happen if the user puts the generator * before and after the
      // function keyword:
      //
      //   (*function* f() {})
      return function_attributes::generator;
    case function_attributes::normal:
      return function_attributes::generator;
    }
    QLJS_UNREACHABLE();
  } else {
    return original_attributes;
  }
}

expression* parser::maybe_wrap_erroneous_arrow_function(
    expression* arrow_function, expression* lhs) {
  switch (lhs->kind()) {
  default:
    return arrow_function;

  case expression_kind::trailing_comma: {
    auto* parameter_list = expression_cast<expression::trailing_comma>(lhs);
    expression* last_parameter =
        parameter_list->child(parameter_list->child_count() - 1);
    if (last_parameter->kind() == expression_kind::spread) {
      this->error_reporter_->report(
          error_comma_not_allowed_after_spread_parameter{
              .comma = parameter_list->comma_span(),
              .spread = last_parameter->span(),
          });
    }
    return arrow_function;
  }

  case expression_kind::call: {
    auto* call = expression_cast<expression::call>(lhs);
    // FIXME(strager): This check is duplicated.
    bool is_async_arrow_using_with_await_operator =
        call->child_0()->kind() == expression_kind::variable &&
        call->child_0()->variable_identifier_token_type() ==
            token_type::kw_await;
    if (is_async_arrow_using_with_await_operator) {
      // await (x) => {}   // Invalid.
      // await () => expr  // Invalid.
      // We treated 'await' as 'async' elsewhere. Don't report any error here.
      return arrow_function;
    } else {
      // f() => {}         // Invalid.
      this->error_reporter_->report(
          error_missing_operator_between_expression_and_arrow_function{
              .where = source_code_span(call->span().begin(),
                                        call->left_paren_span().end()),
          });
      std::array<expression*, 2> children{lhs->child_0(), arrow_function};
      return this->make_expression<expression::binary_operator>(
          this->expressions_.make_array(std::move(children)));
    }
  }
  }
}

void parser::error_on_sketchy_condition(expression* ast) {
  if (ast->kind() == expression_kind::assignment &&
      ast->child_1()->kind() == expression_kind::literal) {
    auto* assignment = static_cast<expression::assignment*>(ast);
    this->error_reporter_->report(error_assignment_makes_condition_constant{
        .assignment_operator = assignment->operator_span_,
    });
  }
}

void parser::error_on_class_statement(statement_kind statement_kind) {
  if (this->peek().type == token_type::kw_class) {
    const char8* expected_body = this->lexer_.end_of_previous_token();
    this->error_reporter_->report(error_class_statement_not_allowed_in_body{
        .kind_of_statement = statement_kind,
        .expected_body = source_code_span(expected_body, expected_body),
        .class_keyword = this->peek().span(),
    });
  }
}

void parser::error_on_lexical_declaration(statement_kind statement_kind) {
  bool is_lexical_declaration;
  switch (this->peek().type) {
  case token_type::kw_const:
    is_lexical_declaration = true;
    break;

  case token_type::kw_let: {
    lexer_transaction transaction = this->lexer_.begin_transaction();
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
    const char8* expected_body = this->lexer_.end_of_previous_token();
    this->error_reporter_->report(error_lexical_declaration_not_allowed_in_body{
        .kind_of_statement = statement_kind,
        .expected_body = source_code_span(expected_body, expected_body),
        .declaring_keyword = this->peek().span(),
    });
  }
}

void parser::error_on_function_statement(statement_kind statement_kind) {
  std::optional<source_code_span> function_keywords =
      this->is_maybe_function_statement();
  if (function_keywords.has_value()) {
    const char8* expected_body = this->lexer_.end_of_previous_token();
    this->error_reporter_->report(error_function_statement_not_allowed_in_body{
        .kind_of_statement = statement_kind,
        .expected_body = source_code_span(expected_body, expected_body),
        .function_keywords = *function_keywords,
    });
  }
}

std::optional<source_code_span> parser::is_maybe_function_statement() {
  switch (this->peek().type) {
    // function f() {}
  case token_type::kw_function:
    return this->peek().span();

    // async;
    // async function f() {}
  case token_type::kw_async: {
    lexer_transaction transaction = this->lexer_.begin_transaction();
    const char8* async_begin = this->peek().begin;
    this->skip();
    if (this->peek().type == token_type::kw_function) {
      source_code_span span(async_begin, this->peek().end);
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

std::optional<function_attributes>
parser::try_parse_function_with_leading_star() {
  QLJS_ASSERT(this->peek().type == token_type::star);
  token star_token = this->peek();
  lexer_transaction transaction = this->lexer_.begin_transaction();
  this->skip();
  if (this->peek().has_leading_newline) {
    this->lexer_.roll_back_transaction(std::move(transaction));
    return std::nullopt;
  }

  function_attributes attributes = function_attributes::generator;
  bool has_leading_async = this->peek().type == token_type::kw_async;
  // *async
  if (has_leading_async) {
    attributes = function_attributes::async_generator;
    this->skip();
  }

  if (this->peek().type != token_type::kw_function) {
    this->lexer_.roll_back_transaction(std::move(transaction));
    return std::nullopt;
  }

  // *function f() {}
  this->skip();
  if (this->peek().type == token_type::identifier) {
    this->error_reporter_->report(
        error_generator_function_star_belongs_before_name{
            .function_name = this->peek().span(),
            .star = star_token.span(),
        });
  } else {
    this->error_reporter_->report(
        error_generator_function_star_belongs_after_keyword_function{
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

bool parser::is_let_token_a_variable_reference(
    token following_token, bool allow_declarations) noexcept {
  switch (following_token.type) {
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL:
  QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
  QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
  case token_type::comma:
  case token_type::complete_template:
  case token_type::dot:
  case token_type::end_of_file:
  case token_type::equal:
  case token_type::equal_greater:
  case token_type::incomplete_template:
  case token_type::left_paren:
  case token_type::minus:
  case token_type::minus_minus:
  case token_type::plus:
  case token_type::plus_plus:
  case token_type::question:
  case token_type::semicolon:
  case token_type::slash:
    return true;

  QLJS_CASE_RESERVED_KEYWORD:
    if (following_token.type == token_type::kw_in ||
        following_token.type == token_type::kw_instanceof) {
      return true;
    } else {
      return following_token.has_leading_newline;
    }

  case token_type::left_square:
    return false;

  default:
    if (!allow_declarations) {
      return this->peek().has_leading_newline;
    } else {
      return false;
    }
  }
}

void parser::consume_semicolon() {
  switch (this->peek().type) {
  case token_type::semicolon:
    this->skip();
    break;
  case token_type::end_of_file:
  case token_type::right_curly:
    // Automatically insert a semicolon, then consume it.
    break;
  default:
    if (this->peek().has_leading_newline) {
      // Automatically insert a semicolon, then consume it.
    } else {
      this->lexer_.insert_semicolon();
      this->error_reporter_->report(
          error_missing_semicolon_after_statement{this->peek().span()});
      this->skip();
    }
    break;
  }
}

bool parser::is_arrow_kind(expression* ast) noexcept {
  return ast->kind() == expression_kind::arrow_function_with_statements ||
         ast->kind() == expression_kind::arrow_function_with_expression;
}

parser_transaction parser::begin_transaction() {
  return parser_transaction(&this->lexer_, &this->error_reporter_,
                            &this->temporary_memory_);
}

void parser::commit_transaction(parser_transaction&& transaction) {
  auto* buffered_errors =
      static_cast<buffering_error_reporter*>(this->error_reporter_);
  buffered_errors->move_into(transaction.old_error_reporter);
  this->error_reporter_ = transaction.old_error_reporter;

  this->lexer_.commit_transaction(std::move(transaction.lex_transaction));
}

void parser::roll_back_transaction(parser_transaction&& transaction) {
  this->error_reporter_ = transaction.old_error_reporter;
  this->lexer_.roll_back_transaction(std::move(transaction.lex_transaction));
}

void parser::crash_on_unimplemented_token(const char* qljs_file_name,
                                          int qljs_line,
                                          const char* qljs_function_name) {
#if QLJS_HAVE_SETJMP
  if (this->have_fatal_parse_error_jmp_buf_) {
    this->error_reporter_->report(error_unexpected_token{
        .token = this->peek().span(),
    });
    std::longjmp(this->fatal_parse_error_jmp_buf_, 1);
    QLJS_UNREACHABLE();
  }
#endif

  std::fprintf(stderr, "%s:%d: fatal: token not implemented in %s: %s",
               qljs_file_name, qljs_line, qljs_function_name,
               to_string(this->peek().type));
  cli_locator locator(this->lexer_.original_input());
  cli_source_position token_position = locator.position(this->peek().begin);
  std::fprintf(stderr, " on line %d column %d", token_position.line_number,
               token_position.column_number);
  std::fprintf(stderr, "\n");

  QLJS_CRASH_ALLOWING_CORE_DUMP();
}

void parser::crash_on_depth_limit_exceeded() {
#if QLJS_HAVE_SETJMP
  if (this->have_fatal_parse_error_jmp_buf_) {
    this->error_reporter_->report(error_depth_limit_exceeded{
        .token = this->peek().span(),
    });
    std::longjmp(this->fatal_parse_error_jmp_buf_, 1);
    QLJS_UNREACHABLE();
  }
#endif

  std::fprintf(stderr, "Error: parser depth limit exceeded\n");

  QLJS_CRASH_ALLOWING_CORE_DUMP();
}

parser::function_guard::function_guard(parser* p, bool was_in_top_level,
                                       bool was_in_async_function,
                                       bool was_in_generator_function,
                                       bool was_in_loop_statement,
                                       bool was_in_switch_statement) noexcept
    : parser_(p),
      was_in_top_level_(was_in_top_level),
      was_in_async_function_(was_in_async_function),
      was_in_generator_function_(was_in_generator_function),
      was_in_loop_statement_(was_in_loop_statement),
      was_in_switch_statement_(was_in_switch_statement) {}

parser::function_guard::~function_guard() noexcept {
  this->parser_->in_top_level_ = this->was_in_top_level_;
  this->parser_->in_async_function_ = this->was_in_async_function_;
  this->parser_->in_generator_function_ = this->was_in_generator_function_;
  this->parser_->in_loop_statement_ = this->was_in_loop_statement_;
  this->parser_->in_switch_statement_ = this->was_in_switch_statement_;
}

parser::depth_guard::depth_guard(parser* p) noexcept
    : parser_(p), old_depth_(p->depth_) {
  if (p->depth_ + 1 > p->stack_limit) {
    p->crash_on_depth_limit_exceeded();
  }
  p->depth_++;
}

parser::depth_guard::~depth_guard() noexcept {
  QLJS_ASSERT(this->parser_->depth_ == this->old_depth_ + 1);
  this->parser_->depth_ = this->old_depth_;
}

bool parser::parse_expression_cache_key::operator==(
    const parser::parse_expression_cache_key& rhs) const noexcept {
  return this->begin == rhs.begin && this->in_top_level == rhs.in_top_level &&
         this->in_async_function == rhs.in_async_function &&
         this->in_generator_function == rhs.in_generator_function &&
         this->in_loop_statement == rhs.in_loop_statement &&
         this->in_switch_statement == rhs.in_switch_statement &&
         this->in_class == rhs.in_class;
}

bool parser::parse_expression_cache_key::operator!=(
    const parser::parse_expression_cache_key& rhs) const noexcept {
  return !(*this == rhs);
}

std::size_t parser::parse_expression_cache_key::hash::operator()(
    const parse_expression_cache_key& x) const noexcept {
  return std::hash<const char8*>()(x.begin);
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
