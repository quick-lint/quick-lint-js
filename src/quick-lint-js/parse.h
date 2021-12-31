// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PARSE_H
#define QUICK_LINT_JS_PARSE_H

#include <boost/container/pmr/unsynchronized_pool_resource.hpp>
#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/expression.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/null-visitor.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/warning.h>
#include <unordered_map>
#include <utility>

#if QLJS_HAVE_SETJMP
#include <csetjmp>
#endif

#define QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_LESS_AND_STAR \
  case token_type::ampersand:                                      \
  case token_type::ampersand_ampersand:                            \
  case token_type::bang_equal:                                     \
  case token_type::bang_equal_equal:                               \
  case token_type::circumflex:                                     \
  case token_type::equal_equal:                                    \
  case token_type::equal_equal_equal:                              \
  case token_type::greater:                                        \
  case token_type::greater_equal:                                  \
  case token_type::greater_greater:                                \
  case token_type::greater_greater_greater:                        \
  case token_type::less_equal:                                     \
  case token_type::less_less:                                      \
  case token_type::percent:                                        \
  case token_type::pipe:                                           \
  case token_type::pipe_pipe:                                      \
  case token_type::question_question:                              \
  case token_type::star_star

#define QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL                 \
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL_EXCEPT_LESS_AND_STAR: \
  case token_type::less:                                      \
  case token_type::star

#define QLJS_CASE_BINARY_ONLY_OPERATOR   \
  QLJS_CASE_BINARY_ONLY_OPERATOR_SYMBOL: \
  case token_type::kw_instanceof

#define QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL \
  case token_type::ampersand_equal:                               \
  case token_type::circumflex_equal:                              \
  case token_type::greater_greater_equal:                         \
  case token_type::greater_greater_greater_equal:                 \
  case token_type::less_less_equal:                               \
  case token_type::minus_equal:                                   \
  case token_type::percent_equal:                                 \
  case token_type::pipe_equal:                                    \
  case token_type::plus_equal:                                    \
  case token_type::star_equal:                                    \
  case token_type::star_star_equal

#define QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR \
  case token_type::slash_equal:                \
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL

#define QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR \
  case token_type::ampersand_ampersand_equal:     \
  case token_type::pipe_pipe_equal:               \
  case token_type::question_question_equal

#define QLJS_PARSER_UNIMPLEMENTED()                                   \
  do {                                                                \
    this->crash_on_unimplemented_token(__FILE__, __LINE__, __func__); \
  } while (false)

#define QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(expected_token_type) \
  do {                                                              \
    if (this->peek().type != (expected_token_type)) {               \
      QLJS_PARSER_UNIMPLEMENTED();                                  \
    }                                                               \
  } while (false)

namespace quick_lint_js {
enum class parser_top_level_await_mode {
  auto_detect = 0,
  await_operator,
};

// TODO(#465): Accept parser options from quick-lint-js.config or CLI options.
struct parser_options {
  parser_top_level_await_mode top_level_await_mode =
      parser_top_level_await_mode::auto_detect;

  // If true, parse JSX langauge extensions: https://facebook.github.io/jsx/
  bool jsx = false;
};

struct parser_transaction {
  // Private to parser's transaction functions. Do not construct, read, or
  // modify.

  explicit parser_transaction(lexer *l, error_reporter **error_reporter_pointer,
                              monotonic_allocator *allocator);

  lexer_transaction lex_transaction;
  buffering_error_reporter reporter;
  error_reporter *old_error_reporter;
};

// A parser reads JavaScript source code and calls the member functions of a
// parse_visitor (visit_variable_declaration, visit_enter_function_scope, etc.).
class parser {
 private:
  template <bool parser::*Member>
  class bool_guard;

  class function_guard;

  class depth_guard;

 public:
  explicit parser(padded_string_view input, error_reporter *error_reporter);
  explicit parser(padded_string_view input, error_reporter *error_reporter,
                  parser_options options);

  quick_lint_js::lexer &lexer() noexcept { return this->lexer_; }

  // For testing only.
  quick_lint_js::expression_arena &expression_arena() noexcept {
    return this->expressions_;
  }

  // For testing and internal use only.
  [[nodiscard]] function_guard enter_function(function_attributes);

#if QLJS_HAVE_SETJMP
  // Returns true if parsing succeeded without QLJS_PARSER_UNIMPLEMENTED being
  // called.
  //
  // Returns false if QLJS_PARSER_UNIMPLEMENTED was called.
  template <QLJS_PARSE_VISITOR Visitor>
  bool parse_and_visit_module_catching_fatal_parse_errors(Visitor &v) {
    this->have_fatal_parse_error_jmp_buf_ = true;
    bool ok;
    if (setjmp(this->fatal_parse_error_jmp_buf_) == 0) {
      this->parse_and_visit_module(v);
      ok = true;
    } else {
      // QLJS_PARSER_UNIMPLEMENTED was called.
      ok = false;
    }
    this->have_fatal_parse_error_jmp_buf_ = false;
    return ok;
  }
#endif

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_module(Visitor &v);

  enum class parse_statement_type {
    any_statement,
    any_statement_in_block,
    no_declarations,
  };

  // If a statement was parsed, this function returns true.
  //
  // If a statement was not parsed (e.g. end of file), then:
  // * no tokens are consumed
  // * no error is reported
  // * this function returns false
  template <QLJS_PARSE_VISITOR Visitor>
  [[nodiscard]] bool parse_and_visit_statement(
      Visitor &v, parse_statement_type statement_type =
                      parse_statement_type::any_statement);

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_expression(Visitor &v) {
    this->parse_and_visit_expression(v, precedence{});
  }

  // The Visitor is only used for the bodies of arrow and function expressions.
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_expression(Visitor &v) {
    return this->parse_expression(v, precedence{});
  }

 private:
  enum class variable_context {
    lhs,
    rhs,
  };

  template <QLJS_PARSE_VISITOR Visitor>
  void visit_expression(expression *ast, Visitor &v, variable_context context);
  template <QLJS_PARSE_VISITOR Visitor>
  void visit_assignment_expression(expression *lhs, expression *rhs,
                                   Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  void visit_compound_or_conditional_assignment_expression(expression *lhs,
                                                           expression *rhs,
                                                           Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  void maybe_visit_assignment(expression *ast, Visitor &v);

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_statement_block_no_scope(Visitor &v);

  enum class name_requirement {
    optional,
    required_for_export,
    required_for_statement,
  };

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_function_declaration(Visitor &v,
                                            function_attributes attributes,
                                            const char8 *begin,
                                            name_requirement require_name);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_function_parameters_and_body(
      Visitor &v, std::optional<source_code_span> name,
      function_attributes attributes);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_function_parameters_and_body_no_scope(
      Visitor &v, std::optional<source_code_span> name,
      function_attributes attributes);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_function_parameters(Visitor &v);
  std::optional<source_code_span> is_maybe_function_statement();
  // If the function returns nullopt, no tokens are consumed.
  //
  // If the function returns a function_attributes, tokens are consumed until
  // the kw_function (i.e. the * and possibly a following async are skipped)
  // E.g. *async function f() {}
  // In this case `*async` is consumed.
  std::optional<function_attributes> try_parse_function_with_leading_star();

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_class(Visitor &v, name_requirement require_name);
  // Parse the 'class' keyword, the class's optional name, and any extends
  // clause.
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_class_heading(Visitor &v, name_requirement require_name);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_class_body(Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_class_member(Visitor &v);

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_try_maybe_catch_maybe_finally(Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  [[nodiscard]] bool parse_and_visit_catch_or_finally_or_both(Visitor &v);

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_do_while(Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_for(Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_while(Visitor &v);

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_if(Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_switch(Visitor &v);

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_with(Visitor &v);

  template <class ExpectedParenthesesError, class ExpectedParenthesisError,
            QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_parenthesized_expression(Visitor &v);

  void error_on_class_statement(statement_kind statement_kind);
  void error_on_lexical_declaration(statement_kind statement_kind);
  void error_on_function_statement(statement_kind statement_kind);

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_import(Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_name_space_import(Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_named_exports_for_import(Visitor &v);

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_export(Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_named_exports_for_export(
      Visitor &v,
      bump_vector<token, monotonic_allocator> &out_exported_bad_tokens);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_named_exports(
      Visitor &v,
      bump_vector<token, monotonic_allocator> *out_exported_bad_tokens);

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_variable_declaration_statement(Visitor &v);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_let_bindings(
      Visitor &v, token declaring_token, bool allow_in_operator,
      bool allow_const_without_initializer = false,
      bool is_in_for_initializer = false);
  // declaring_token is the const/let/var token.
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_let_bindings(Visitor &v, token declaring_token,
                                    variable_kind declaration_kind,
                                    bool allow_in_operator,
                                    bool allow_const_without_initializer,
                                    bool is_in_for_initializer);
  bool is_let_token_a_variable_reference(token following_token,
                                         bool allow_declarations) noexcept;
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_binding_element(
      Visitor &v, variable_kind declaration_kind,
      std::optional<source_code_span> declaring_token, bool allow_in_operator);
  template <QLJS_PARSE_VISITOR Visitor>
  void visit_binding_element(expression *ast, Visitor &v,
                             variable_kind declaration_kind,
                             std::optional<source_code_span> declaring_token);

  struct precedence {
    bool binary_operators = true;
    bool math_or_logical_or_assignment = true;
    bool commas = true;
    bool in_operator = true;

    // If true, parse unexpected trailing identifiers as part of the
    // expression (and emit an error).
    bool trailing_identifiers = false;
    bool conditional_operator = true;
  };

  template <QLJS_PARSE_VISITOR Visitor>
  void parse_and_visit_expression(Visitor &v, precedence prec) {
    monotonic_allocator &alloc = *this->expressions_.allocator();
    auto rewind_guard = alloc.make_rewind_guard();

    expression *ast = this->parse_expression(v, prec);
    {
      auto disable_guard = alloc.disable();
      this->visit_expression(ast, v, variable_context::rhs);
    }
  }

  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_expression(Visitor &, precedence);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_primary_expression(Visitor &, precedence);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_async_expression(Visitor &, token async_token, precedence);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_async_expression_only(Visitor &, token async_token,
                                          bool allow_in_operator);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_await_expression(Visitor &, token await_token,
                                     precedence prec);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_expression_remainder(Visitor &, expression *, precedence);
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_arrow_function_expression_remainder(
      Visitor &, source_code_span arrow_span,
      expression_arena::vector<expression *> &children, bool allow_in_operator);
  // Precondition: Current token is '=>'.
  template <QLJS_PARSE_VISITOR Visitor>
  void parse_arrow_function_expression_remainder(
      Visitor &, expression_arena::vector<expression *> &children,
      bool allow_in_operator);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_call_expression_remainder(Visitor &, expression *callee);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_index_expression_remainder(Visitor &, expression *lhs);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_arrow_function_body(Visitor &, function_attributes,
                                        const char8 *parameter_list_begin,
                                        bool allow_in_operator);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_arrow_function_body(
      Visitor &, function_attributes, const char8 *parameter_list_begin,
      bool allow_in_operator,
      expression_arena::array_ptr<expression *> &&parameters);
  // Args is either of the following:
  // * expression_arena::array_ptr<expression*> &&parameters
  // * (none)
  template <class Visitor, class... Args>
  expression *parse_arrow_function_body_impl(Visitor &, function_attributes,
                                             const char8 *parameter_list_begin,
                                             bool allow_in_operator,
                                             Args &&... args);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_function_expression(Visitor &, function_attributes,
                                        const char8 *span_begin);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_object_literal(Visitor &);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_class_expression(Visitor &);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_jsx_expression(Visitor &);
  // tag is optional. If it is nullptr, parse a fragment. Otherwise, parse an
  // element.
  //
  // Precondition: previous token was '<' (for fragments) or an identifier (for
  //               elements).
  // Postcondition: current token is '>' or end_of_file.
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_jsx_element_or_fragment(Visitor &, identifier *tag,
                                            const char8 *less_begin);
  template <QLJS_PARSE_VISITOR Visitor>
  expression *parse_template(Visitor &, std::optional<expression *> tag);

  function_attributes parse_generator_star(function_attributes);

  expression *maybe_wrap_erroneous_arrow_function(expression *arrow_function,
                                                  expression *lhs);

  bool is_arrow_kind(expression *ast) noexcept;

  void consume_semicolon();

  const token &peek() const noexcept { return this->lexer_.peek(); }
  void skip() noexcept { this->lexer_.skip(); }

  // Save lexer and parser state.
  //
  // After calling begin_transaction, you must call either commit_transaction or
  // roll_back_transaction with the returned transaction.
  //
  // You can call begin_transaction again before calling commit_transaction
  // or roll_back_transaction. Doing so begins a nested transaction.
  //
  // Inside a transaction, errors are not reported until commit_transaction is
  // called for the outer-most nested transaction.
  //
  // A parser transaction does not cover visits. Use a buffering_visitor
  // if you need to optionally visit.
  //
  // A parser transaction does not cover variables such as in_async_function_.
  // Use with care.
  parser_transaction begin_transaction();

  // After calling commit_transaction, it's almost as if you never called
  // begin_transaction in the first place.
  //
  // commit_transaction does not restore the state of the parser or lexer when
  // begin_transaction was called.
  void commit_transaction(parser_transaction &&);

  // Restore parser state to a prior version.
  //
  // After calling roll_back_transaction, it's as if you never called
  // begin_transaction or subsequently called skip, insert_semicolon, or
  // other functions.
  //
  // roll_back_transaction effectively undoes calls to parse_expression,
  // skip, etc.
  //
  // Calling roll_back_transaction will not report parser or lexer errors which
  // might have been reported if it weren't for begin_transaction.
  void roll_back_transaction(parser_transaction &&);

  [[noreturn]] void crash_on_unimplemented_token(
      const char *qljs_file_name, int qljs_line,
      const char *qljs_function_name);

  [[noreturn]] void crash_on_depth_limit_exceeded();

  template <class Expression, class... Args>
  expression *make_expression(Args &&... args) {
    return this->expressions_.make_expression<Expression>(
        std::forward<Args>(args)...);
  }

  class function_guard {
   public:
    explicit function_guard(parser *, bool was_in_top_level,
                            bool was_in_async_function,
                            bool was_in_generator_function,
                            bool was_in_loop_statement,
                            bool was_in_switch_statement) noexcept;

    function_guard(const function_guard &) = delete;
    function_guard &operator=(const function_guard &) = delete;

    ~function_guard() noexcept;

   private:
    parser *parser_;
    bool was_in_top_level_;
    bool was_in_async_function_;
    bool was_in_generator_function_;
    bool was_in_loop_statement_;
    bool was_in_switch_statement_;
  };

  template <bool parser::*Member>
  class bool_guard {
   public:
    explicit bool_guard(parser *p, bool old_value) noexcept
        : parser_(p), old_value_(old_value) {}

    bool_guard(const bool_guard &) = delete;
    bool_guard &operator=(const bool_guard &) = delete;

    ~bool_guard() noexcept { this->parser_->*Member = this->old_value_; }

   private:
    parser *parser_;
    bool old_value_;
  };

  class depth_guard {
   public:
    explicit depth_guard(parser *p) noexcept;

    depth_guard(const depth_guard &) = delete;
    depth_guard &operator=(const depth_guard &) = delete;

    ~depth_guard() noexcept;

   private:
    parser *parser_;
    int old_depth_;
  };

  struct parse_expression_cache_key {
    const char8 *begin;
    bool in_top_level;
    bool in_async_function;
    bool in_generator_function;
    bool in_loop_statement;
    bool in_switch_statement;
    bool in_class;

    bool operator==(const parse_expression_cache_key &rhs) const noexcept;
    bool operator!=(const parse_expression_cache_key &rhs) const noexcept;

    struct hash {
      std::size_t operator()(const parse_expression_cache_key &) const noexcept;
    };
  };

  quick_lint_js::lexer lexer_;
  error_reporter *error_reporter_;
  parser_options options_;
  quick_lint_js::expression_arena expressions_;

  // Memory used for temporary memory allocations (e.g. vectors on the stack).
  monotonic_allocator temporary_memory_;

  // TODO(strager): unsynchronized_pool_resource is overkill. Pick a better
  // allocator.
  boost::container::pmr::unsynchronized_pool_resource buffering_visitor_memory_;

  bool in_top_level_ = true;
  bool in_async_function_ = false;
  bool in_generator_function_ = false;
  bool in_loop_statement_ = false;
  bool in_switch_statement_ = false;
  bool in_class_ = false;

  // Cache of whether 'await' is an identifier or an operator. This cache is
  // used to avoid quadratic run-time in code like the following:
  //
  //   await / await / await / await / await
  //
  // (In `await/await`, `await` is an identifier. But in `await/await/`, the
  // first `await` is an operator.)
  //
  // The value of each entry indicates the conclusion:
  // * true means 'await' looks like an identifier, thus '/' is the division
  //   operator.
  // * false means 'await' looks like an operator, thus '/' begins a regular
  //   expression literal.
  std::unordered_map<parse_expression_cache_key, bool,
                     parse_expression_cache_key::hash>
      await_slash_is_identifier_divide_cache_;

#if QLJS_HAVE_SETJMP
  bool have_fatal_parse_error_jmp_buf_ = false;
  std::jmp_buf fatal_parse_error_jmp_buf_;
#endif

  int depth_ = 0;

  using loop_guard = bool_guard<&parser::in_loop_statement_>;
  using switch_guard = bool_guard<&parser::in_switch_statement_>;
  using class_guard = bool_guard<&parser::in_class_>;

 public:
  static constexpr const int stack_limit = 150;
  // For testing and internal use only.
  [[nodiscard]] loop_guard enter_loop();
  [[nodiscard]] class_guard enter_class();
};
}

#include <quick-lint-js/parse-expression-inl.h>
#include <quick-lint-js/parse-statement-inl.h>

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
