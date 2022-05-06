// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PARSE_H
#define QUICK_LINT_JS_PARSE_H

#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/diag-reporter.h>
#include <quick-lint-js/diagnostic-types.h>
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
#include <stack>
#include <unordered_map>
#include <utility>

#if QLJS_HAVE_SETJMP
#include <csetjmp>
#endif

#if QLJS_HAVE_FILE_NAME_MACRO
#define QLJS_PARSER_UNIMPLEMENTED()                                        \
  do {                                                                     \
    this->crash_on_unimplemented_token(__FILE_NAME__, __LINE__, __func__); \
  } while (false)
#else
#define QLJS_PARSER_UNIMPLEMENTED()                                   \
  do {                                                                \
    this->crash_on_unimplemented_token(__FILE__, __LINE__, __func__); \
  } while (false)
#endif

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

  // If true, parse JSX language extensions: https://facebook.github.io/jsx/
  bool jsx = false;

  // If true, parse TypeScript instead of JavaScript.
  bool typescript = false;
};

struct parser_transaction {
  // Private to parser's transaction functions. Do not construct, read, or
  // modify.

  explicit parser_transaction(lexer *l, diag_reporter **diag_reporter_pointer,
                              monotonic_allocator *allocator);

  lexer_transaction lex_transaction;
  buffering_diag_reporter reporter;
  diag_reporter *old_diag_reporter;
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
  explicit parser(padded_string_view input, diag_reporter *diag_reporter);
  explicit parser(padded_string_view input, diag_reporter *diag_reporter,
                  parser_options options);

  quick_lint_js::lexer &lexer() noexcept { return this->lexer_; }

  // For testing and internal use only.
  [[nodiscard]] function_guard enter_function(function_attributes);

#if QLJS_HAVE_SETJMP
  // Returns true if parsing succeeded without QLJS_PARSER_UNIMPLEMENTED being
  // called.
  //
  // Returns false if QLJS_PARSER_UNIMPLEMENTED was called.
  bool parse_and_visit_module_catching_fatal_parse_errors(
      parse_visitor_base &v) {
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

  void parse_and_visit_module(parse_visitor_base &v);

  enum class parse_statement_type {
    any_statement,
    any_statement_in_block,
    no_declarations,
  };

  // If a statement was parsed, this function returns true.
  //
  // If a statement was not parsed (e.g. end of file), then:
  // * no tokens are consumed
  // * no diagnostic is reported
  // * this function returns false
  [[nodiscard]] bool parse_and_visit_statement(
      parse_visitor_base &v, parse_statement_type statement_type =
                                 parse_statement_type::any_statement);

  void parse_and_visit_expression(parse_visitor_base &v) {
    this->parse_and_visit_expression(v, precedence{});
  }

  // The Visitor is only used for the bodies of arrow and function expressions.
  expression *parse_expression(parse_visitor_base &v) {
    return this->parse_expression(v, precedence{});
  }

 private:
  enum class variable_context {
    lhs,
    rhs,
  };

  void visit_expression(expression *ast, parse_visitor_base &v,
                        variable_context context);
  void visit_assignment_expression(expression *lhs, expression *rhs,
                                   parse_visitor_base &v);
  void visit_compound_or_conditional_assignment_expression(
      expression *lhs, expression *rhs, parse_visitor_base &v);
  void maybe_visit_assignment(expression *ast, parse_visitor_base &v);

  void parse_and_visit_statement_block_no_scope(parse_visitor_base &v);

  enum class name_requirement {
    optional,
    required_for_export,
    required_for_statement,
  };

  void parse_and_visit_function_declaration(parse_visitor_base &v,
                                            function_attributes attributes,
                                            const char8 *begin,
                                            name_requirement require_name);
  void parse_and_visit_function_parameters_and_body(
      parse_visitor_base &v, std::optional<source_code_span> name,
      function_attributes attributes);
  void parse_and_visit_function_parameters_and_body_no_scope(
      parse_visitor_base &v, std::optional<source_code_span> name,
      function_attributes attributes);
  void parse_and_visit_function_parameters(parse_visitor_base &v);
  std::optional<source_code_span> is_maybe_function_statement();
  // If the function returns nullopt, no tokens are consumed.
  //
  // If the function returns a function_attributes, tokens are consumed until
  // the kw_function (i.e. the * and possibly a following async are skipped)
  // E.g. *async function f() {}
  // In this case `*async` is consumed.
  std::optional<function_attributes> try_parse_function_with_leading_star();

  void parse_and_visit_class(parse_visitor_base &v,
                             name_requirement require_name);
  // Parse the 'class' keyword, the class's optional name, and any extends
  // clause.
  void parse_and_visit_class_heading(parse_visitor_base &v,
                                     name_requirement require_name);
  void parse_and_visit_class_body(parse_visitor_base &v);
  void parse_and_visit_class_member(parse_visitor_base &v);

  void parse_and_visit_typescript_interface(parse_visitor_base &v);
  void parse_and_visit_typescript_interface_extends(parse_visitor_base &v);

  void parse_and_visit_try_maybe_catch_maybe_finally(parse_visitor_base &v);
  [[nodiscard]] bool parse_and_visit_catch_or_finally_or_both(
      parse_visitor_base &v);

  void parse_and_visit_do_while(parse_visitor_base &v);
  void parse_and_visit_for(parse_visitor_base &v);
  void parse_and_visit_while(parse_visitor_base &v);

  void parse_and_visit_if(parse_visitor_base &v);
  void parse_and_visit_switch(parse_visitor_base &v);

  void parse_and_visit_with(parse_visitor_base &v);

  template <class ExpectedParenthesesError, class ExpectedParenthesisError,
            bool CheckForSketchyConditions>
  void parse_and_visit_parenthesized_expression(parse_visitor_base &v);

  void error_on_sketchy_condition(expression *);

  void error_on_class_statement(statement_kind statement_kind);
  void error_on_lexical_declaration(statement_kind statement_kind);
  void error_on_function_statement(statement_kind statement_kind);

  void parse_and_visit_import(parse_visitor_base &v);
  void parse_and_visit_name_space_import(parse_visitor_base &v);
  void parse_and_visit_named_exports_for_import(parse_visitor_base &v);

  void parse_and_visit_export(parse_visitor_base &v);
  void parse_and_visit_named_exports_for_export(
      parse_visitor_base &v,
      bump_vector<token, monotonic_allocator> &out_exported_bad_tokens);
  void parse_and_visit_named_exports(
      parse_visitor_base &v,
      bump_vector<token, monotonic_allocator> *out_exported_bad_tokens);

  void parse_and_visit_variable_declaration_statement(parse_visitor_base &v);
  void parse_and_visit_let_bindings(
      parse_visitor_base &v, const token &declaring_token,
      bool allow_in_operator, bool allow_const_without_initializer = false,
      bool is_in_for_initializer = false);
  // declaring_token is the const/let/var token.
  void parse_and_visit_let_bindings(parse_visitor_base &v,
                                    const token &declaring_token,
                                    variable_kind declaration_kind,
                                    bool allow_in_operator,
                                    bool allow_const_without_initializer,
                                    bool is_in_for_initializer);
  bool is_let_token_a_variable_reference(const token &following_token,
                                         bool allow_declarations) noexcept;
  void visit_binding_element(expression *ast, parse_visitor_base &v,
                             variable_kind declaration_kind,
                             std::optional<source_code_span> declaring_token,
                             variable_init_kind init_kind);

  struct precedence {
    bool binary_operators : 1 = true;
    bool math_or_logical_or_assignment : 1 = true;
    bool commas : 1 = true;
    bool in_operator : 1 = true;

    // If true, parse unexpected trailing identifiers as part of the
    // expression (and emit an error).
    bool trailing_identifiers : 1 = false;

    // If true, try parsing a trailing '{' as the body of an arrow function. For
    // example:
    //
    // (x) { return x; }
    //    ^ missing '=>'
    //
    // If false, stop parsing at a trailing '{' and do not report an error.
    bool trailing_curly_is_arrow_body : 1 = true;

    bool conditional_operator : 1 = true;
  };

  // binary_expression_builder helps in the creation of a
  // expression::binary_operator.
  //
  // Upon construction, binary_expression_builder stores a single expression*.
  // As a binary expression is parsed, other expression*-s are added to the
  // binary_expression_builder.
  class binary_expression_builder {
   public:
    explicit binary_expression_builder(monotonic_allocator *,
                                       expression *first_child);

    expression *last_expression() const noexcept;
    bool has_multiple_children() const noexcept;

    // Returns the given expression*.
    expression *add_child(source_code_span prior_operator_span, expression *);

    void replace_last(expression *new_last_child);

    void reset_after_build(expression *new_first_child);

    expression_arena::array_ptr<expression *> move_expressions(
        expression_arena &) noexcept;
    expression_arena::array_ptr<source_code_span> move_operator_spans(
        expression_arena &) noexcept;

   private:
    expression_arena::vector<expression *> children_;
    expression_arena::vector<source_code_span> operator_spans_;
  };

  void parse_and_visit_expression(parse_visitor_base &v, precedence prec) {
    monotonic_allocator &alloc = *this->expressions_.allocator();
    auto rewind_guard = alloc.make_rewind_guard();

    expression *ast = this->parse_expression(v, prec);
    {
      auto disable_guard = alloc.disable();
      this->visit_expression(ast, v, variable_context::rhs);
    }
  }

  expression *parse_expression(parse_visitor_base &, precedence);
  expression *parse_primary_expression(parse_visitor_base &, precedence);
  expression *parse_async_expression(parse_visitor_base &,
                                     const token &async_token, precedence);
  expression *parse_async_expression_only(parse_visitor_base &,
                                          const token &async_token,
                                          bool allow_in_operator);
  expression *parse_await_expression(parse_visitor_base &,
                                     const token &await_token, precedence prec);
  expression *parse_expression_remainder(parse_visitor_base &, expression *,
                                         precedence);
  void parse_arrow_function_expression_remainder(parse_visitor_base &,
                                                 source_code_span arrow_span,
                                                 binary_expression_builder &,
                                                 bool allow_in_operator);
  // Precondition: Current token is '=>'.
  void parse_arrow_function_expression_remainder(parse_visitor_base &,
                                                 binary_expression_builder &,
                                                 bool allow_in_operator);
  expression *parse_call_expression_remainder(parse_visitor_base &,
                                              expression *callee);
  expression *parse_index_expression_remainder(parse_visitor_base &,
                                               expression *lhs);
  expression *parse_arrow_function_body(
      parse_visitor_base &, function_attributes,
      const char8 *parameter_list_begin, bool allow_in_operator,
      expression_arena::array_ptr<expression *> &&parameters);
  expression *parse_function_expression(parse_visitor_base &,
                                        function_attributes,
                                        const char8 *span_begin);
  expression *parse_object_literal(parse_visitor_base &);
  expression *parse_class_expression(parse_visitor_base &);
  expression *parse_jsx_expression(parse_visitor_base &);
  expression *parse_jsx_element_or_fragment(parse_visitor_base &);
  // tag is optional. If it is nullptr, parse a fragment. Otherwise, parse an
  // element.
  //
  // Precondition: previous token was '<' (for fragments) or an identifier (for
  //               elements).
  // Postcondition: current token is '>' or end_of_file.
  expression *parse_jsx_element_or_fragment(parse_visitor_base &,
                                            identifier *tag,
                                            const char8 *less_begin);
  void check_jsx_attribute(const identifier &attribute_name);
  expression *parse_tagged_template(parse_visitor_base &, expression *tag);
  expression *parse_untagged_template(parse_visitor_base &);

  function_attributes parse_generator_star(function_attributes);

  expression *maybe_wrap_erroneous_arrow_function(expression *arrow_function,
                                                  expression *lhs);

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
  // Inside a transaction, diagnostics are not reported until commit_transaction
  // is called for the outer-most nested transaction.
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
  // Calling roll_back_transaction will not report parser or lexer diagnostics
  // which might have been reported if it weren't for begin_transaction.
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
  diag_reporter *diag_reporter_;
  parser_options options_;
  quick_lint_js::expression_arena expressions_;

  // Memory used for temporary memory allocations (e.g. vectors on the stack).
  monotonic_allocator temporary_memory_;

  // Memory used for strings in diagnostic messages.
  monotonic_allocator diagnostic_memory_;

  // These are stored in a stack here (rather than on the C++ stack via local
  // variables) so that memory can be released in case we call setjmp.
  std::stack<buffering_visitor> buffering_visitor_stack_;

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
  // * true means 'await' looks like an identifier, thus the following '/' or
  //   '/=' or '<' is the division operator.
  // * false means 'await' looks like an operator, thus '/' begins a regular
  //   expression literal or '<' begins a JSX element.
  std::unordered_map<parse_expression_cache_key, bool,
                     parse_expression_cache_key::hash>
      await_is_identifier_cache_;

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

template <class ExpectedParenthesesError, class ExpectedParenthesisError,
          bool CheckForSketchyConditions>
void parser::parse_and_visit_parenthesized_expression(parse_visitor_base &v) {
  bool have_expression_left_paren = this->peek().type == token_type::left_paren;
  if (have_expression_left_paren) {
    this->skip();
  }
  const char8 *expression_begin = this->peek().begin;

  expression *ast = this->parse_expression(v);
  this->visit_expression(ast, v, variable_context::rhs);
  if constexpr (CheckForSketchyConditions) {
    this->error_on_sketchy_condition(ast);
  }

  const char8 *expression_end = this->lexer_.end_of_previous_token();
  bool have_expression_right_paren =
      this->peek().type == token_type::right_paren;
  if (have_expression_right_paren) {
    this->skip();
  }

  if (!have_expression_left_paren && !have_expression_right_paren) {
    this->diag_reporter_->report(ExpectedParenthesesError{
        source_code_span(expression_begin, expression_end)});
  } else if (!have_expression_right_paren) {
    this->diag_reporter_->report(ExpectedParenthesisError{
        .where = source_code_span(expression_end, expression_end),
        .token = ')',
    });
  } else if (!have_expression_left_paren) {
    this->diag_reporter_->report(ExpectedParenthesisError{
        .where = source_code_span(expression_begin, expression_begin),
        .token = '(',
    });
  }
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
