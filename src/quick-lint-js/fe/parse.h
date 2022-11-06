// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_PARSE_H
#define QUICK_LINT_JS_FE_PARSE_H

#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fe/buffering-visitor-stack.h>
#include <quick-lint-js/fe/buffering-visitor.h>
#include <quick-lint-js/fe/diag-reporter.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/expression.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/null-visitor.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/try-catch-stack.h>
#include <utility>

#if QLJS_HAVE_FILE_NAME_MACRO
#define QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(parser)                          \
  do {                                                                         \
    (parser)->crash_on_unimplemented_token(__FILE_NAME__, __LINE__, __func__); \
  } while (false)
#else
#define QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(parser)                     \
  do {                                                                    \
    (parser)->crash_on_unimplemented_token(__FILE__, __LINE__, __func__); \
  } while (false)
#endif

#define QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN_WITH_PARSER( \
    parser, expected_token_type)                            \
  do {                                                      \
    if ((parser)->peek().type != (expected_token_type)) {   \
      QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(parser);        \
    }                                                       \
  } while (false)

#define QLJS_PARSER_UNIMPLEMENTED() QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(this)
#define QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(expected_token_type) \
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN_WITH_PARSER(this, expected_token_type)

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

 public:
  class depth_guard;
  class function_guard;

  explicit parser(padded_string_view input, diag_reporter *diag_reporter,
                  parser_options options);

  quick_lint_js::lexer &lexer() noexcept { return this->lexer_; }

  // For testing and internal use only.
  [[nodiscard]] function_guard enter_function(function_attributes);

  // Returns true if parsing succeeded without QLJS_PARSER_UNIMPLEMENTED being
  // called.
  //
  // Returns false if QLJS_PARSER_UNIMPLEMENTED was called.
  bool parse_and_visit_module_catching_fatal_parse_errors(
      parse_visitor_base &v) {
    return this->catch_fatal_parse_errors(
        [this, &v] { this->parse_and_visit_module(v); });
  }

  // Returns true if parsing succeeded without QLJS_PARSER_UNIMPLEMENTED being
  // called.
  //
  // Returns false if QLJS_PARSER_UNIMPLEMENTED was called.
  //
  // func's return value is ignored.
  template <class Func>
  bool catch_fatal_parse_errors(Func &&func) {
    int old_depth = this->depth_;
    diag_reporter *old_diag_reporter = this->diag_reporter_;

    bool ok = this->fatal_parse_error_stack_.try_catch<bool>(
        [&]() -> bool {
          std::move(func)();
          return true;
        },
        [&](fatal_parse_error &&exception) -> bool {
          // QLJS_PARSER_UNIMPLEMENTED was called.
          this->diag_reporter_ = old_diag_reporter;
          QLJS_ASSERT(this->depth_ >= old_depth);
          this->depth_ = old_depth;

          switch (exception.kind) {
          case fatal_parse_error_kind::depth_limit_exceeded:
            this->diag_reporter_->report(diag_depth_limit_exceeded{
                .token = exception.error_span,
            });
            break;
          case fatal_parse_error_kind::unexpected_token:
            this->diag_reporter_->report(diag_unexpected_token{
                .token = exception.error_span,
            });
            break;
          }

          return false;
        });

    QLJS_ASSERT(this->depth_ == old_depth);
    QLJS_ASSERT(this->diag_reporter_ == old_diag_reporter);
    return ok;
  }

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

  void parse_typescript_colon_for_type();

  void parse_and_visit_typescript_colon_type_expression(parse_visitor_base &v);
  void parse_and_visit_typescript_type_expression(
      parse_visitor_base &v, bool parse_question_as_invalid = true);

  void parse_and_visit_typescript_colon_type_expression_or_type_predicate(
      parse_visitor_base &v);
  void parse_and_visit_typescript_type_expression_or_type_predicate(
      parse_visitor_base &v);

  enum class typescript_type_arrow_or_paren {
    arrow,
    paren,
  };

  void parse_and_visit_typescript_arrow_type_expression(parse_visitor_base &v);
  void parse_and_visit_typescript_arrow_type_expression_after_left_paren(
      parse_visitor_base &v);
  void
  parse_and_visit_typescript_arrow_type_expression_after_left_paren_no_scope(
      parse_visitor_base &v);
  typescript_type_arrow_or_paren
  parse_and_visit_typescript_arrow_or_paren_type_expression(
      parse_visitor_base &v);
  void parse_and_visit_typescript_object_type_expression(parse_visitor_base &v);
  void parse_and_visit_typescript_template_type_expression(
      parse_visitor_base &v);
  void parse_and_visit_typescript_tuple_type_expression(parse_visitor_base &v);

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

  void parse_and_visit_typescript_generic_arguments(parse_visitor_base &v);

 public:  // For testing only.
  void parse_and_visit_typescript_generic_parameters(parse_visitor_base &v);

 private:
  void parse_and_visit_statement_block_no_scope(parse_visitor_base &v);

  enum class name_requirement {
    optional,
    required_for_export,
    required_for_statement,
  };

  enum class function_parameter_parse_result {
    parsed_parameters,
    parsed_parameters_missing_body,
    missing_parameters_ignore_body,
    missing_parameters,
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
  void parse_and_visit_abstract_function_parameters_and_body_no_scope(
      parse_visitor_base &v, std::optional<source_code_span> name,
      function_attributes attributes);
  void parse_and_visit_interface_function_parameters_and_body_no_scope(
      parse_visitor_base &v, std::optional<source_code_span> name,
      function_attributes attributes);
  function_parameter_parse_result parse_and_visit_function_parameters(
      parse_visitor_base &v, std::optional<source_code_span> name);
  void parse_and_visit_function_parameters(parse_visitor_base &v,
                                           variable_kind parameter_kind);
  std::optional<source_code_span> is_maybe_function_statement();
  // If the function returns nullopt, no tokens are consumed.
  //
  // If the function returns a function_attributes, tokens are consumed until
  // the kw_function (i.e. the * and possibly a following async are skipped)
  // E.g. *async function f() {}
  // In this case `*async` is consumed.
  std::optional<function_attributes> try_parse_function_with_leading_star();

  // See parse_end_of_typescript_overload_signature.
  struct overload_signature_parse_result {
    // Invariant: is_overload_signature ? !has_missing_body_error : true
    // Invariant: is_overload_signature == second_function_name.has_value()

    // If true, the first function was an overload signature, and the lexer is
    // at the '(' in the second function.
    //
    // If false, the first function was not an overload signature, and the lexer
    // did not change position.
    bool is_overload_signature;

    // If true, the first function's missing body is an error. If false, the
    // first function's missing body is not an error.
    bool has_missing_body_error;

    // If is_overload_signature is true, then *second_function_name is the name
    // of the second function.
    std::optional<identifier> second_function_name;

    // If is_overload_signature is true, then second_function_attributes is the
    // attributes of the second function.
    function_attributes second_function_attributes;

    // If is_overload_signature is true, then second_function_generator_star is
    // the span of the second function's '*' (or nullopt if it doesn't have a
    // '*').
    std::optional<source_code_span> second_function_generator_star;
  };

  // Given the following code:
  //
  //     function f()
  //     function f() {}
  //
  // Immediately after parsing ')', you should call this function.
  //
  // See overload_signature_parse_result's members for details on the effects of
  // this function.
  overload_signature_parse_result parse_end_of_typescript_overload_signature(
      const identifier &function_name);

  void parse_and_visit_class(
      parse_visitor_base &v, name_requirement require_name,
      std::optional<source_code_span> abstract_keyword_span);
  // Parse the 'class' keyword and the class's optional name.
  std::optional<identifier> parse_class_and_optional_name();
  // Parse any extends clauses after the class's name.
  void parse_and_visit_class_heading_after_name(parse_visitor_base &v);
  void parse_and_visit_class_extends(parse_visitor_base &v);
  void parse_and_visit_typescript_class_implements(parse_visitor_base &v);
  void visit_class_name(parse_visitor_base &v,
                        std::optional<identifier> class_name,
                        source_code_span class_keyword_span,
                        name_requirement require_name);
  void parse_and_visit_class_body(parse_visitor_base &v,
                                  source_code_span class_keyword_span,
                                  bool is_abstract);
  void parse_and_visit_class_or_interface_member(
      parse_visitor_base &v, source_code_span class_or_interface_keyword_span,
      bool is_interface, bool is_abstract);

  void parse_and_visit_typescript_interface(
      parse_visitor_base &v, source_code_span interface_keyword_span);
  void parse_and_visit_typescript_interface_extends(parse_visitor_base &v);
  void parse_and_visit_typescript_interface_body(
      parse_visitor_base &v, source_code_span interface_keyword_span);

  // Parse a single item in a class 'implements' clause or an interface
  // 'extends' clause.
  void parse_and_visit_typescript_interface_reference(parse_visitor_base &v);

  void parse_and_visit_typescript_namespace(
      parse_visitor_base &v, source_code_span namespace_keyword_span);

  void parse_and_visit_typescript_type_alias(parse_visitor_base &v,
                                             source_code_span type_token);

  enum class enum_value_kind {
    constant,
    computed,
    unknown,
  };
  void parse_and_visit_typescript_enum(parse_visitor_base &v, enum_kind);
  void parse_and_visit_typescript_enum_members(parse_visitor_base &v,
                                               enum_kind);
  static enum_value_kind classify_enum_value_expression(
      const expression *ast) noexcept;

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
  void error_on_pointless_string_compare(expression::binary_operator *);
  void error_on_pointless_compare_against_literal(
      expression::binary_operator *);
  void check_compare_against_literal(expression *, expression *,
                                     source_code_span op_span);
  void error_on_invalid_as_const(expression *, source_code_span as_const_span);

  void error_on_class_statement(statement_kind statement_kind);
  void error_on_lexical_declaration(statement_kind statement_kind);
  void error_on_function_statement(statement_kind statement_kind);

  void parse_and_visit_import(parse_visitor_base &v);
  void parse_and_visit_name_space_import(parse_visitor_base &v);
  void parse_and_visit_named_exports_for_import(parse_visitor_base &v);
  void parse_and_visit_named_exports_for_typescript_type_only_import(
      parse_visitor_base &v, source_code_span type_keyword);

  void parse_and_visit_export(parse_visitor_base &v);
  void parse_and_visit_named_exports(
      parse_visitor_base &v,
      std::optional<source_code_span> typescript_type_only_keyword,
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

  struct binding_element_info {
    variable_kind declaration_kind;
    std::optional<source_code_span> declaring_token;
    variable_init_kind init_kind;

    // Valid only if declaration_kind == variable_kind::_function_parameter.
    const char8 *first_parameter_begin;

    bool is_destructuring = false;
    const char8 *spread_operator_begin = nullptr;

    bool has_spread_operator() const noexcept {
      return this->spread_operator_begin != nullptr;
    }

    source_code_span spread_operator_span() const noexcept {
      QLJS_ASSERT(this->has_spread_operator());
      return source_code_span(
          this->spread_operator_begin,
          this->spread_operator_begin + this->spread_operator_length);
    }

    binding_element_info with_init_kind(variable_init_kind init_kind) const
        noexcept {
      binding_element_info result = *this;
      result.init_kind = init_kind;
      return result;
    }

    binding_element_info with_destructuring() const noexcept {
      binding_element_info result = *this;
      result.is_destructuring = true;
      return result;
    }

    binding_element_info with_spread(source_code_span spread_operator) const
        noexcept {
      QLJS_ASSERT(spread_operator.end() - spread_operator.begin() ==
                  spread_operator_length);
      binding_element_info result = *this;
      result.spread_operator_begin = spread_operator.begin();
      return result;
    }

    static constexpr int spread_operator_length =
        expression::spread::spread_operator_length;
  };
  void visit_binding_element(expression *ast, parse_visitor_base &v,
                             const binding_element_info &);

  // FIXME(strager): This should be an enum class, but that causes GCC 8.3.0 to
  // fail compilation:
  //
  // > error: cannot convert 'quick_lint_js::parser::allow_type_annotations' to
  // > 'unsigned char:2' in initialization
  enum allow_type_annotations : std::uint8_t {
    typescript_only,
    always,
    never,
  };

  struct precedence {
    bool binary_operators : 1 = true;
    bool math_or_logical_or_assignment : 1 = true;
    bool equals_assignment : 1 = true;
    bool commas : 1 = true;
    bool in_operator : 1 = true;
    allow_type_annotations colon_type_annotation : 2 =
        allow_type_annotations::typescript_only;

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

    // If true, parse '?:' as the '?' optional indicator on a function parameter
    // followed by the ':' type annotation indicator on that function parameter.
    //
    // If false, parse '?:' as the conditional operator with no expression in
    // between.
    bool colon_question_is_typescript_optional_with_type_annotation : 1 = false;
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

  // Be sure to call builder.reset_after_build immediately after calling
  // build_expression.
  expression *build_expression(binary_expression_builder &builder);

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
  // Parses either:
  // * an expression starting with 'async', or
  // * an expression starting with 'await' where we determined that it is either
  //   a unary operator or a mistyped 'async'.
  expression *parse_async_expression_only(parse_visitor_base &,
                                          const token &async_or_await_token,
                                          bool allow_in_operator);
  expression *parse_await_expression(parse_visitor_base &,
                                     const token &await_token, precedence prec);
  expression *parse_expression_remainder(parse_visitor_base &, expression *,
                                         precedence);
  expression *parse_arrow_function_expression_remainder(
      parse_visitor_base &, buffering_visitor *generic_parameter_visits,
      expression *parameters_expression, buffering_visitor *return_type_visits,
      bool allow_in_operator);
  expression::call *parse_call_expression_remainder(parse_visitor_base &,
                                                    expression *callee);
  expression *parse_index_expression_remainder(parse_visitor_base &,
                                               expression *lhs);
  expression_arena::vector<expression *>
  parse_arrow_function_parameters_or_call_arguments(parse_visitor_base &v);
  expression *parse_arrow_function_body(
      parse_visitor_base &, function_attributes,
      const char8 *parameter_list_begin, bool allow_in_operator,
      expression_arena::array_ptr<expression *> &&parameters,
      buffering_visitor *return_type_visits);
  expression *parse_arrow_function_body_no_scope(
      parse_visitor_base &, function_attributes,
      const char8 *parameter_list_begin, bool allow_in_operator,
      expression_arena::array_ptr<expression *> &&parameters,
      buffering_visitor *return_type_visits);
  expression *parse_function_expression(parse_visitor_base &,
                                        function_attributes,
                                        const char8 *span_begin);
  expression *parse_object_literal(parse_visitor_base &);
  expression *parse_class_expression(parse_visitor_base &);
  expression *parse_jsx_expression(parse_visitor_base &);
  expression *parse_jsx_or_typescript_generic_expression(parse_visitor_base &,
                                                         precedence);
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
  expression *parse_typescript_generic_arrow_expression(parse_visitor_base &,
                                                        bool allow_in_operator);
  expression *parse_typescript_angle_type_assertion_expression(
      parse_visitor_base &, precedence, bool is_invalid_due_to_jsx_ambiguity);
  expression *parse_tagged_template(parse_visitor_base &, expression *tag);
  expression *parse_untagged_template(parse_visitor_base &);

  // If a generator '*' is parsed, modifies *attributes and returns the span of
  // the '*'.
  std::optional<source_code_span> parse_generator_star(
      function_attributes *attributes);

  void check_assignment_lhs(expression *);

  void check_body_after_label();

  expression *maybe_wrap_erroneous_arrow_function(expression *arrow_function,
                                                  expression *lhs);

  void consume_semicolon_after_statement();
  template <class MissingSemicolonDiagnostic>
  void consume_semicolon();

  const token &peek() const noexcept { return this->lexer_.peek(); }
  void skip() noexcept { this->lexer_.skip(); }

 public:
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

  // Speculatively parse something. If parsing turned out to be a bad idea, roll
  // back and do something else.
  //
  // try_func is always called. It should return a boolean.
  //
  // catch_func is called if try_func returns false.
  //
  // When try_func and catch_func are called, they start with the same lexer
  // state.
  template <class TryFunc, class CatchFunc>
  void try_parse(TryFunc &&try_func, CatchFunc &&catch_func) {
    parser_transaction transaction = this->begin_transaction();
    bool should_commit = std::move(try_func)();
    if (should_commit) {
      this->commit_transaction(std::move(transaction));
    } else {
      this->roll_back_transaction(std::move(transaction));
      std::move(catch_func)();
    }
  }

  [[noreturn]] void crash_on_unimplemented_token(
      const char *qljs_file_name, int qljs_line,
      const char *qljs_function_name);

  [[noreturn]] void crash_on_depth_limit_exceeded();

 private:
  template <class Expression, class... Args>
  expression *make_expression(Args &&... args) {
    return this->expressions_.make_expression<Expression>(
        std::forward<Args>(args)...);
  }

 public:
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

 private:
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

 public:
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

 private:
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

  parse_expression_cache_key parse_expression_cache_key_for_current_state()
      const;

  quick_lint_js::lexer lexer_;
  diag_reporter *diag_reporter_;
  diag_reporter *original_diag_reporter_;
  parser_options options_;
  expression_arena expressions_;

  // Memory used for temporary memory allocations (e.g. vectors on the stack).
  monotonic_allocator temporary_memory_{"parser::temporary_memory_"};

  // Memory used for strings in diagnostic messages.
  monotonic_allocator diagnostic_memory_{"parser::diagnostic_memory_"};

  // Memory used for TypeScript type expressions.
  // TODO(strager): Rewind periodically (e.g. after parsing a function body).
  monotonic_allocator type_expression_memory_{
      "parser::type_expression_memory_"};

  // These are stored in a stack here (rather than on the C++ stack via local
  // variables) so that memory can be released in case we call setjmp.
  buffering_visitor_stack buffering_visitor_stack_;

  bool in_top_level_ = true;
  bool in_async_function_ = false;
  bool in_generator_function_ = false;
  bool in_loop_statement_ = false;
  bool in_switch_statement_ = false;
  bool in_class_ = false;

  bool in_typescript_only_construct_ = false;

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
  hash_map<parse_expression_cache_key, bool, parse_expression_cache_key::hash>
      await_is_identifier_cache_;

  enum class fatal_parse_error_kind {
    depth_limit_exceeded,
    unexpected_token,
  };
  struct fatal_parse_error {
    source_code_span error_span = source_code_span::unit(nullptr);
    fatal_parse_error_kind kind;
  };
  try_catch_stack<fatal_parse_error> fatal_parse_error_stack_;

 public:
  using loop_guard = bool_guard<&parser::in_loop_statement_>;
  using switch_guard = bool_guard<&parser::in_switch_statement_>;
  using class_guard = bool_guard<&parser::in_class_>;

  using typescript_only_construct_guard =
      bool_guard<&parser::in_typescript_only_construct_>;

  int depth_ = 0;

  // TODO(#735): Reduce stack usage in our parse functions and increase this
  // limit.
  static constexpr const int stack_limit = 130;

  // For testing and internal use only.
  [[nodiscard]] loop_guard enter_loop();
  [[nodiscard]] class_guard enter_class();

 private:
  [[nodiscard]] typescript_only_construct_guard
  enter_typescript_only_construct();
  [[nodiscard]] switch_guard enter_switch();
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
        .where = source_code_span::unit(expression_end),
        .token = ')',
    });
  } else if (!have_expression_left_paren) {
    this->diag_reporter_->report(ExpectedParenthesisError{
        .where = source_code_span::unit(expression_begin),
        .token = '(',
    });
  }
}

extern template void
parser::consume_semicolon<diag_missing_semicolon_after_abstract_method>();
extern template void
parser::consume_semicolon<diag_missing_semicolon_after_field>();
extern template void
parser::consume_semicolon<diag_missing_semicolon_after_index_signature>();
extern template void
parser::consume_semicolon<diag_missing_semicolon_after_interface_method>();
extern template void
parser::consume_semicolon<diag_missing_semicolon_after_statement>();
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
