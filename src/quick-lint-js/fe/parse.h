// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/buffering-visitor-stack.h>
#include <quick-lint-js/fe/buffering-visitor.h>
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
enum class Parser_Top_Level_Await_Mode {
  auto_detect = 0,
  await_operator,
};

// TODO(#465): Accept parser options from quick-lint-js.config or CLI options.
struct Parser_Options {
  Parser_Top_Level_Await_Mode top_level_await_mode =
      Parser_Top_Level_Await_Mode::auto_detect;

  // If true, parse JSX language extensions: https://facebook.github.io/jsx/
  bool jsx = false;

  // If true, parse TypeScript instead of JavaScript.
  bool typescript = false;

  // If true, parse as a TypeScript definition file (.d.ts).
  //
  // Invariant: typescript_definition_file implies typescript.
  bool typescript_definition_file = false;
};

struct Parser_Transaction {
  // Private to Parser's transaction functions. Do not construct, read, or
  // modify.

  explicit Parser_Transaction(Lexer *l, Diag_Reporter **diag_reporter_pointer,
                              Monotonic_Allocator *allocator);

  Lexer_Transaction lex_transaction;
  Buffering_Diag_Reporter reporter;
  Diag_Reporter *old_diag_reporter;
};

// A Parser reads JavaScript source code and calls the member functions of a
// Parse_Visitor (visit_variable_declaration, visit_enter_function_scope, etc.).
class Parser {
 private:
  template <bool Parser::*Member>
  class Bool_Guard;

 public:
  class Depth_Guard;
  class Function_Guard;

  explicit Parser(Padded_String_View input, Diag_Reporter *diag_reporter,
                  Parser_Options options);

  quick_lint_js::Lexer &lexer() { return this->lexer_; }

  // For testing and internal use only.
  [[nodiscard]] Function_Guard enter_function(Function_Attributes);

  // Returns true if parsing succeeded without QLJS_PARSER_UNIMPLEMENTED being
  // called.
  //
  // Returns false if QLJS_PARSER_UNIMPLEMENTED was called.
  bool parse_and_visit_module_catching_fatal_parse_errors(
      Parse_Visitor_Base &v);

  // Returns true if parsing succeeded without QLJS_PARSER_UNIMPLEMENTED being
  // called.
  //
  // Returns false if QLJS_PARSER_UNIMPLEMENTED was called.
  //
  // func's return value is ignored.
  template <class Func>
  bool catch_fatal_parse_errors(Func &&func) {
    int old_depth = this->depth_;
    Diag_Reporter *old_diag_reporter = this->diag_reporter_;

    bool ok = this->fatal_parse_error_stack_.try_catch<bool>(
        [&]() -> bool {
          std::move(func)();
          return true;
        },
        [&](Fatal_Parse_Error &&exception) -> bool {
          // QLJS_PARSER_UNIMPLEMENTED was called.
          this->diag_reporter_ = old_diag_reporter;
          QLJS_ASSERT(this->depth_ >= old_depth);
          this->depth_ = old_depth;

          switch (exception.kind) {
          case Fatal_Parse_Error_Kind::depth_limit_exceeded:
            this->diag_reporter_->report(Diag_Depth_Limit_Exceeded{
                .token = exception.error_span,
            });
            break;
          case Fatal_Parse_Error_Kind::unexpected_token:
            this->diag_reporter_->report(Diag_Unexpected_Token{
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

  void parse_and_visit_module(Parse_Visitor_Base &v);

  struct Parse_Statement_Options {
    // If false, 'let' can be a loop label or a variable reference, but not a
    // variable declaration.
    bool allow_let_declaration : 1 = true;

    bool possibly_followed_by_another_statement : 1 = false;

    // If true, this is a .d.ts file (Parse_Options::typescript_definition_file
    // is true) and we are parsing a top-level statement.
    bool top_level_typescript_definition : 1 = false;

    // If true, we require that the statement declares something (e.g. a
    // variable or interface). The statement does not necessarily need to use
    // the 'declare' keyword.
    //
    // If true and Parse_Options::typescript_definition_file is false, then
    // declare_keyword must be non-null.
    bool require_declaration : 1 = false;

    // Must be non-null if require_declaration is true and
    // Parse_Options::typescript_definition_file is false
    std::optional<Source_Code_Span> declare_keyword = std::nullopt;
  };

  // If a statement was parsed, this function returns true.
  //
  // If a statement was not parsed (e.g. end of file), then:
  // * no tokens are consumed
  // * no diagnostic is reported
  // * this function returns false
  [[nodiscard]] bool parse_and_visit_statement(Parse_Visitor_Base &v) {
    return this->parse_and_visit_statement(v, Parse_Statement_Options());
  }
  [[nodiscard]] bool parse_and_visit_statement(Parse_Visitor_Base &v,
                                               Parse_Statement_Options options);

  void parse_and_visit_expression(Parse_Visitor_Base &v) {
    this->parse_and_visit_expression(v, Precedence{});
  }

  // The Visitor is only used for the bodies of arrow and function expressions.
  Expression *parse_expression(Parse_Visitor_Base &v) {
    return this->parse_expression(v, Precedence{});
  }

  void parse_typescript_colon_for_type();

  struct TypeScript_Type_Parse_Options {
    struct Declaring_Type {
      Identifier name;
      Variable_Kind kind;
    };

    std::optional<Declaring_Type> type_being_declared = std::nullopt;
    bool parse_question_as_invalid = true;
    // If false, assertion predicates and type predicates are parsed but a
    // diagnostic is reported.
    bool allow_assertion_signature_or_type_predicate = false;
    // When parsing a generic argument list, if the '<' is preceeded by a
    // newline, TypeScript always stops parsing the type. quick-lint-js is
    // smarter. If this boolean is false, quick-lint-js will report a diagnostic
    // but parse the generic argument list.
    bool stop_parsing_type_at_newline_before_generic_arguments = true;
    // When parsing, if 'extends' is preceeded by a newline, TypeScript always
    // stops parsing the type before 'extends'. quick-lint-js is smarter. If
    // this boolean is false, quick-lint-js will report a diagnostic but parse
    // the 'extends' as part of the type.
    bool stop_parsing_type_at_newline_before_extends = true;
    // When parsing, if 'asserts' is followed by a newline, TypeScript always
    // stops parsing, treating 'asserts' as a type name. quick-lint-js is
    // smarter. If this boolean is false, quick-lint-js will report a diagnostic
    // but parse the 'asserts' as part of the type.
    //
    // This is relevant only if allow_assertion_signature_or_type_predicate is
    // true.
    bool stop_parsing_type_at_newline_after_asserts = true;
    // If true, the 'extends' operator is allowed and defines a conditional
    // type. If false, the 'extends' keyword may constrain 'infer T'.
    bool extends_is_conditional_type = true;
  };

  void parse_and_visit_typescript_colon_type_expression(Parse_Visitor_Base &v);
  void parse_and_visit_typescript_colon_type_expression(
      Parse_Visitor_Base &v, const TypeScript_Type_Parse_Options &);
  void parse_and_visit_typescript_type_expression(Parse_Visitor_Base &v);
  void parse_and_visit_typescript_type_expression(
      Parse_Visitor_Base &v, const TypeScript_Type_Parse_Options &);
  // The _no_scope variant does not emit visit_enter_type_scope or
  // visit_exit_type_scope.
  void parse_and_visit_typescript_type_expression_no_scope(
      Parse_Visitor_Base &v);
  void parse_and_visit_typescript_type_expression_no_scope(
      Parse_Visitor_Base &v, const TypeScript_Type_Parse_Options &);

  enum class TypeScript_Type_Arrow_Or_Paren {
    arrow,
    paren,
  };

  void parse_and_visit_typescript_arrow_type_expression(Parse_Visitor_Base &v);
  void parse_and_visit_typescript_arrow_type_expression_after_left_paren(
      Parse_Visitor_Base &v);
  void
  parse_and_visit_typescript_arrow_type_expression_after_left_paren_no_scope(
      Parse_Visitor_Base &v);
  void parse_and_visit_typescript_arrow_type_arrow_and_return_type_no_scope(
      Parse_Visitor_Base &v);
  TypeScript_Type_Arrow_Or_Paren
  parse_and_visit_typescript_arrow_or_paren_type_expression(
      Parse_Visitor_Base &v, const TypeScript_Type_Parse_Options &);
  // Parse 'import(...)'.
  void parse_and_visit_typescript_import_type_expression(Parse_Visitor_Base &v);
  void parse_and_visit_typescript_object_type_expression(Parse_Visitor_Base &v);
  void parse_and_visit_typescript_template_type_expression(
      Parse_Visitor_Base &v, const TypeScript_Type_Parse_Options &);
  void parse_and_visit_typescript_tuple_type_expression(Parse_Visitor_Base &v);

 private:
  enum class Variable_Context {
    lhs,
    rhs,
  };

  void visit_expression(Expression *ast, Parse_Visitor_Base &v,
                        Variable_Context context);
  void visit_assignment_expression(Expression *lhs, Expression *rhs,
                                   Parse_Visitor_Base &v);
  void visit_compound_or_conditional_assignment_expression(
      Expression *lhs, Expression *rhs, Parse_Visitor_Base &v);
  void maybe_visit_assignment(Expression *ast, Parse_Visitor_Base &v,
                              Variable_Assignment_Flags flags);

  void parse_and_visit_typescript_generic_arguments(Parse_Visitor_Base &v);
  void parse_and_visit_typescript_generic_arguments_no_scope(
      Parse_Visitor_Base &v);

 public:  // For testing only.
  void parse_and_visit_typescript_generic_parameters(Parse_Visitor_Base &v);

 private:
  void parse_and_visit_statement_block_no_scope(Parse_Visitor_Base &v);
  void parse_and_visit_statement_block_no_scope(
      Parse_Visitor_Base &v, Parse_Statement_Options statement_options);
  // Parses the closing '}', if present.
  void parse_and_visit_statement_block_after_left_curly(
      Parse_Visitor_Base &v, Source_Code_Span left_curly_span);
  void parse_and_visit_statement_block_after_left_curly(
      Parse_Visitor_Base &v, Source_Code_Span left_curly_span,
      Parse_Statement_Options statement_options);

  enum class Name_Requirement {
    optional,
    required_for_export,
    required_for_statement,
  };

  enum class Function_Parameter_Parse_Result {
    parsed_parameters,
    parsed_parameters_missing_body,
    missing_parameters_ignore_body,
    missing_parameters,
  };

  struct Parameter_List_Options {
    std::optional<Source_Code_Span> declare_class_keyword = std::nullopt;
    std::optional<Source_Code_Span> abstract_method_keyword = std::nullopt;
    bool is_class_constructor = false;
    bool is_declare_function = false;
    bool is_class_method = false;
    bool is_interface_method = false;
  };

  struct Function_Declaration_Options {
    Function_Attributes attributes;
    const Char8 *begin;
    Name_Requirement require_name;
    std::optional<Source_Code_Span> async_keyword;
    // TypeScript 'declare', either direct or from 'declare namespace'.
    std::optional<Source_Code_Span> declare_keyword;
    std::optional<Source_Code_Span> export_keyword;
  };

  void parse_and_visit_function_declaration(
      Parse_Visitor_Base &v, Function_Declaration_Options options);
  void parse_and_visit_function_parameters_and_body(
      Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
      Function_Attributes attributes, Parameter_List_Options);
  void parse_and_visit_function_parameters_and_body_no_scope(
      Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
      Function_Attributes attributes, Parameter_List_Options);
  void parse_and_visit_abstract_function_parameters_and_body_no_scope(
      Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
      Function_Attributes attributes, Parameter_List_Options);
  void parse_and_visit_declare_class_method_parameters_and_body(
      Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
      Function_Attributes attributes, Parameter_List_Options);
  void parse_and_visit_interface_function_parameters_and_body_no_scope(
      Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
      Function_Attributes attributes, Parameter_List_Options);

  // Parse a function's parameter list, including the surrounding parentheses.
  // Tries to handle things like an extra function name before the parameter
  // list or a generator '*' before the parameter list.
  Function_Parameter_Parse_Result parse_and_visit_function_parameter_list(
      Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
      Parameter_List_Options);

  // Parse a function's parameter list assuming the lexer points to the first
  // parameter (not '(').
  void parse_and_visit_function_parameters(Parse_Visitor_Base &v,
                                           Variable_Kind parameter_kind,
                                           Parameter_List_Options);

  std::optional<Source_Code_Span> is_maybe_function_statement();

  // If the function returns nullopt, no tokens are consumed.
  //
  // If the function returns a function_attributes, tokens are consumed until
  // the kw_function (i.e. the * and possibly a following async are skipped)
  // E.g. *async function f() {}
  // In this case `*async` is consumed.
  std::optional<Function_Attributes> try_parse_function_with_leading_star();

  // See parse_end_of_typescript_overload_signature.
  struct Overload_Signature_Parse_Result {
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
    std::optional<Identifier> second_function_name;

    // If is_overload_signature is true, then second_function_attributes is the
    // attributes of the second function.
    Function_Attributes second_function_attributes;

    // If is_overload_signature is true, then second_function_generator_star is
    // the span of the second function's '*' (or nullopt if it doesn't have a
    // '*').
    std::optional<Source_Code_Span> second_function_generator_star;

    // If is_overload_signature is true, then second_function_export_keyword is
    // the span of the second function's 'export' modifier (or nullopt if it
    // doesn't have a 'export').
    std::optional<Source_Code_Span> second_function_export_keyword;

    // If is_overload_signature is true, then second_function_expected_export is
    // the empty span where the 'export' keyword should be written if it is
    // missing.
    std::optional<Source_Code_Span> second_function_expected_export;
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
  Overload_Signature_Parse_Result parse_end_of_typescript_overload_signature(
      const Identifier &function_name);

  // Parse just the '@decorator' syntax.
  void parse_and_visit_decorator(Parse_Visitor_Base &v);
  void parse_and_visit_one_or_more_decorators(Parse_Visitor_Base &v);
  // Parse '@decorator' followed by a class statement.
  void parse_and_visit_decorator_statement(Parse_Visitor_Base &v);

  struct Parse_Class_Options {
    Name_Requirement require_name;
    std::optional<Source_Code_Span> abstract_keyword_span;
    std::optional<Source_Code_Span> declare_keyword_span;
    bool is_top_level_typescript_definition_without_declare_or_export = false;
  };

  struct Parse_Class_Body_Options {
    Source_Code_Span class_or_interface_keyword_span;
    bool is_abstract;
    std::optional<Source_Code_Span> declare_keyword;
    bool is_interface;
  };

  void parse_and_visit_class(Parse_Visitor_Base &v, Parse_Class_Options);
  // Parse the 'class' keyword and the class's optional name.
  std::optional<Identifier> parse_class_and_optional_name();
  // Parse any extends clauses after the class's name.
  void parse_and_visit_class_heading_after_name(Parse_Visitor_Base &v);
  void parse_and_visit_class_extends(Parse_Visitor_Base &v);
  void parse_and_visit_typescript_class_implements(Parse_Visitor_Base &v);
  void visit_class_name(Parse_Visitor_Base &v,
                        std::optional<Identifier> class_name,
                        Source_Code_Span class_keyword_span,
                        Name_Requirement require_name);
  void parse_and_visit_class_body(Parse_Visitor_Base &v,
                                  Parse_Class_Body_Options);
  void parse_and_visit_class_or_interface_member(Parse_Visitor_Base &v,
                                                 Parse_Class_Body_Options);

  void parse_and_visit_typescript_interface(
      Parse_Visitor_Base &v, Source_Code_Span interface_keyword_span);
  void parse_and_visit_typescript_interface_extends(Parse_Visitor_Base &v);
  void parse_and_visit_typescript_interface_body(
      Parse_Visitor_Base &v, Source_Code_Span interface_keyword_span);

  // Parse a single item in a class 'implements' clause or an interface
  // 'extends' clause.
  void parse_and_visit_typescript_interface_reference(Parse_Visitor_Base &v,
                                                      Statement_Kind context);

  // Parse a TypeScript namespace declared with either 'namespace' or 'module'.
  //
  // If the code looks like a TypeScript module (e.g. 'module "modname" {}'), it
  // is interpreted as a 'declare module' (as if
  // parse_and_visit_typescript_declare_namespace was called instead).
  void parse_and_visit_typescript_namespace(
      Parse_Visitor_Base &v,
      std::optional<Source_Code_Span> export_keyword_span,
      Source_Code_Span namespace_keyword_span);

  // If a namespace head is parsed (even if it's declared with the 'module'
  // keyword), returns the outer-most declared identifier. If a module head is
  // parsed, returns nullopt.
  //
  // Examples:
  //
  //   namespace ns {}        // Returns "ns".
  //   module ns {}           // Returns "ns".
  //   namespace ns.subns {}  // Returns "ns".
  //   module ns.subns {}     // Returns "ns".
  //   module "mymod" {}      // Returns nullopt.
  std::optional<Identifier> parse_and_visit_typescript_namespace_or_module_head(
      Parse_Visitor_Base &v,
      std::optional<Source_Code_Span> export_keyword_span,
      std::optional<Source_Code_Span> declare_keyword_span,
      Source_Code_Span namespace_or_module_keyword_span);

  // Parse an ambient TypeScript namespace declared with either 'declare
  // namespace' or 'declare module' or a TypeScript module declared with
  // 'declare module'.
  void parse_and_visit_typescript_declare_namespace_or_module(
      Parse_Visitor_Base &v, Source_Code_Span declare_keyword_span);

  // Information about how the TypeScript 'declare' keyword was used.
  struct TypeScript_Declare_Context {
    // If present, the parser found a containing 'declare namespace' or
    // 'declare module' or 'declare global'.
    std::optional<Source_Code_Span> declare_namespace_declare_keyword =
        std::nullopt;

    // If present, the parser found a 'declare' keyword immediately before the
    // statement being parsed.
    std::optional<Source_Code_Span> direct_declare_keyword = std::nullopt;

    // If true, we are inside a TypeScript ambient module. For example:
    //
    // module "modulename" {
    //   /* in_module is true here. */
    // }
    bool in_module = false;

    // Precondition: declare_namespace_declare_keyword.has_value()
    //               || direct_declare_keyword.has_value()
    Source_Code_Span declare_keyword_span() const;

    std::optional<Source_Code_Span> maybe_declare_keyword_span() const;
  };

  // Parse a TypeScript ambient code block, including curlies. An ambient code
  // block is the body of a 'declare namespace', for example.
  void parse_and_visit_typescript_declare_block(
      Parse_Visitor_Base &v, const TypeScript_Declare_Context &declare_context);

  void parse_and_visit_typescript_type_alias(Parse_Visitor_Base &v,
                                             Source_Code_Span type_token);

  enum class Enum_Value_Kind {
    constant,
    computed,
    unknown,
  };
  void parse_and_visit_typescript_enum(Parse_Visitor_Base &v, Enum_Kind);
  void parse_and_visit_typescript_enum_members(Parse_Visitor_Base &v,
                                               Enum_Kind);
  static Enum_Value_Kind classify_enum_value_expression(const Expression *ast);

  void parse_and_visit_try_maybe_catch_maybe_finally(Parse_Visitor_Base &v);
  [[nodiscard]] bool parse_and_visit_catch_or_finally_or_both(
      Parse_Visitor_Base &v);

  void parse_and_visit_do_while(Parse_Visitor_Base &v);
  void parse_and_visit_for(Parse_Visitor_Base &v);
  void parse_and_visit_while(Parse_Visitor_Base &v);

  void parse_and_visit_if(Parse_Visitor_Base &v);
  void parse_and_visit_switch(Parse_Visitor_Base &v);

  void parse_and_visit_with(Parse_Visitor_Base &v);

  template <class Expected_Parentheses_Error, class Expected_Parenthesis_Error,
            bool check_for_sketchy_conditions, bool check_for_comma_operator>
  void parse_and_visit_parenthesized_expression(Parse_Visitor_Base &v,
                                                Source_Code_Span token);

  void error_on_sketchy_condition(Expression *);
  void warn_on_comma_operator_in_conditional_statement(Expression *);
  void warn_on_comma_operator_in_index(Expression *, Source_Code_Span);
  void warn_on_xor_operator_as_exponentiation(Expression::Binary_Operator *);
  void warn_on_unintuitive_bitshift_precedence(Expression *ast);
  void error_on_pointless_string_compare(Expression::Binary_Operator *);
  void error_on_pointless_compare_against_literal(
      Expression::Binary_Operator *);
  void check_compare_against_literal(Expression *, Expression *,
                                     Source_Code_Span op_span);
  void error_on_invalid_as_const(Expression *, Source_Code_Span as_const_span);

  void error_on_class_statement(Statement_Kind statement_kind);
  void error_on_lexical_declaration(Statement_Kind statement_kind);
  void error_on_function_statement(Statement_Kind statement_kind);

  void parse_and_visit_import(Parse_Visitor_Base &v);
  void parse_and_visit_import(
      Parse_Visitor_Base &v, const TypeScript_Declare_Context &declare_context);
  void parse_and_visit_name_space_import(Parse_Visitor_Base &v);
  void parse_and_visit_named_exports_for_import(Parse_Visitor_Base &v);
  void parse_and_visit_named_exports_for_typescript_type_only_import(
      Parse_Visitor_Base &v, Source_Code_Span type_keyword);

  // If set, refers to the first `export default` statement in this module. A
  // module cannot contain more than one `export default`.
  std::optional<Source_Code_Span>
      first_export_default_statement_default_keyword_ = std::nullopt;

  struct Parse_Export_Options {
    TypeScript_Declare_Context declare_context;

    // Location of '@' in a decorator if this export has a decorator. Example:
    //
    //   @myDecorator export class C {}
    //   ^
    std::optional<Source_Code_Span> decorator_at_span;
  };
  void parse_and_visit_export(Parse_Visitor_Base &v);
  void parse_and_visit_export(Parse_Visitor_Base &v,
                              const Parse_Export_Options &options);
  void parse_and_visit_named_exports(
      Parse_Visitor_Base &v,
      std::optional<Source_Code_Span> typescript_type_only_keyword,
      Vector<Token> *out_exported_bad_tokens);

  void found_default_export(Source_Code_Span default_keyword,
                            bool is_mergeable_interface);

  void parse_and_visit_variable_declaration_statement(
      Parse_Visitor_Base &v,
      bool is_top_level_typescript_definition_without_declare_or_export =
          false);
  struct Parse_Let_Bindings_Options {
    const Token &declaring_token;
    bool allow_in_operator = true;
    bool allow_const_without_initializer = false;
    bool is_in_for_initializer = false;
    bool is_top_level_typescript_definition_without_declare_or_export = false;

    // If set, refers to the TypeScript 'declare' keyword in 'declare var x;'
    // for example.
    std::optional<Source_Code_Span> declare_keyword;

    bool is_declare(Parser *parser) const {
      return this->declare_keyword.has_value() ||
             parser->options_.typescript_definition_file;
    }
  };
  // declaring_token is the const/let/var token.
  void parse_and_visit_let_bindings(Parse_Visitor_Base &v,
                                    const Parse_Let_Bindings_Options &);
  bool is_let_token_a_variable_reference(const Token &following_token,
                                         bool allow_declarations);

  struct Binding_Element_Info {
    Variable_Kind declaration_kind;
    std::optional<Source_Code_Span> declaring_token;
    Variable_Declaration_Flags flags;

    // Valid only if declaration_kind == Variable_Kind::_function_parameter.
    const Char8 *first_parameter_begin;

    bool is_destructuring = false;
    const Char8 *spread_operator_begin = nullptr;

    bool has_spread_operator() const {
      return this->spread_operator_begin != nullptr;
    }

    Source_Code_Span spread_operator_span() const {
      QLJS_ASSERT(this->has_spread_operator());
      return Source_Code_Span(
          this->spread_operator_begin,
          this->spread_operator_begin + this->spread_operator_length);
    }

    Binding_Element_Info with_flags(Variable_Declaration_Flags flags) const {
      Binding_Element_Info result = *this;
      result.flags = flags;
      return result;
    }

    Binding_Element_Info with_destructuring() const {
      Binding_Element_Info result = *this;
      result.is_destructuring = true;
      return result;
    }

    Binding_Element_Info with_spread(Source_Code_Span spread_operator) const {
      QLJS_ASSERT(spread_operator.end() - spread_operator.begin() ==
                  spread_operator_length);
      Binding_Element_Info result = *this;
      result.spread_operator_begin = spread_operator.begin();
      return result;
    }

    static constexpr int spread_operator_length =
        Expression::Spread::spread_operator_length;
  };
  void visit_binding_element(Expression *ast, Parse_Visitor_Base &v,
                             const Binding_Element_Info &);

  // TODO(#1069): This should be an enum class, but that causes GCC 8.3.0 to
  // fail compilation:
  //
  // > error: cannot convert 'quick_lint_js::Parser::Allow_Type_Annotations' to
  // > 'unsigned char:2' in initialization
  enum Allow_Type_Annotations : std::uint8_t {
    typescript_only,
    // Used inside 't' in 'c ? t : f':
    //
    //   c ? (param): Type => body : f;  // 'Type' is a type annotation.
    //   c ? x : y => body : f;          // 'y' is not a type annotation because
    //                                   // 'x' is not parenthesized. Invalid.
    //   c ? (x) : param => body;        // 'param' is not a type annotation
    //                                   // because no ':' follows 'body'.
    // Only parse annotations if they are valid return type annotations inside
    // the '?:'.
    typescript_only_if_legal_as_conditional_true_branch,
    always,
    never,
  };

  struct Precedence {
    bool binary_operators : 1 = true;
    bool math_or_logical_or_assignment : 1 = true;
    bool equals_assignment : 1 = true;
    bool commas : 1 = true;
    bool in_operator : 1 = true;
    Allow_Type_Annotations colon_type_annotation : 2 =
        Allow_Type_Annotations::typescript_only;

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

    // See
    // TypeScript_Type_Parse_Options::stop_parsing_type_at_newline_before_generic_arguments.
    bool
        stop_parsing_type_at_newline_before_generic_arguments_in_type_annotation : 1 =
            true;

    // Whether we are parsing the expression after 'extends' in a 'class'
    // expression or statement.
    //
    // * true:  'T<U> {}' is TypeScript generic followed by curlies;
    //   false: 'T<U> {}' is '<' and '>' binary expressions.
    //
    // * If in_async_function_ is false:
    //   * true:  'await {}' is a variable named 'await' followed by curlies;
    //     false: 'await {}' is an 'await' operator with an object literal.
    bool in_class_extends_clause : 1 = false;
  };

  // binary_expression_builder helps in the creation of a
  // Expression::Binary_Operator.
  //
  // Upon construction, binary_expression_builder stores a single expression*.
  // As a binary expression is parsed, other expression*-s are added to the
  // binary_expression_builder.
  class Binary_Expression_Builder {
   public:
    explicit Binary_Expression_Builder(Monotonic_Allocator *,
                                       Expression *first_child);

    Expression *last_expression() const;
    bool has_multiple_children() const;

    // Returns the given expression*.
    Expression *add_child(Source_Code_Span prior_operator_span, Expression *);

    void replace_last(Expression *new_last_child);

    void reset_after_build(Expression *new_first_child);

    Expression_Arena::Array_Ptr<Expression *> move_expressions(
        Expression_Arena &);
    Expression_Arena::Array_Ptr<Source_Code_Span> move_operator_spans(
        Expression_Arena &);

   private:
    Expression_Arena::Vector<Expression *> children_;
    Expression_Arena::Vector<Source_Code_Span> operator_spans_;
  };

  // Be sure to call builder.reset_after_build immediately after calling
  // build_expression.
  Expression *build_expression(Binary_Expression_Builder &builder);

  void parse_and_visit_expression(Parse_Visitor_Base &v, Precedence prec) {
    Monotonic_Allocator &alloc = *this->expressions_.allocator();
    auto rewind_guard = alloc.make_rewind_guard();

    Expression *ast = this->parse_expression(v, prec);
    {
      auto disable_guard = alloc.disable();
      this->visit_expression(ast, v, Variable_Context::rhs);
    }
  }

  Expression *parse_expression(Parse_Visitor_Base &, Precedence);
  Expression *parse_primary_expression(Parse_Visitor_Base &, Precedence);
  Expression *parse_async_expression(Parse_Visitor_Base &,
                                     const Token &async_token, Precedence);
  // Parses either:
  // * an expression starting with 'async', or
  // * an expression starting with 'await' where we determined that it is either
  //   a unary operator or a mistyped 'async'.
  Expression *parse_async_expression_only(Parse_Visitor_Base &,
                                          const Token &async_or_await_token,
                                          Precedence);
  Expression *parse_await_expression(Parse_Visitor_Base &,
                                     const Token &await_token, Precedence prec);
  Expression *parse_expression_remainder(Parse_Visitor_Base &, Expression *,
                                         Precedence);
  enum class Arrow_Function_Parameter_List_Validation {
    ok,
    error,
    // '=>' should be parsed as an operator like '>='.
    equal_greater_looks_like_operator,
  };
  Arrow_Function_Parameter_List_Validation
  validate_arrow_function_parameter_list(Expression *parameters_expression,
                                         Source_Code_Span arrow_span);
  Expression *parse_arrow_function_expression_remainder(
      Parse_Visitor_Base &, Buffering_Visitor *generic_parameter_visits,
      Expression *parameters_expression, Buffering_Visitor *return_type_visits,
      Precedence);
  Expression::Call *parse_call_expression_remainder(Parse_Visitor_Base &,
                                                    Expression *callee);
  Expression *parse_index_expression_remainder(Parse_Visitor_Base &,
                                               Expression *lhs);
  Expression_Arena::Vector<Expression *>
  parse_arrow_function_parameters_or_call_arguments(Parse_Visitor_Base &v);
  Expression *parse_arrow_function_body(
      Parse_Visitor_Base &, Function_Attributes,
      const Char8 *parameter_list_begin, Precedence,
      Expression_Arena::Array_Ptr<Expression *> &&parameters,
      Buffering_Visitor *return_type_visits);
  Expression *parse_arrow_function_body_no_scope(
      Parse_Visitor_Base &, Function_Attributes,
      const Char8 *parameter_list_begin, Precedence,
      Expression_Arena::Array_Ptr<Expression *> &&parameters,
      Buffering_Visitor *return_type_visits);
  Expression *parse_function_expression(Parse_Visitor_Base &,
                                        Function_Attributes,
                                        const Char8 *span_begin);
  Expression *parse_object_literal(Parse_Visitor_Base &);
  Expression *parse_class_expression(Parse_Visitor_Base &);
  Expression *parse_jsx_expression(Parse_Visitor_Base &);
  Expression *parse_jsx_or_typescript_generic_expression(Parse_Visitor_Base &,
                                                         Precedence);
  Expression *parse_jsx_element_or_fragment(Parse_Visitor_Base &);
  // tag is optional. If it is nullptr, parse a fragment. Otherwise, parse an
  // element.
  //
  // Precondition: previous token was '<' (for fragments) or an identifier (for
  //               elements).
  // Postcondition: current token is '>' or end_of_file.
  Expression *parse_jsx_element_or_fragment(Parse_Visitor_Base &,
                                            Identifier *tag,
                                            const Char8 *less_begin);
  void check_jsx_attribute(const Identifier &attribute_name);
  Expression *parse_typescript_generic_arrow_expression(Parse_Visitor_Base &,
                                                        Precedence);
  Expression *parse_typescript_angle_type_assertion_expression(
      Parse_Visitor_Base &, Precedence, bool is_invalid_due_to_jsx_ambiguity);
  Expression *parse_tagged_template(Parse_Visitor_Base &, Expression *tag);
  Expression *parse_untagged_template(Parse_Visitor_Base &);

  // If a generator '*' is parsed, modifies *attributes and returns the span of
  // the '*'.
  std::optional<Source_Code_Span> parse_generator_star(
      Function_Attributes *attributes);

  void check_assignment_lhs(Expression *);

  void check_body_after_label();

  Expression *maybe_wrap_erroneous_arrow_function(Expression *arrow_function,
                                                  Expression *lhs);

  void consume_semicolon_after_statement();
  template <class Missing_Semicolon_Diagnostic>
  void consume_semicolon();
  template <class Missing_Semicolon_Diagnostic>
  void consume_semicolon_or_comma();

  void error_on_pointless_nullish_coalescing_operator(
      Expression::Binary_Operator *);

  void check_lhs_for_null_potential(Expression *, Source_Code_Span op_span);

  const Token &peek() const { return this->lexer_.peek(); }
  void skip() { this->lexer_.skip(); }

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
  Parser_Transaction begin_transaction();

  // After calling commit_transaction, it's almost as if you never called
  // begin_transaction in the first place.
  //
  // commit_transaction does not restore the state of the parser or lexer when
  // begin_transaction was called.
  void commit_transaction(Parser_Transaction &&);

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
  void roll_back_transaction(Parser_Transaction &&);

  // Speculatively parse something. If parsing turned out to be a bad idea, roll
  // back and do something else.
  //
  // try_func is always called. It is given a Parser_Transaction&. It should not
  // call commit_transaction or roll_back_transaction with the given
  // Parser_Transaction. It should return a boolean.
  //
  // catch_func is called if try_func returns false.
  //
  // When try_func and catch_func are called, they start with the same lexer
  // state.
  template <class Try_Func, class Catch_Func>
  void try_parse(Try_Func &&try_func, Catch_Func &&catch_func) {
    Parser_Transaction transaction = this->begin_transaction();
    bool should_commit = std::move(try_func)(transaction);
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
  template <class New_Expression, class... Args>
  Expression *make_expression(Args &&... args) {
    return this->expressions_.make_expression<New_Expression>(
        std::forward<Args>(args)...);
  }

 public:
  class Function_Guard {
   public:
    explicit Function_Guard(Parser *, bool was_in_top_level,
                            bool was_in_async_function,
                            bool was_in_generator_function,
                            bool was_in_loop_statement,
                            bool was_in_switch_statement);

    Function_Guard(const Function_Guard &) = delete;
    Function_Guard &operator=(const Function_Guard &) = delete;

    ~Function_Guard();

   private:
    Parser *parser_;
    bool was_in_top_level_;
    bool was_in_async_function_;
    bool was_in_generator_function_;
    bool was_in_loop_statement_;
    bool was_in_switch_statement_;
  };

 private:
  template <bool Parser::*Member>
  class Bool_Guard {
   public:
    explicit Bool_Guard(Parser *p, bool old_value)
        : parser_(p), old_value_(old_value) {}

    Bool_Guard(const Bool_Guard &) = delete;
    Bool_Guard &operator=(const Bool_Guard &) = delete;

    ~Bool_Guard() { this->parser_->*Member = this->old_value_; }

   private:
    Parser *parser_;
    bool old_value_;
  };

 public:
  class Depth_Guard {
   public:
    explicit Depth_Guard(Parser *p);

    Depth_Guard(const Depth_Guard &) = delete;
    Depth_Guard &operator=(const Depth_Guard &) = delete;

    ~Depth_Guard();

   private:
    Parser *parser_;
    int old_depth_;
  };

 private:
  struct Parse_Expression_Cache_Key {
    const Char8 *begin;
    bool in_top_level;
    bool in_async_function;
    bool in_generator_function;
    bool in_loop_statement;
    bool in_switch_statement;
    bool in_class;

    bool operator==(const Parse_Expression_Cache_Key &rhs) const;
    bool operator!=(const Parse_Expression_Cache_Key &rhs) const;

    struct Hash {
      std::size_t operator()(const Parse_Expression_Cache_Key &) const;
    };
  };

  Parse_Expression_Cache_Key parse_expression_cache_key_for_current_state()
      const;

  quick_lint_js::Lexer lexer_;
  Diag_Reporter *diag_reporter_;
  Parser_Options options_;
  Expression_Arena expressions_;

  // Memory used for temporary memory allocations (e.g. vectors on the stack).
  Monotonic_Allocator temporary_memory_{"parser::temporary_memory_"};

  // Memory used for strings in diagnostic messages.
  Monotonic_Allocator diagnostic_memory_{"parser::diagnostic_memory_"};

  // Memory used for TypeScript type expressions.
  // TODO(strager): Rewind periodically (e.g. after parsing a function body).
  Monotonic_Allocator type_expression_memory_{
      "parser::type_expression_memory_"};

  // These are stored in a stack here (rather than on the C++ stack via local
  // variables) so that memory can be released in case we call setjmp.
  Buffering_Visitor_Stack buffering_visitor_stack_;

  bool in_top_level_ = true;
  bool in_async_function_ = false;
  bool in_generator_function_ = false;
  bool in_loop_statement_ = false;
  bool in_switch_statement_ = false;
  bool in_class_ = false;

  class TypeScript_Namespace_Or_Module_Guard {
   public:
    explicit TypeScript_Namespace_Or_Module_Guard(
        Parser *,
        std::optional<Source_Code_Span> old_in_typescript_namespace_or_module,
        bool old_in_typescript_module, bool old_in_loop_statement,
        bool old_in_switch_statement);

    TypeScript_Namespace_Or_Module_Guard(
        const TypeScript_Namespace_Or_Module_Guard &) = delete;
    TypeScript_Namespace_Or_Module_Guard &operator=(
        const TypeScript_Namespace_Or_Module_Guard &) = delete;

    ~TypeScript_Namespace_Or_Module_Guard();

   private:
    Parser *parser_;
    std::optional<Source_Code_Span> old_in_typescript_namespace_or_module_;
    bool old_in_typescript_module_;
    bool old_in_loop_statement_;
    bool old_in_switch_statement_;
  };
  // Sets in_typescript_namespace_or_module_ and in_typescript_module_.
  [[nodiscard]] TypeScript_Namespace_Or_Module_Guard
  enter_typescript_namespace_or_module(
      Source_Code_Span namespace_or_module_keyword_span, bool is_module);

  // If present, we are inside the body of a TypeScript namespace (declared with
  // 'namespace' or 'module') or a TypeScript module. This variable then refers
  // to the inner-most 'namespace' or 'module' token.
  std::optional<Source_Code_Span> in_typescript_namespace_or_module_ =
      std::nullopt;

  // If true, we are inside the body of a TypeScript module (declared with
  // 'module "modulename"'). in_typescript_namespace_or_module_ will be present.
  //
  // If false, we might be inside the body of a TypeScript namespace (declared
  // with 'module'); see in_typescript_namespace_or_module_ for that
  // information.
  bool in_typescript_module_ = false;

  bool in_typescript_only_construct_ = false;

  // Set to true when a statement is found which would cause the TypeScript
  // compiler to generate JavaScript code for a namespace.
  //
  // This might be true for 'declare namespace', in which case this flag applies
  // to the containing namespace, if any. For example:
  //
  //   namespace outer {
  //     declare namespace inner {
  //       // The following statement sets
  //       // is_current_typescript_namespace_non_empty_=true for 'outer'.
  //       export class C {}
  //     }
  //   }
  //
  // NOTE[non-empty-namespace]: The rules for what makes a namespace empty or
  // not are not obvious. See tests.
  //
  // See also Variable_Declaration_Flags::non_empty_namespace.
  bool is_current_typescript_namespace_non_empty_ = false;

  // When parsing TypeScript 'infer', store visit_variable_declaration calls
  // here. If typescript_infer_declaration_buffer_ is null, then we are in a
  // context where 'infer' is disallowed.
  Buffering_Visitor *typescript_infer_declaration_buffer_ = nullptr;

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
  Hash_Map<Parse_Expression_Cache_Key, bool, Parse_Expression_Cache_Key::Hash>
      await_is_identifier_cache_;

  enum class Fatal_Parse_Error_Kind {
    depth_limit_exceeded,
    unexpected_token,
  };
  struct Fatal_Parse_Error {
    Source_Code_Span error_span = Source_Code_Span::unit(nullptr);
    Fatal_Parse_Error_Kind kind;
  };
  Try_Catch_Stack<Fatal_Parse_Error> fatal_parse_error_stack_;

 public:
  using Loop_Guard = Bool_Guard<&Parser::in_loop_statement_>;
  using Switch_Guard = Bool_Guard<&Parser::in_switch_statement_>;
  using Class_Guard = Bool_Guard<&Parser::in_class_>;

  using TypeScript_Only_Construct_Guard =
      Bool_Guard<&Parser::in_typescript_only_construct_>;

  int depth_ = 0;

  // FIXME(#735): We should make this higher.
  static constexpr const int stack_limit = 100;

  // For testing and internal use only.
  [[nodiscard]] Loop_Guard enter_loop();
  [[nodiscard]] Class_Guard enter_class();

 private:
  [[nodiscard]] TypeScript_Only_Construct_Guard
  enter_typescript_only_construct();
  [[nodiscard]] Switch_Guard enter_switch();

  void parse_end_of_expression_statement();
  void parse_and_visit_return_statement(
      Parse_Visitor_Base &v, bool possibly_followed_by_another_statement);
  void parse_and_visit_throw_statement(Parse_Visitor_Base &v);
  void parse_and_visit_break_or_continue();

  enum class Parse_Possible_Label_Result {
    parsed_as_label,
    parsed_not_as_a_label,
  };

  Parse_Possible_Label_Result
  parse_and_visit_typescript_interface_or_namespace_or_type_statement(
      Parse_Visitor_Base &v);

  enum class Parse_Possible_Declare_Result {
    declare_is_expression_or_loop_label,
    parsed,
  };

  Parse_Possible_Declare_Result parse_and_visit_possible_declare_statement(
      Parse_Visitor_Base &v);

  // Precondition: declare_context.declare_namespace_declare_keyword.has_value()
  //               || declare_context.direct_declare_keyword.has_value()
  // Precondition: is_declare_statement_start_token(this->peek().type)
  void parse_and_visit_declare_statement(
      Parse_Visitor_Base &v, const TypeScript_Declare_Context &declare_context);

  void parse_and_visit_declare_global(
      Parse_Visitor_Base &v, const TypeScript_Declare_Context &declare_context);

  bool is_declare_statement_start_token(Token_Type);
};

template <class Expected_Parentheses_Error, class Expected_Parenthesis_Error,
          bool check_for_sketchy_conditions, bool check_for_comma_operator>
void Parser::parse_and_visit_parenthesized_expression(
    Parse_Visitor_Base &v, Source_Code_Span token_span) {
  bool have_expression_left_paren = this->peek().type == Token_Type::left_paren;
  if (have_expression_left_paren) {
    Source_Code_Span left_paren_span = this->peek().span();
    this->skip();

    if (this->peek().type == Token_Type::right_paren) {
      this->diag_reporter_->report(Diag_Empty_Paren_After_Control_Statement{
          .token = token_span,
          .expected_expression =
              Source_Code_Span::unit(left_paren_span.end())});
    }
  }

  const Char8 *expression_begin = this->peek().begin;

  Expression *ast = this->parse_expression(v, {.trailing_identifiers = true});
  this->visit_expression(ast, v, Variable_Context::rhs);

  if constexpr (check_for_sketchy_conditions) {
    this->error_on_sketchy_condition(ast);
  }

  if constexpr (check_for_comma_operator) {
    this->warn_on_comma_operator_in_conditional_statement(ast);
  }

  const Char8 *expression_end = this->lexer_.end_of_previous_token();
  bool have_expression_right_paren =
      this->peek().type == Token_Type::right_paren;
  if (have_expression_right_paren) {
    this->skip();
  }

  if (!have_expression_left_paren && !have_expression_right_paren) {
    this->diag_reporter_->report(Expected_Parentheses_Error{
        Source_Code_Span(expression_begin, expression_end)});
  } else if (!have_expression_right_paren) {
    this->diag_reporter_->report(Expected_Parenthesis_Error{
        .where = Source_Code_Span::unit(expression_end),
        .token = ')',
    });
  } else if (!have_expression_left_paren) {
    this->diag_reporter_->report(Expected_Parenthesis_Error{
        .where = Source_Code_Span::unit(expression_begin),
        .token = '(',
    });
  }
}

extern template void
Parser::consume_semicolon<Diag_Missing_Semicolon_After_Abstract_Method>();
extern template void
Parser::consume_semicolon<Diag_Missing_Semicolon_After_Declare_Class_Method>();
extern template void
Parser::consume_semicolon<Diag_Missing_Semicolon_After_Field>();
extern template void
Parser::consume_semicolon<Diag_Missing_Semicolon_After_Statement>();
extern template void Parser::consume_semicolon<
    Diag_Missing_Semicolon_After_TypeScript_Method_Overload_Signature>();

extern template void
Parser::consume_semicolon_or_comma<Diag_Missing_Semicolon_After_Field>();
extern template void Parser::consume_semicolon_or_comma<
    Diag_Missing_Semicolon_After_Index_Signature>();
extern template void Parser::consume_semicolon_or_comma<
    Diag_Missing_Semicolon_After_Interface_Method>();
extern template void Parser::consume_semicolon_or_comma<
    Diag_Missing_Separator_Between_Object_Type_Entries>();
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
