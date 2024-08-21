// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/hash-set.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/buffering-visitor.h>
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
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/enum.h>
#include <utility>

// For Parser::binding_element_info.
QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")

namespace quick_lint_js {
bool Parser::parse_and_visit_module_catching_fatal_parse_errors(
    Parse_Visitor_Base &v) {
  return this->catch_fatal_parse_errors(
      [this, &v] { this->parse_and_visit_module(v); });
}

void Parser::parse_and_visit_module(Parse_Visitor_Base &v) {
  QLJS_ASSERT(
      !this->first_export_default_statement_default_keyword_.has_value());
  bool done = false;
  Parse_Statement_Options statement_options = {
      .possibly_followed_by_another_statement = true,
      .top_level_typescript_definition =
          this->options_.typescript_definition_file,
      .require_declaration = this->options_.typescript_definition_file,
  };
  while (!done) {
    bool parsed_statement =
        this->parse_and_visit_statement(v, statement_options);
    if (!parsed_statement) {
      switch (this->peek().type) {
      case Token_Type::end_of_file:
        done = true;
        break;

      case Token_Type::right_curly:
        this->diags_.add(Diag_Unmatched_Right_Curly{
            .right_curly = this->peek().span(),
        });
        this->skip();
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
  }
  this->check_all_jsx_attributes();
  v.visit_end_of_module();
}

bool Parser::parse_and_visit_statement(Parse_Visitor_Base &v,
                                       Parse_Statement_Options options) {
  Depth_Guard d_guard(this);

  auto on_non_declaring_statement = [&]() -> void {
    if (options.require_declaration) {
      if (this->options_.typescript_definition_file) {
        this->diags_.add(Diag_DTS_Non_Declaring_Statement{
            .first_statement_token = this->peek().span(),
        });
      } else {
        QLJS_ASSERT(options.declare_keyword.has_value());
        this->diags_.add(Diag_Declare_Namespace_Cannot_Contain_Statement{
            .first_statement_token = this->peek().span(),
            .declare_keyword = *options.declare_keyword,
        });
      }
    }
  };

parse_statement:
  switch (this->peek().type) {
    // export class C {}
    // export {taco} from "taco-stand";
  case Token_Type::kw_export:
    // is_current_typescript_namespace_non_empty_ is possibly set by
    // parse_and_visit_export.
    this->parse_and_visit_export(v);
    break;

  case Token_Type::semicolon:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->skip();
    break;

    // function f() {}
  case Token_Type::kw_function:
    this->is_current_typescript_namespace_non_empty_ = true;
    if (options.top_level_typescript_definition) {
      this->diags_.add(Diag_DTS_Missing_Declare_Or_Export{
          .expected = Source_Code_Span::unit(this->peek().begin),
          .declaring_token = this->peek().span(),
      });
    }
    this->parse_and_visit_function_declaration(
        v, Function_Declaration_Options{
               .attributes = Function_Attributes::normal,
               .begin = this->peek().begin,
               .require_name = Name_Requirement::required_for_statement,
               .async_keyword = std::nullopt,
               .declare_keyword = std::nullopt,
           });
    break;

  // var x = 42;
  // const enum E {}  // TypeScript only.
  case Token_Type::kw_const:
  case Token_Type::kw_var:
    // is_current_typescript_namespace_non_empty_ is possibly set by
    // parse_and_visit_variable_declaration_statement.
    this->parse_and_visit_variable_declaration_statement(
        v,
        /*is_top_level_typescript_definition_without_declare_or_export=*/options
            .top_level_typescript_definition);
    break;

    // let x = 42;
    // let();
    // let: while (true) {}
  case Token_Type::kw_let: {
    this->is_current_typescript_namespace_non_empty_ = true;
    Token let_token = this->peek();
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    if (this->peek().type == Token_Type::colon) {
      // Labelled statement.
      this->lexer_.commit_transaction(std::move(transaction));
      this->skip();
      this->check_body_after_label();
      goto parse_statement;
    } else if (this->is_let_token_a_variable_reference(
                   this->peek(),
                   /*allow_declarations=*/options.allow_let_declaration)) {
      // Expression.
      this->lexer_.roll_back_transaction(std::move(transaction));
      Expression *ast =
          this->parse_expression(v, Precedence{.in_operator = true});
      this->visit_expression(ast, v, Variable_Context::rhs);

      // let ({x} = y);  // Confusing, if 'let' is a function
      if (ast->kind() == Expression_Kind::Call) {
        Expression::Call *call = expression_cast<Expression::Call *>(ast);
        if (call->child_count() == 2 &&
            call->child_0()->kind() == Expression_Kind::Variable &&
            expression_cast<Expression::Variable *>(call->child_0())->type_ ==
                Token_Type::kw_let &&
            call->child_1()->kind() == Expression_Kind::Assignment &&
            call->child_1()->child_0()->kind() == Expression_Kind::Object) {
          this->diags_.add(
              Diag_Confusing_Let_Call{.let_function_call = let_token.span()});
        }
      }

      this->parse_end_of_expression_statement();
    } else {
      // Variable declaration.
      this->lexer_.commit_transaction(std::move(transaction));
      this->parse_and_visit_let_bindings(
          v, Parse_Let_Bindings_Options{
                 .declaring_token = let_token,
                 .is_top_level_typescript_definition_without_declare_or_export =
                     options.top_level_typescript_definition,
             });
      this->consume_semicolon_after_statement();
    }
    break;
  }

    // abstract class C {}  // TypeScript only.
    // abstract = 42;
  case Token_Type::kw_abstract: {
    this->is_current_typescript_namespace_non_empty_ = true;
    Source_Code_Span abstract_token = this->peek().span();
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    switch (this->peek().type) {
    // abstract class C {}
    //
    // abstract  // ASI
    // class C {}
    case Token_Type::kw_class:
      if (this->peek().has_leading_newline) {
        // abstract  // ASI
        // class C {}
        this->lexer_.roll_back_transaction(std::move(transaction));
        goto parse_loop_label_or_expression_starting_with_identifier;
      }

      // abstract class C {}
      this->lexer_.commit_transaction(std::move(transaction));
      this->parse_and_visit_class(
          v, Parse_Class_Options{
                 .require_name = Name_Requirement::required_for_statement,
                 .abstract_keyword_span = abstract_token,
                 .is_top_level_typescript_definition_without_declare_or_export =
                     options.top_level_typescript_definition,
             });
      break;

    // abstract:  // Label.
    // abstract();
    case Token_Type::colon:
    default:
      this->lexer_.roll_back_transaction(std::move(transaction));
      goto parse_loop_label_or_expression_starting_with_identifier;
    }
    break;
  }

  // declare class C {}  // TypeScript only.
  // declare enum E {}  // TypeScript only.
  // declare = 42;
  case Token_Type::kw_declare:
    // is_current_typescript_namespace_non_empty_ is possibly set by
    // parse_and_visit_possible_declare_statement.
    switch (this->parse_and_visit_possible_declare_statement(v)) {
    case Parse_Possible_Declare_Result::declare_is_expression_or_loop_label:
      goto parse_loop_label_or_expression_starting_with_identifier;
    case Parse_Possible_Declare_Result::parsed:
      break;
    }
    break;

    // async function f() {}
    // async = 42;
  case Token_Type::kw_async: {
    this->is_current_typescript_namespace_non_empty_ = true;
    Token async_token = this->peek();
    this->skip();
    switch (this->peek().type) {
      // async function f() {}
    case Token_Type::kw_function:
      if (this->peek().has_leading_newline) {
        // async  // ASI
        // function f() {}
        v.visit_variable_use(async_token.identifier_name());
        break;
      }

      this->parse_and_visit_function_declaration(
          v, Function_Declaration_Options{
                 .attributes = Function_Attributes::async,
                 .begin = async_token.begin,
                 .require_name = Name_Requirement::required_for_statement,
                 .async_keyword = async_token.span(),
                 .declare_keyword = std::nullopt,
             });
      break;

      // async (x, y) => expressionOrStatement
      // async x => expressionOrStatement
      // async => expressionOrStatement
      // async += 42;
    QLJS_CASE_BINARY_ONLY_OPERATOR:
    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
    QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
    QLJS_CASE_CONTEXTUAL_KEYWORD:
    case Token_Type::comma:
    case Token_Type::complete_template:
    case Token_Type::dot:
    case Token_Type::end_of_file:
    case Token_Type::equal:
    case Token_Type::equal_greater:
    case Token_Type::identifier:
    case Token_Type::incomplete_template:
    case Token_Type::kw_in:
    case Token_Type::kw_yield:
    case Token_Type::left_paren:
    case Token_Type::left_square:
    case Token_Type::less:
    case Token_Type::minus:
    case Token_Type::minus_minus:
    case Token_Type::plus:
    case Token_Type::plus_plus:
    case Token_Type::question:
    case Token_Type::right_curly:
    case Token_Type::semicolon:
    case Token_Type::slash: {
      Expression *ast =
          this->parse_async_expression(v, async_token, Precedence{});
      this->visit_expression(ast, v, Variable_Context::rhs);
      this->consume_semicolon_after_statement();
      break;
    }

      // Labelled statement.
    case Token_Type::colon:
      this->skip();
      this->check_body_after_label();
      goto parse_statement;
      // "async export function f()" is not valid. It should be "export async
      // function f()"
    case Token_Type::kw_export: {
      if (this->peek().has_leading_newline) {
        // async  // ASI
        // export function f() {}
        v.visit_variable_use(async_token.identifier_name());
        break;
      }
      this->diags_.add(
          Diag_Async_Export_Function{.async_export = Source_Code_Span(
                                         async_token.begin, this->peek().end)});
      this->skip();
      this->parse_and_visit_function_declaration(
          v, Function_Declaration_Options{
                 .attributes = Function_Attributes::async,
                 .begin = async_token.begin,
                 .require_name = Name_Requirement::required_for_statement,
                 .async_keyword = async_token.span(),
                 .declare_keyword = std::nullopt,
             });
      break;
    }
    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    break;
  }

    // import {bananas} from "Thailand";
    // import(url).then(loaded);
  case Token_Type::kw_import:
    // is_current_typescript_namespace_non_empty_ is possibly set by
    // parse_and_visit_import.
    this->parse_and_visit_import(v);
    break;

    // this.explode();
    // [1, 2, 3].forEach(x => console.log(x));
    // ^ x  // invalid expression
  QLJS_CASE_BINARY_ONLY_OPERATOR:
  QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR_EXCEPT_SLASH_EQUAL:
  QLJS_CASE_CONDITIONAL_ASSIGNMENT_OPERATOR:
  case Token_Type::bang:
  case Token_Type::comma:
  case Token_Type::complete_template:
  case Token_Type::dot:
  case Token_Type::equal:
  case Token_Type::equal_greater:
  case Token_Type::incomplete_template:
  case Token_Type::kw_delete:
  case Token_Type::kw_false:
  case Token_Type::kw_in:
  case Token_Type::kw_new:
  case Token_Type::kw_null:
  case Token_Type::kw_super:
  case Token_Type::kw_this:
  case Token_Type::kw_true:
  case Token_Type::kw_typeof:
  case Token_Type::kw_void:
  case Token_Type::left_paren:
  case Token_Type::left_square:
  case Token_Type::less:
  case Token_Type::minus:
  case Token_Type::minus_minus:
  case Token_Type::number:
  case Token_Type::plus:
  case Token_Type::plus_plus:
  case Token_Type::private_identifier:
  case Token_Type::right_paren:
  case Token_Type::slash:
  case Token_Type::slash_equal:
  case Token_Type::string:
  case Token_Type::tilde: {
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    if (this->peek().type == Token_Type::star) {
      // * 42; // Invalid (missing operand).
      // *function f() {} // Invalid (misplaced '*').
      Token star_token = this->peek();
      std::optional<Function_Attributes> attributes =
          this->try_parse_function_with_leading_star();
      if (attributes.has_value()) {
        this->parse_and_visit_function_declaration(
            v, Function_Declaration_Options{
                   .attributes = attributes.value(),
                   .begin = star_token.begin,
                   .require_name = Name_Requirement::required_for_statement,
                   .async_keyword = std::nullopt,
                   .declare_keyword = std::nullopt,
               });
        break;
      }
    }
    this->parse_and_visit_expression(v);
    this->parse_end_of_expression_statement();
    break;
  }

    // await settings.save();
    // await = value;
    // await: for(;;);
  case Token_Type::kw_await: {
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    Token await_token = this->peek();
    this->skip();
    if (this->peek().type == Token_Type::colon) {
      // Labelled statement.
      if (this->in_async_function_) {
        this->diags_.add(Diag_Label_Named_Await_Not_Allowed_In_Async_Function{
            .await = await_token.span(), .colon = this->peek().span()});
      }
      this->skip();
      this->check_body_after_label();
      goto parse_statement;
    } else {
      Expression *ast =
          this->parse_await_expression(v, await_token, Precedence{});
      ast = this->parse_expression_remainder(v, ast, Precedence{});
      this->visit_expression(ast, v, Variable_Context::rhs);
      this->parse_end_of_expression_statement();
    }
    break;
  }

    // yield value;
    // yield = value;
    // yield: for(;;);
  case Token_Type::kw_yield:
    if (this->in_generator_function_) {
      on_non_declaring_statement();
      this->parse_and_visit_expression(v);
      this->parse_end_of_expression_statement();
      break;
    } else {
      // is_current_typescript_namespace_non_empty_ is possibly set by
      // parse_loop_label_or_expression_starting_with_identifier.
      goto parse_loop_label_or_expression_starting_with_identifier;
    }

    // console.log("hello");
    // label: for(;;);
  parse_loop_label_or_expression_starting_with_identifier:
  case Token_Type::identifier:
  case Token_Type::kw_accessor:
  case Token_Type::kw_any:
  case Token_Type::kw_as:
  case Token_Type::kw_assert:
  case Token_Type::kw_asserts:
  case Token_Type::kw_bigint:
  case Token_Type::kw_boolean:
  case Token_Type::kw_constructor:
  case Token_Type::kw_from:
  case Token_Type::kw_get:
  case Token_Type::kw_infer:
  case Token_Type::kw_intrinsic:
  case Token_Type::kw_is:
  case Token_Type::kw_keyof:
  case Token_Type::kw_never:
  case Token_Type::kw_number:
  case Token_Type::kw_object:
  case Token_Type::kw_of:
  case Token_Type::kw_out:
  case Token_Type::kw_override:
  case Token_Type::kw_readonly:
  case Token_Type::kw_require:
  case Token_Type::kw_satisfies:
  case Token_Type::kw_set:
  case Token_Type::kw_static:
  case Token_Type::kw_string:
  case Token_Type::kw_symbol:
  case Token_Type::kw_undefined:
  case Token_Type::kw_unique:
  case Token_Type::kw_unknown: {
    on_non_declaring_statement();
    Token_Type ident_token_type = this->peek().type;
    Identifier ident = this->peek().identifier_name();
    this->skip();
    switch (this->peek().type) {
      // Labelled statement.
    case Token_Type::colon:
      this->is_current_typescript_namespace_non_empty_ = true;
      this->skip();
      this->check_body_after_label();
      goto parse_statement;

      // Expression statement.
    default:
      this->is_current_typescript_namespace_non_empty_ = true;
      Expression *ast =
          this->make_expression<Expression::Variable>(ident, ident_token_type);
      ast = this->parse_expression_remainder(v, ast, Precedence{});
      this->visit_expression(ast, v, Variable_Context::rhs);
      this->parse_end_of_expression_statement();
      break;
    }
    break;
  }

    // \u{69}\u{66} // 'if', but escaped.
  case Token_Type::reserved_keyword_with_escape_sequence:
    this->lexer_.peek().add_diags_for_escape_sequences_in_keyword(
        this->diag_reporter_->diags());
    // is_current_typescript_namespace_non_empty_ is possibly set by
    // parse_loop_label_or_expression_starting_with_identifier.
    goto parse_loop_label_or_expression_starting_with_identifier;

  // type++;
  // type T = number;  // TypeScript only.
  // namespace.foo();
  // namespace ns {}   // TypeScript only.
  // interface * x;
  // interface I {}   // TypeScript only.
  case Token_Type::kw_interface:
  case Token_Type::kw_module:
  case Token_Type::kw_namespace:
  case Token_Type::kw_type:
    switch (
        this->parse_and_visit_typescript_interface_or_namespace_or_type_statement(
            v)) {
    case Parse_Possible_Label_Result::parsed_not_as_a_label:
      // is_current_typescript_namespace_non_empty_ was possibly set by
      // parse_and_visit_typescript_interface_or_namespace_or_type_statement.
      break;
    case Parse_Possible_Label_Result::parsed_as_label:
      this->is_current_typescript_namespace_non_empty_ = true;
      goto parse_statement;
    }
    break;

  // global();
  // global:
  // global {}  // Invalid.
  case Token_Type::kw_global: {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    bool is_typescript_global_block =
        this->peek().type == Token_Type::left_curly &&
        !this->peek().has_leading_newline;
    this->lexer_.roll_back_transaction(std::move(transaction));
    if (is_typescript_global_block) {
      // global {}  // Invalid.
      Source_Code_Span declare_keyword =
          Source_Code_Span::unit(this->peek().begin);
      this->diags_.add(Diag_TypeScript_Global_Block_Must_Be_Declare{
          .global_keyword = this->peek().span(),
          .expected_declare_keyword = declare_keyword,
      });
      this->parse_and_visit_declare_global(
          v, TypeScript_Declare_Context{
                 .direct_declare_keyword = declare_keyword,
             });
    } else {
      goto parse_loop_label_or_expression_starting_with_identifier;
    }
    break;
  }

  case Token_Type::kw_implements:
  case Token_Type::kw_package:
  case Token_Type::kw_private:
  case Token_Type::kw_protected:
  case Token_Type::kw_public:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    goto parse_loop_label_or_expression_starting_with_identifier;

    // class C {}
  case Token_Type::kw_class:
    this->is_current_typescript_namespace_non_empty_ = true;
    this->parse_and_visit_class(
        v, Parse_Class_Options{
               .require_name = Name_Requirement::required_for_statement,
               .abstract_keyword_span = std::nullopt,
               .is_top_level_typescript_definition_without_declare_or_export =
                   options.top_level_typescript_definition,
           });
    break;

    // switch (x) { default: ; }
  case Token_Type::kw_switch:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->parse_and_visit_switch(v);
    break;

    // return;
    // return 42;
  case Token_Type::kw_return:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->parse_and_visit_return_statement(
        v, /*possibly_followed_by_another_statement=*/options
               .possibly_followed_by_another_statement);
    break;

    // throw fit;
  case Token_Type::kw_throw:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->parse_and_visit_throw_statement(v);
    break;

    // try { hard(); } catch (exhaustion) {}
  case Token_Type::kw_try:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->parse_and_visit_try_maybe_catch_maybe_finally(v);
    break;

    // catch (e) { }  // Invalid.
  case Token_Type::kw_catch: {
    this->is_current_typescript_namespace_non_empty_ = true;
    this->diags_.add(Diag_Catch_Without_Try{
        .catch_token = this->peek().span(),
    });
    bool parsed_catch = this->parse_and_visit_catch_or_finally_or_both(v);
    QLJS_ASSERT(parsed_catch);
    break;
  }

    // finally { }  // Invalid.
  case Token_Type::kw_finally: {
    this->is_current_typescript_namespace_non_empty_ = true;
    this->diags_.add(Diag_Finally_Without_Try{
        .finally_token = this->peek().span(),
    });
    bool parsed_finally = this->parse_and_visit_catch_or_finally_or_both(v);
    QLJS_ASSERT(parsed_finally);
    break;
  }

    // do { } while (can);
  case Token_Type::kw_do:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->parse_and_visit_do_while(v);
    break;

    // for (let i = 0; i < length; ++i) {}
    // for (let x of xs) {}
  case Token_Type::kw_for:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->parse_and_visit_for(v);
    break;

    // while (cond) {}
  case Token_Type::kw_while:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->parse_and_visit_while(v);
    break;

    // with (o) { eek(); }
  case Token_Type::kw_with:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->parse_and_visit_with(v);
    break;

    // if (cond) { yay; } else { nay; }
  case Token_Type::kw_if:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->parse_and_visit_if(v);
    break;

    // else { nay; } // Invalid.
  case Token_Type::kw_else: {
    this->is_current_typescript_namespace_non_empty_ = true;
    this->diags_.add(Diag_Else_Has_No_If{
        .else_token = this->peek().span(),
    });
    this->skip();

    bool parsed_else_body = this->parse_and_visit_statement(v);
    if (!parsed_else_body) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    break;
  }

    // break;
    // continue label;
  case Token_Type::kw_break:
  case Token_Type::kw_continue:
    this->is_current_typescript_namespace_non_empty_ = true;
    this->parse_and_visit_break_or_continue();
    break;

    // debugger;
  case Token_Type::kw_debugger:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    this->skip();
    this->consume_semicolon_after_statement();
    break;

    // enum E { a, b, c }  // TypeScript.
  case Token_Type::kw_enum:
    // is_current_typescript_namespace_non_empty_ is set by
    // parse_and_visit_typescript_enum.
    if (options.top_level_typescript_definition) {
      this->diags_.add(Diag_DTS_Missing_Declare_Or_Export{
          .expected = Source_Code_Span::unit(this->peek().begin),
          .declaring_token = this->peek().span(),
      });
    }
    this->parse_and_visit_typescript_enum(v, Enum_Kind::normal);
    break;

    // { statement; statement; }
  case Token_Type::left_curly:
    this->is_current_typescript_namespace_non_empty_ = true;
    on_non_declaring_statement();
    v.visit_enter_block_scope();
    this->parse_and_visit_statement_block_no_scope(v);
    v.visit_exit_block_scope();
    break;

  case Token_Type::at:
    // TODO(#690): Are decorators allowed inside declare namespaces or .d.ts
    // files? Do they make namespaces non-empty?
    this->parse_and_visit_decorator_statement(v);
    break;

    // case 3:  // Invalid.
  case Token_Type::kw_case:
    this->is_current_typescript_namespace_non_empty_ = true;
    this->diags_.add(Diag_Unexpected_Case_Outside_Switch_Statement{
        .case_token = this->peek().span(),
    });
    this->skip();
    this->parse_and_visit_expression(
        v, Precedence{
               .colon_type_annotation = Allow_Type_Annotations::never,
           });
    if (this->peek().type == Token_Type::colon) {
      this->skip();
    }
    break;

    // default:  // Invalid.
  case Token_Type::kw_default:
    // Do not set is_current_typescript_namespace_non_empty_.
    this->diags_.add(Diag_Unexpected_Default_Outside_Switch_Statement{
        .default_token = this->peek().span(),
    });
    this->skip();
    if (this->peek().type == Token_Type::colon) {
      this->skip();
    }
    break;

  case Token_Type::colon:
  case Token_Type::kw_extends:
  case Token_Type::question:
    // Do not set is_current_typescript_namespace_non_empty_.
    this->diags_.add(Diag_Unexpected_Token{
        .token = this->peek().span(),
    });
    this->skip();
    break;

  case Token_Type::end_of_file:
  case Token_Type::right_curly:
    // Do not set is_current_typescript_namespace_non_empty_.
    return false;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  return true;
}

Parser::Parse_Possible_Label_Result
Parser::parse_and_visit_typescript_interface_or_namespace_or_type_statement(
    Parse_Visitor_Base &v) {
  Token initial_keyword = this->peek();
  Lexer_Transaction transaction = this->lexer_.begin_transaction();
  this->skip();
  switch (this->peek().type) {
  // type:  // Labelled statement.
  case Token_Type::colon:
    this->lexer_.commit_transaction(std::move(transaction));
    this->skip();
    this->check_body_after_label();
    return Parse_Possible_Label_Result::parsed_as_label;

  // type T = number;  // TypeScript only.
  //
  // namespace 'my namespace name' {}  // Invalid.
  //
  // type  // ASI
  // f();
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case Token_Type::kw_await:
  case Token_Type::identifier:
  case Token_Type::string:
    if (this->peek().has_leading_newline) {
      bool is_expression = true;
      if (initial_keyword.type == Token_Type::kw_interface) {
        Parser_Transaction inner_transaction = this->begin_transaction();
        this->skip();
        bool has_generic_parameters = false;
        if (this->options_.typescript &&
            this->peek().type == Token_Type::less) {
          // interface
          //   I<T> {}  // Invalid TypeScript.
          this->parse_and_visit_typescript_generic_parameters(
              Null_Visitor::instance);
          has_generic_parameters = true;
        }
        if (this->peek().type == Token_Type::left_curly &&
            (!this->peek().has_leading_newline || has_generic_parameters)) {
          // interface
          //   I {}     // Invalid.
          // Treat 'interface' as a keyword.
          is_expression = false;
        }
        this->roll_back_transaction(std::move(inner_transaction));

        if (!is_expression) {
          this->diags_.add(Diag_Newline_Not_Allowed_After_Interface_Keyword{
              .interface_keyword = initial_keyword.span(),
          });
        }
      }
      if (initial_keyword.type == Token_Type::kw_module ||
          initial_keyword.type == Token_Type::kw_namespace) {
        Lexer_Transaction inner_transaction = this->lexer_.begin_transaction();
        this->skip();
        if (this->peek().type == Token_Type::left_curly &&
            !this->peek().has_leading_newline) {
          // namespace
          //   ns {}     // Invalid.
          // Treat 'namespace' as a keyword.
          is_expression = false;
          // Diag_Newline_Not_Allowed_After_Namespace_Keyword is reported
          // later by parse_and_visit_typescript_namespace.
        }
        this->lexer_.roll_back_transaction(std::move(inner_transaction));
      }
      if (is_expression) {
        goto initial_keyword_is_expression;
      }
    }
    this->lexer_.commit_transaction(std::move(transaction));
    switch (initial_keyword.type) {
    case Token_Type::kw_interface:
      this->parse_and_visit_typescript_interface(v, initial_keyword.span());
      break;
    case Token_Type::kw_module:
    case Token_Type::kw_namespace:
      this->parse_and_visit_typescript_namespace(
          v,
          /*export_keyword_span=*/std::nullopt, initial_keyword.span());
      break;
    case Token_Type::kw_type:
      this->parse_and_visit_typescript_type_alias(v, initial_keyword.span());
      break;
    default:
      QLJS_UNREACHABLE();
    }
    return Parse_Possible_Label_Result::parsed_not_as_a_label;

  // type++;  // Expression.
  initial_keyword_is_expression:
  default:
    this->lexer_.roll_back_transaction(std::move(transaction));
    this->parse_and_visit_expression(v);
    this->parse_end_of_expression_statement();
    return Parse_Possible_Label_Result::parsed_not_as_a_label;
  }
}

void Parser::parse_and_visit_break_or_continue() {
  bool is_break = this->peek().type == Token_Type::kw_break;
  Source_Code_Span token_span = this->peek().span();
  this->skip();
  switch (this->peek().type) {
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case Token_Type::identifier:
  case Token_Type::kw_await:
  case Token_Type::kw_yield:
    if (this->peek().has_leading_newline) {
      // ASI.
      this->lexer_.insert_semicolon();
    } else {
      // Loop label.
      this->skip();
    }
    break;
  default:
    if (is_break) {
      if (!(this->in_switch_statement_ || this->in_loop_statement_)) {
        this->diags_.add(Diag_Invalid_Break{token_span});
      }
    } else {
      if (!this->in_loop_statement_) {
        this->diags_.add(Diag_Invalid_Continue{token_span});
      }
    }
    break;
  }
  this->consume_semicolon_after_statement();
}

void Parser::parse_and_visit_throw_statement(Parse_Visitor_Base &v) {
  this->skip();
  if (this->peek().type == Token_Type::semicolon) {
    this->diags_.add(
        Diag_Expected_Expression_Before_Semicolon{this->peek().span()});
    this->skip();
    return;
  }
  if (this->peek().has_leading_newline) {
    this->lexer_.insert_semicolon();
    this->diags_.add(
        Diag_Expected_Expression_Before_Newline{this->peek().span()});
    this->skip();
    return;
  }
  this->parse_and_visit_expression(v);
  this->parse_end_of_expression_statement();
}

void Parser::parse_and_visit_return_statement(
    Parse_Visitor_Base &v, bool possibly_followed_by_another_statement) {
  Source_Code_Span return_span = this->peek().span();
  this->skip();
  switch (this->peek().type) {
  case Token_Type::semicolon:
    this->skip();
    break;

  case Token_Type::right_curly:
    break;

  default:
    if (this->peek().has_leading_newline) {
      switch (this->peek().type) {
        // 'return' followed by a newline (ASI) followed by an expression.
      case Token_Type::bang:
      case Token_Type::complete_template:
      case Token_Type::identifier:
      case Token_Type::incomplete_template:
      case Token_Type::kw_await:
      case Token_Type::kw_false:
      case Token_Type::kw_function:
      case Token_Type::kw_new:
      case Token_Type::kw_null:
      case Token_Type::kw_super:
      case Token_Type::kw_this:
      case Token_Type::kw_true:
      case Token_Type::kw_typeof:
      case Token_Type::left_curly:  // Object literal.
      case Token_Type::left_paren:
      case Token_Type::left_square:  // Array literal.
      case Token_Type::less:
      case Token_Type::minus:
      case Token_Type::number:
      case Token_Type::plus:
      case Token_Type::slash:        // Regular expression.
      case Token_Type::slash_equal:  // Regular expression.
      case Token_Type::string:
      case Token_Type::tilde:
        if (possibly_followed_by_another_statement) {
          this->diags_.add(Diag_Return_Statement_Returns_Nothing{
              .return_keyword = return_span,
          });
        }
        break;

      default:
        break;
      }
      // Insert a semicolon, then consume it.
    } else {
      this->parse_and_visit_expression(v);
      this->parse_end_of_expression_statement();
    }
    break;
  }
}

void Parser::parse_end_of_expression_statement() {
  while (this->peek().type == Token_Type::right_paren) {
    this->diags_.add(Diag_Unmatched_Parenthesis{
        .where = this->peek().span(),
    });
    this->skip();
  }
  this->consume_semicolon_after_statement();
}

void Parser::parse_and_visit_export(Parse_Visitor_Base &v) {
  this->parse_and_visit_export(v, Parse_Export_Options());
}

void Parser::parse_and_visit_export(Parse_Visitor_Base &v,
                                    const Parse_Export_Options &options) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_export);
  Source_Code_Span export_token_span = this->peek().span();
  this->skip();

  std::optional<Source_Code_Span> typescript_type_only_keyword;
  Stacked_Buffering_Visitor deferred_visits =
      this->buffering_visitor_stack_.push();

  switch (this->peek().type) {
    // export default class C {}
  case Token_Type::kw_default: {
    Source_Code_Span default_keyword = this->peek().span();

    this->is_current_typescript_namespace_non_empty_ = true;
    if (this->in_typescript_namespace_or_module_.has_value() &&
        !this->in_typescript_module_) {
      this->diags_.add(Diag_TypeScript_Namespace_Cannot_Export_Default{
          .default_keyword = default_keyword,
          .namespace_keyword = *this->in_typescript_namespace_or_module_,
      });
    }
    this->skip();

    this->found_default_export(default_keyword,
                               /*is_mergeable_interface=*/this->peek().type ==
                                   Token_Type::kw_interface);

    switch (this->peek().type) {
      // export default async function f() {}
      // export default async () => {}
    case Token_Type::kw_async: {
      Lexer_Transaction transaction = this->lexer_.begin_transaction();
      Token async_token = this->peek();
      this->skip();
      if (this->peek().type == Token_Type::kw_function) {
        this->lexer_.commit_transaction(std::move(transaction));
        if (this->peek().has_leading_newline) {
          // export default async  // ASI.
          // function f() {}
          v.visit_variable_export_default_use(async_token.identifier_name());
          this->consume_semicolon_after_statement();
          break;
        }
        this->parse_and_visit_function_declaration(
            v, Function_Declaration_Options{
                   .attributes = Function_Attributes::async,
                   .begin = async_token.begin,
                   .require_name = Name_Requirement::optional,
                   .async_keyword = async_token.span(),
                   .declare_keyword =
                       options.declare_context.maybe_declare_keyword_span(),
                   .export_keyword = export_token_span,
               });
      } else {
        this->lexer_.roll_back_transaction(std::move(transaction));
        goto export_default_expression;
      }
      break;
    }

    // export default class C {}
    parse_default_class:
    case Token_Type::kw_class:
      this->parse_and_visit_class(
          v, Parse_Class_Options{
                 .require_name = Name_Requirement::optional,
                 .abstract_keyword_span = std::nullopt,
                 .declare_keyword_span =
                     options.declare_context.maybe_declare_keyword_span(),
             });
      break;

    // export default @myDecorator class C {}
    case Token_Type::at:
      if (options.decorator_at_span.has_value()) {
        this->diags_.add(Diag_Decorator_Before_And_After_Export_Keyword{
            .decorator_at_before = *options.decorator_at_span,
            .decorator_at_after = this->peek().span(),
        });
      }
      // See NOTE[class-decorator-deferred-visits].
      this->parse_and_visit_one_or_more_decorators(deferred_visits.visitor());
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_class);
      goto parse_default_class;

    // export default abstract class C {}
    // export default abstract
    case Token_Type::kw_abstract: {
      Lexer_Transaction transaction = this->lexer_.begin_transaction();
      Source_Code_Span abstract_keyword = this->peek().span();
      this->skip();
      if (this->peek().has_leading_newline ||
          this->peek().type == Token_Type::semicolon) {
        // export default abstract;
        // export default abstract  // ASI.
        this->lexer_.roll_back_transaction(std::move(transaction));
        goto export_default_expression;
      } else {
        // export default abstract class C {}
        this->lexer_.commit_transaction(std::move(transaction));
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_class);
        this->parse_and_visit_class(
            v, Parse_Class_Options{
                   .require_name = Name_Requirement::optional,
                   .abstract_keyword_span = abstract_keyword,
                   .declare_keyword_span =
                       options.declare_context.maybe_declare_keyword_span(),
               });
      }
      break;
    }

      // export default function f() {}
    case Token_Type::kw_function:
      this->parse_and_visit_function_declaration(
          v, Function_Declaration_Options{
                 .attributes = Function_Attributes::normal,
                 .begin = this->peek().begin,
                 .require_name = Name_Requirement::optional,
                 .async_keyword = std::nullopt,
                 .declare_keyword =
                     options.declare_context.maybe_declare_keyword_span(),
                 .export_keyword = export_token_span,
             });
      break;

      // export default let x = null;  // Invalid.
      // export default let;           // Invalid.
    case Token_Type::kw_const:
    case Token_Type::kw_let:
    case Token_Type::kw_var: {
      Token declaring_token = this->peek();
      this->skip();
      this->diags_.add(Diag_Cannot_Export_Default_Variable{
          .declaring_token = declaring_token.span(),
      });
      this->parse_and_visit_let_bindings(v,
                                         Parse_Let_Bindings_Options{
                                             .declaring_token = declaring_token,
                                         });
      break;
    }

    // export default interface I {}  // TypeScript only.
    case Token_Type::kw_interface: {
      Source_Code_Span interface_keyword = this->peek().span();
      this->skip();
      this->parse_and_visit_typescript_interface(v, interface_keyword);
      break;
    }

    // export default MyClass;
    // export default 2 + 2;
    export_default_expression:
    default:
      Expression *ast = this->parse_expression(v);
      if (ast->kind() == Expression_Kind::Variable) {
        // export default MyClass;
        // declare module "m" { export default MyClass; }
        v.visit_variable_export_default_use(ast->variable_identifier());
      } else {
        // export default 2 + 2;
        // declare module "m" { export default 2 + 2; }  // Invalid.
        this->visit_expression(ast, v, Variable_Context::rhs);
      }
      this->consume_semicolon_after_statement();
      break;
    }
    break;
  }

    // export * from "module";
    // export * as name from "module";
  export_star:
  case Token_Type::star:
    // Do not set is_current_typescript_namespace_non_empty_. See
    // NOTE[ambiguous-ambient-statement-in-namespace].
    this->skip();
    if (this->peek().type == Token_Type::kw_as) {
      this->skip();
      switch (this->peek().type) {
      case Token_Type::string:
        // TODO(strager): Check that the string is valid Unicode
        // (standard: IsStringWellFormedUnicode).
        [[fallthrough]];
      QLJS_CASE_KEYWORD:
      case Token_Type::identifier:
      case Token_Type::reserved_keyword_with_escape_sequence:
        this->skip();
        break;
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_from);
    if (options.declare_context.declare_namespace_declare_keyword.has_value() &&
        !options.declare_context.in_module) {
      // declare namespace ns { export * from "b"; }
      // See NOTE[declare-import].
      this->diags_.add(Diag_Declare_Namespace_Cannot_Import_Module{
          .importing_keyword = this->peek().span(),
          .declare_keyword =
              *options.declare_context.declare_namespace_declare_keyword,
      });
    }
    this->skip();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::string);
    this->skip();
    this->consume_semicolon_after_statement();
    break;

    // export {a as default, b};
    // export {a, b, c} from "module";
  named_export_list:
  case Token_Type::left_curly: {
    // Do not set is_current_typescript_namespace_non_empty_. See
    // NOTE[ambiguous-ambient-statement-in-namespace].
    Stacked_Buffering_Visitor exports_visitor =
        this->buffering_visitor_stack_.push();
    Vector<Token> exported_bad_tokens(
        "parse_and_visit_export exported_bad_tokens", &this->temporary_memory_);
    this->parse_and_visit_named_exports(
        exports_visitor.visitor(),
        /*typescript_type_only_keyword=*/typescript_type_only_keyword,
        /*out_exported_bad_tokens=*/&exported_bad_tokens);
    if (this->peek().type == Token_Type::kw_from) {
      // export {a, b, c} from "module";
      if (options.declare_context.declare_namespace_declare_keyword
              .has_value() &&
          !options.declare_context.in_module) {
        this->diags_.add(Diag_Declare_Namespace_Cannot_Import_Module{
            .importing_keyword = this->peek().span(),
            .declare_keyword =
                *options.declare_context.declare_namespace_declare_keyword,
        });
      }
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::string);
      this->skip();
      // Ignore exported_keywords.
    } else {
      // export {a as default, b};
      for (const Token &exported_bad_token : exported_bad_tokens) {
        switch (exported_bad_token.type) {
        case Token_Type::reserved_keyword_with_escape_sequence:
          exported_bad_token.add_diags_for_escape_sequences_in_keyword(
              this->diag_reporter_->diags());
          break;
        case Token_Type::string:
          this->diags_.add(
              Diag_Exporting_String_Name_Only_Allowed_For_Export_From{
                  .export_name = exported_bad_token.span(),
              });
          break;
        default:
          this->diags_.add(Diag_Cannot_Export_Variable_Named_Keyword{
              .export_name = exported_bad_token.span(),
          });
          break;
        }
      }
      exports_visitor.visitor().move_into(v);
    }

    this->consume_semicolon_after_statement();
    break;
  }

    // export async function f() {}
  case Token_Type::kw_async: {
    this->is_current_typescript_namespace_non_empty_ = true;
    Source_Code_Span async_token_span = this->peek().span();
    this->skip();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_function);

    std::optional<Source_Code_Span> declare_keyword =
        options.declare_context.maybe_declare_keyword_span();
    if (this->options_.typescript_definition_file) {
      this->diags_.add(Diag_DTS_Function_Cannot_Be_Async{
          .async_keyword = async_token_span,
      });
    } else {
      if (declare_keyword.has_value()) {
        this->diags_.add(Diag_Declare_Function_Cannot_Be_Async{
            .async_keyword = async_token_span,
        });
      }
    }
    this->parse_and_visit_function_declaration(
        v, Function_Declaration_Options{
               .attributes = Function_Attributes::async,
               .begin = async_token_span.begin(),
               .require_name = Name_Requirement::required_for_export,
               .async_keyword = async_token_span,
               .declare_keyword = declare_keyword,
               .export_keyword = export_token_span,
           });
    break;
  }

    // export function f() {}
  case Token_Type::kw_function:
    this->is_current_typescript_namespace_non_empty_ = true;
    this->parse_and_visit_function_declaration(
        v, Function_Declaration_Options{
               .attributes = Function_Attributes::normal,
               .begin = this->peek().begin,
               .require_name = Name_Requirement::required_for_export,
               .async_keyword = std::nullopt,
               .declare_keyword =
                   options.declare_context.maybe_declare_keyword_span(),
               .export_keyword = export_token_span,
           });
    break;

  // export class C {}
  parse_class:
  case Token_Type::kw_class:
    this->is_current_typescript_namespace_non_empty_ = true;
    this->parse_and_visit_class(
        v, Parse_Class_Options{
               .require_name = Name_Requirement::required_for_export,
               .abstract_keyword_span = std::nullopt,
               .declare_keyword_span =
                   options.declare_context.declare_namespace_declare_keyword,
           });
    break;

  // export @myDecorator class C {}
  case Token_Type::at:
    if (options.decorator_at_span.has_value()) {
      this->diags_.add(Diag_Decorator_Before_And_After_Export_Keyword{
          .decorator_at_before = *options.decorator_at_span,
          .decorator_at_after = this->peek().span(),
      });
    }
    // See NOTE[class-decorator-deferred-visits].
    this->parse_and_visit_one_or_more_decorators(deferred_visits.visitor());
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_class);
    goto parse_class;

  // export abstract class C {}
  case Token_Type::kw_abstract: {
    this->is_current_typescript_namespace_non_empty_ = true;
    Source_Code_Span abstract_keyword = this->peek().span();
    this->skip();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_class);
    if (this->peek().has_leading_newline) {
      this->diags_.add(Diag_Newline_Not_Allowed_After_Abstract_Keyword{
          .abstract_keyword = abstract_keyword,
      });
    }
    this->parse_and_visit_class(
        v, Parse_Class_Options{
               .require_name = Name_Requirement::required_for_export,
               .abstract_keyword_span = abstract_keyword,
               .declare_keyword_span =
                   options.declare_context.declare_namespace_declare_keyword,
           });
    break;
  }

  // export let x = 42;
  // export const enum E {}  // TypeScript only.
  case Token_Type::kw_const:
  case Token_Type::kw_let:
  case Token_Type::kw_var:
    if (options.declare_context.declare_namespace_declare_keyword.has_value()) {
      // declare namespace ns { export let x; }
      // declare namespace ns { export const enum E {} }
      this->parse_and_visit_declare_statement(v, options.declare_context);
    } else {
      // is_current_typescript_namespace_non_empty_ is possibly set by
      // parse_and_visit_variable_declaration_statement.
      this->parse_and_visit_variable_declaration_statement(v);
    }
    break;

  // export interface I {}  // TypeScript only.
  case Token_Type::kw_interface: {
    // Do not set is_current_typescript_namespace_non_empty_.
    Source_Code_Span interface_keyword = this->peek().span();
    this->skip();
    if (this->peek().has_leading_newline) {
      this->diags_.add(Diag_Newline_Not_Allowed_After_Interface_Keyword{
          .interface_keyword = interface_keyword,
      });
    }
    this->parse_and_visit_typescript_interface(v, interface_keyword);
    break;
  }

  // export type A = B;        // TypeScript only.
  // export type {A, B as C};  // TypeScript only.
  case Token_Type::kw_type: {
    // Do not set is_current_typescript_namespace_non_empty_.
    Source_Code_Span type_keyword = this->peek().span();
    this->skip();
    switch (this->peek().type) {
    // export type {A, B as C};
    case Token_Type::left_curly:
      // Do not set is_current_typescript_namespace_non_empty_. See
      // NOTE[ambiguous-ambient-statement-in-namespace].
      typescript_type_only_keyword = type_keyword;
      if (!this->options_.typescript) {
        this->diags_.add(Diag_TypeScript_Type_Export_Not_Allowed_In_JavaScript{
            .type_keyword = type_keyword,
        });
      }
      goto named_export_list;

    // export type * from 'othermod';
    case Token_Type::star:
      typescript_type_only_keyword = type_keyword;
      if (!this->options_.typescript) {
        this->diags_.add(Diag_TypeScript_Type_Export_Not_Allowed_In_JavaScript{
            .type_keyword = type_keyword,
        });
      }
      goto export_star;

    default:
      // export type A = B;
      // Do not set is_current_typescript_namespace_non_empty_.
      this->parse_and_visit_typescript_type_alias(v, type_keyword);
      break;
    }
    break;
  }

  // export import A = ns;  // TypeScript only.
  case Token_Type::kw_import:
    this->is_current_typescript_namespace_non_empty_ = true;
    this->parse_and_visit_import(v);
    // TODO(#795): Report an error if the import is not a TypeScript import
    // alias.
    break;

  // export namespace ns {}  // TypeScript only.
  case Token_Type::kw_module:
  case Token_Type::kw_namespace: {
    if (options.declare_context.declare_namespace_declare_keyword.has_value()) {
      // declare namespace ns { export namespace subns {} }
      this->parse_and_visit_typescript_declare_namespace_or_module(
          v, *options.declare_context.declare_namespace_declare_keyword);
    } else {
      // export namespace ns {}
      Source_Code_Span namespace_keyword = this->peek().span();
      this->skip();
      this->parse_and_visit_typescript_namespace(
          v,
          /*export_keyword_span=*/export_token_span, namespace_keyword);
    }
    break;
  }

  // export as namespace MyLibrary;  // TypeScript definition only.
  case Token_Type::kw_as:
    // TODO(#690): 'declare namespace' stuff.
    this->skip();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_namespace);
    this->skip();
    switch (this->peek().type) {
    QLJS_CASE_CONTEXTUAL_KEYWORD:
    case Token_Type::identifier:
      this->skip();
      break;
    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    if (!this->options_.typescript_definition_file) {
      this->diags_.add(
          Diag_TypeScript_Export_As_Namespace_Is_Only_Allowed_In_TypeScript_Definition_File{
              .export_keyword = export_token_span,
          });
    } else if (this->in_typescript_namespace_or_module_.has_value()) {
      this->diags_.add(
          Diag_TypeScript_Export_As_Namespace_Is_Not_Allowed_In_Namespace_Or_Module{
              .export_keyword = export_token_span,
              .namespace_or_module_keyword =
                  *this->in_typescript_namespace_or_module_,
          });
    }
    this->consume_semicolon_after_statement();
    break;

  // export enum E {}  // TypeScript only.
  case Token_Type::kw_enum:
    // is_current_typescript_namespace_non_empty_ is possibly set by
    // parse_and_visit_typescript_enum.
    this->parse_and_visit_typescript_enum(
        v, options.declare_context.declare_namespace_declare_keyword.has_value()
               ? Enum_Kind::declare_enum
               : Enum_Kind::normal);
    break;

    // export stuff;    // Invalid.
    // export a, b, c;  // Invalid.
    // export 2 + 2;    // Invalid.
  case Token_Type::identifier:
  case Token_Type::number: {
    this->is_current_typescript_namespace_non_empty_ = true;
    Expression *ast = this->parse_expression(v);
    switch (ast->kind()) {
    case Expression_Kind::Variable:
      this->diags_.add(Diag_Exporting_Requires_Curlies{
          .names = ast->span(),
      });
      break;
    default:
      this->diags_.add(Diag_Exporting_Requires_Default{
          .expression = ast->span(),
      });
      break;
    }
    this->visit_expression(ast, v, Variable_Context::rhs);
    this->consume_semicolon_after_statement();
    break;
  }

  // export = foo;                // TypeScript only.
  // export = 2 + foo.bar.baz();  // TypeScript only.
  case Token_Type::equal: {
    this->is_current_typescript_namespace_non_empty_ = true;
    if (!this->options_.typescript) {
      this->diags_.add(Diag_TypeScript_Export_Equal_Not_Allowed_In_JavaScript{
          .equal = this->peek().span(),
          .export_keyword = export_token_span,
      });
    }
    this->skip();
    Expression *ast = this->parse_expression(v);
    if (ast->kind() == Expression_Kind::Variable) {
      v.visit_variable_export_use(ast->variable_identifier());
    } else {
      this->visit_expression(ast, v, Variable_Context::rhs);
    }
    this->consume_semicolon_after_statement();
    break;
  }

  case Token_Type::end_of_file:
    // Do not set is_current_typescript_namespace_non_empty_.
    this->diags_.add(Diag_Missing_Token_After_Export{
        .export_token = export_token_span,
    });
    break;

  case Token_Type::semicolon:
    this->is_current_typescript_namespace_non_empty_ = true;
    this->diags_.add(Diag_Missing_Token_After_Export{
        .export_token = export_token_span,
    });
    break;

  // export declare class C { }    // TypeScript only.
  // export declare import A = B;  // Invalid.
  case Token_Type::kw_declare: {
    Source_Code_Span declare_span = this->peek().span();
    this->skip();
    if (this->peek().has_leading_newline) {
      // export declare   // (newline)
      //   class C { }    // Invalid.
      this->diags_.add(Diag_Newline_Not_Allowed_After_Export_Declare{
          .declare_keyword = declare_span,
          .export_keyword = export_token_span,
      });
    }
    this->parse_and_visit_declare_statement(
        v, TypeScript_Declare_Context{
               .direct_declare_keyword = declare_span,
           });
    break;
  }

  default:
    this->diags_.add(Diag_Unexpected_Token_After_Export{
        .unexpected_token = this->peek().span(),
    });
    break;
  }

  // See NOTE[class-decorator-deferred-visits].
  deferred_visits.visitor().move_into(v);
}

void Parser::found_default_export(Source_Code_Span default_keyword,
                                  bool is_mergeable_interface) {
  if (this->in_typescript_module_) {
    // TODO(#1127): Report duplicate export inside TypeScript modules.
    return;
  }

  if (this->first_export_default_statement_default_keyword_.has_value()) {
    if (is_mergeable_interface) {
      // export default class {}  export default interface I {}
    } else {
      // export default class {}  export default class {}  // Invalid.
      this->diags_.add(Diag_Multiple_Export_Defaults{
          .second_export_default = default_keyword,
          .first_export_default =
              *this->first_export_default_statement_default_keyword_,
      });
    }
  } else {
    if (is_mergeable_interface) {
      // export default interface I {}  ...
      // export default interface I {}  export default class {}  // Merged.
      // Allow an interface to merge with any other default export.
      // TODO(#1107): An interface is not always mergeable with anything.
    } else {
      // export default class {}  ...
      this->first_export_default_statement_default_keyword_ = default_keyword;
    }
  }
}

void Parser::parse_and_visit_typescript_generic_parameters(
    Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::less);
  const Char8 *less_end = this->peek().end;
  this->skip();

  Vector<Source_Code_Span> leading_commas(
      "parse_and_visit_typescript_generic_parameters leading_commas",
      &this->temporary_memory_);
  while (this->peek().type == Token_Type::comma) {
    // <, T>   // Invalid.
    // <,>     // Invalid.
    leading_commas.emplace_back(this->peek().span());
    this->skip();
  }
  if (this->peek().type == Token_Type::greater) {
    // <,>    // Invalid.
    this->diags_.add(Diag_TypeScript_Generic_Parameter_List_Is_Empty{
        .expected_parameter = Source_Code_Span::unit(less_end),
    });
    for (Vector_Size i = 1; i < leading_commas.size(); ++i) {
      this->diags_.add(Diag_Multiple_Commas_In_Generic_Parameter_List{
          .unexpected_comma = leading_commas[i],
      });
    }
    this->skip();
    return;
  }
  for (const Source_Code_Span &comma : leading_commas) {
    // <, T>
    this->diags_.add(Diag_Comma_Not_Allowed_Before_First_Generic_Parameter{
        .unexpected_comma = comma,
    });
  }

  Variable_Kind parameter_kind = Variable_Kind::_generic_parameter;

  // NOTE[generic-type-parameter-visit-order]: Visit order is important.
  // Given:
  //
  //   <T extends U = Default>
  //
  // 'Default' should be visited first so references to 'T' report
  // use-before-declaration. 'U' should be visited after 'T' so references to
  // 'T' are legal.
  //
  // Also, given multiple parameters, all extends clauses should be visited
  // after all variables are declared. For example, the following is legal:
  //
  //   <T extends U, U>
  Stacked_Buffering_Visitor extends_visits =
      this->buffering_visitor_stack_.push();

next_parameter:
  // 'in' and 'out' keywords.
  struct Modifier {
    Identifier identifier;
    Token_Type token_type;
  };
  Vector<Modifier> modifiers(
      "parse_and_visit_typescript_generic_parameters modifiers",
      &this->temporary_memory_);
  for (;;) {
    switch (this->peek().type) {
    // <in T>
    // <in out T>
    // <const T>
    case Token_Type::kw_const:
    case Token_Type::kw_in:
    case Token_Type::kw_out:
      modifiers.push_back(Modifier{
          .identifier = this->peek().identifier_name(),
          .token_type = this->peek().type,
      });
      this->skip();
      if (this->lexer_.peek().has_leading_newline) {
        this->diags_.add(Diag_Newline_Not_Allowed_After_In_Out_Const_Modifiers{
            .modifier = modifiers.back().identifier.span()});
      }
      break;

    default:
      goto done_parsing_modifiers;
    }
  }
done_parsing_modifiers:

  std::optional<Identifier> parameter_name;
  switch (this->peek().type) {
  case Token_Type::identifier:
  case Token_Type::kw_abstract:
  case Token_Type::kw_accessor:
  case Token_Type::kw_as:
  case Token_Type::kw_assert:
  case Token_Type::kw_asserts:
  case Token_Type::kw_async:
  case Token_Type::kw_await:
  case Token_Type::kw_constructor:
  case Token_Type::kw_declare:
  case Token_Type::kw_from:
  case Token_Type::kw_get:
  case Token_Type::kw_global:
  case Token_Type::kw_infer:
  case Token_Type::kw_intrinsic:
  case Token_Type::kw_is:
  case Token_Type::kw_keyof:
  case Token_Type::kw_module:
  case Token_Type::kw_namespace:
  case Token_Type::kw_of:
  case Token_Type::kw_override:
  case Token_Type::kw_readonly:
  case Token_Type::kw_require:
  case Token_Type::kw_satisfies:
  case Token_Type::kw_set:
  case Token_Type::kw_type:
  case Token_Type::kw_undefined:
  case Token_Type::kw_unique:
    parameter_name = this->peek().identifier_name();
    this->skip();
    break;

  case Token_Type::kw_in:
  case Token_Type::kw_out:
    QLJS_UNREACHABLE();
    break;

  default:
    if (!modifiers.empty() &&
        modifiers.back().token_type == Token_Type::kw_out) {
      // <out>
      parameter_name = modifiers.back().identifier;
      modifiers.pop_back();
    } else {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    break;
  }

  // Report bad modifiers.
  {
    std::optional<Source_Code_Span> first_in_keyword;
    std::optional<Source_Code_Span> first_out_keyword;
    bool reported_out_in_misorder = false;
    for (const Modifier &modifier : modifiers) {
      if (modifier.token_type == Token_Type::kw_in) {
        if (first_in_keyword.has_value()) {
          this->diags_.add(Diag_TypeScript_Variance_Keyword_Repeated{
              .first_keyword = *first_in_keyword,
              .second_keyword = modifier.identifier.span(),
          });
        } else {
          first_in_keyword = modifier.identifier.span();
        }
      }
      if (modifier.token_type == Token_Type::kw_out) {
        if (first_out_keyword.has_value()) {
          this->diags_.add(Diag_TypeScript_Variance_Keyword_Repeated{
              .first_keyword = *first_out_keyword,
              .second_keyword = modifier.identifier.span(),
          });
        } else {
          first_out_keyword = modifier.identifier.span();
        }
      }
      if (!reported_out_in_misorder &&
          modifier.token_type == Token_Type::kw_in &&
          first_out_keyword.has_value()) {
        // <out in T>  // Invalid.
        this->diags_.add(Diag_TypeScript_Variance_Keywords_In_Wrong_Order{
            .in_keyword = modifier.identifier.span(),
            .out_keyword = *first_out_keyword,
        });
        reported_out_in_misorder = true;
      }
    }
  }

  if (this->peek().type == Token_Type::kw_extends ||
      this->peek().type == Token_Type::colon) {
    // <T: U>  // Invalid.
    // <T extends U>
    if (this->peek().type == Token_Type::colon) {
      // <T: U>  // Invalid.
      this->diags_.add(Diag_Unexpected_Colon_After_Generic_Definition{
          .colon = this->peek().span(),
      });
    }
    this->skip();
    this->parse_and_visit_typescript_type_expression(
        extends_visits.visitor(),
        TypeScript_Type_Parse_Options{
            .type_being_declared =
                TypeScript_Type_Parse_Options::Declaring_Type{
                    .name = *parameter_name,
                    .kind = parameter_kind,
                },
        });
  }

  if (this->peek().type == Token_Type::equal) {
    // <T = Default>
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
  }

  QLJS_ASSERT(parameter_name.has_value());
  v.visit_variable_declaration(*parameter_name, parameter_kind,
                               Variable_Declaration_Flags::none);

  switch (this->peek().type) {
  case Token_Type::greater:
    break;

  case Token_Type::comma:
    this->skip();
    while (this->peek().type == Token_Type::comma) {
      this->diags_.add(Diag_Multiple_Commas_In_Generic_Parameter_List{
          .unexpected_comma = this->peek().span(),
      });
      this->skip();
    }
    break;

  // <T U>  // Invalid.
  case Token_Type::identifier:
    this->diags_.add(Diag_Missing_Comma_Between_Generic_Parameters{
        .expected_comma =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
    });
    goto next_parameter;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  if (this->peek().type != Token_Type::greater) {
    goto next_parameter;
  }
  this->skip();

  // See NOTE[generic-type-parameter-visit-order].
  extends_visits.visitor().move_into(v);
}

void Parser::parse_and_visit_statement_block_no_scope(Parse_Visitor_Base &v) {
  this->parse_and_visit_statement_block_no_scope(
      v, Parse_Statement_Options{
             .possibly_followed_by_another_statement = true,
         });
}

void Parser::parse_and_visit_statement_block_no_scope(
    Parse_Visitor_Base &v, Parse_Statement_Options statement_options) {
  QLJS_ASSERT(this->peek().type == Token_Type::left_curly);
  Source_Code_Span left_curly_span = this->peek().span();
  this->skip();
  this->parse_and_visit_statement_block_after_left_curly(v, left_curly_span,
                                                         statement_options);
}

void Parser::parse_and_visit_statement_block_after_left_curly(
    Parse_Visitor_Base &v, Source_Code_Span left_curly_span) {
  this->parse_and_visit_statement_block_after_left_curly(
      v, left_curly_span,
      Parse_Statement_Options{
          .possibly_followed_by_another_statement = true,
      });
}

void Parser::parse_and_visit_statement_block_after_left_curly(
    Parse_Visitor_Base &v, Source_Code_Span left_curly_span,
    Parse_Statement_Options statement_options) {
  for (;;) {
    bool parsed_statement =
        this->parse_and_visit_statement(v, statement_options);
    if (!parsed_statement) {
      switch (this->peek().type) {
      case Token_Type::right_curly:
        this->skip();
        return;

      case Token_Type::end_of_file:
        this->diags_.add(Diag_Unclosed_Code_Block{
            .block_open = left_curly_span,
        });
        return;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
  }
}

void Parser::parse_and_visit_function_declaration(
    Parse_Visitor_Base &v, Function_Declaration_Options options) {
  Parameter_List_Options parameter_list_options = {
      // TODO[declare-namespace-function-ASI]: Set is_declare_function to true
      // only for direct declare, not functions inside a declare namespace.
      .is_declare_function = options.declare_keyword.has_value(),
  };
  auto parse_and_visit_declare_function_parameters_and_body =
      [&](std::optional<Source_Code_Span> function_name) -> void {
    // declare function f();  // TypeScript only
    v.visit_enter_function_scope();
    Function_Guard guard = this->enter_function(options.attributes);

    Function_Parameter_Parse_Result result =
        this->parse_and_visit_function_parameter_list(v, function_name,
                                                      parameter_list_options);
    switch (result) {
    case Function_Parameter_Parse_Result::parsed_parameters:
    case Function_Parameter_Parse_Result::missing_parameters:
      if (this->options_.typescript_definition_file) {
        this->diags_.add(Diag_DTS_Function_Cannot_Have_Body{
            .body_start = this->peek().span(),
        });
      } else {
        QLJS_ASSERT(options.declare_keyword.has_value());
        this->diags_.add(Diag_Declare_Function_Cannot_Have_Body{
            .body_start = this->peek().span(),
            .declare_keyword = *options.declare_keyword,
        });
      }
      v.visit_enter_function_scope_body();
      this->parse_and_visit_statement_block_no_scope(v);
      break;

    case Function_Parameter_Parse_Result::missing_parameters_ignore_body:
    case Function_Parameter_Parse_Result::parsed_parameters_missing_body:
      this->consume_semicolon_after_statement();
      break;
    }

    v.visit_exit_function_scope();
  };

  if (options.declare_keyword.has_value() && !this->options_.typescript) {
    this->diags_.add(Diag_Declare_Function_Not_Allowed_In_JavaScript{
        .declare_keyword = *options.declare_keyword,
    });
  }

  QLJS_ASSERT(this->peek().type == Token_Type::kw_function);
  Source_Code_Span function_token_span = this->peek().span();
  const Char8 *function_token_begin = function_token_span.begin();
  this->skip();

  // NOTE(strager): This potentially updates options.attributes.
  std::optional<Source_Code_Span> generator_star =
      this->parse_generator_star(&options.attributes);
  if (generator_star.has_value()) {
    if (this->options_.typescript_definition_file) {
      this->diags_.add(Diag_DTS_Function_Cannot_Be_Generator{
          .star = *generator_star,
      });
    } else if (options.declare_keyword.has_value()) {
      // declare function *f();  // Invalid.
      this->diags_.add(Diag_Declare_Function_Cannot_Be_Generator{
          .star = *generator_star,
      });
    }
  }

  switch (this->peek().type) {
  case Token_Type::kw_await:
    if (this->in_async_function_) {
      this->diags_.add(Diag_Cannot_Declare_Await_In_Async_Function{
          .name = this->peek().span(),
      });
    }
    goto named_function;

  case Token_Type::kw_yield:
    if (this->in_generator_function_) {
      this->diags_.add(Diag_Cannot_Declare_Yield_In_Generator_Function{
          .name = this->peek().span(),
      });
    }
    goto named_function;

  // function protected() {}
  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    goto named_function;

  // function f() {}
  // declare function f();  // TypeScript only
  named_function:
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case Token_Type::identifier: {
    if (this->peek().type == Token_Type::kw_let &&
        options.require_name == Name_Requirement::required_for_export) {
      this->diags_.add(Diag_Cannot_Export_Let{
          .export_name = this->peek().span(),
      });
    }
    Identifier function_name = this->peek().identifier_name();
    this->skip();

    Vector<Identifier> overload_names(
        "parse_and_visit_function_declaration overload_names",
        &this->temporary_memory_);

    // For each signature without an 'export' keyword, this Vector contains
    // where the missing 'export' keyword would belong. This is used to report
    // Diag_Missing_Export_For_Function_With_Overload_Signature.
    Vector<Source_Code_Span> missing_export_locations(
        "parse_and_visit_function_declaration missing_export_locations",
        &this->temporary_memory_);
    // A found 'export' keyword, if any.
    std::optional<Source_Code_Span> found_export_keyword = std::nullopt;
    if (this->options_.typescript) {
      if (options.export_keyword.has_value()) {
        found_export_keyword = *options.export_keyword;
      } else {
        missing_export_locations.push_back(
            Source_Code_Span::unit(options.begin));
      }
    }

  next_overload:
    if (options.declare_keyword.has_value() ||
        this->options_.typescript_definition_file) {
      // declare function f();  // TypeScript only
      parse_and_visit_declare_function_parameters_and_body(
          function_name.span());
    } else {
      // function f() {}
      v.visit_enter_function_scope();
      {
        Function_Guard guard = this->enter_function(options.attributes);
        Function_Parameter_Parse_Result result =
            this->parse_and_visit_function_parameter_list(
                v, function_name.span(), parameter_list_options);
        switch (result) {
        case Function_Parameter_Parse_Result::parsed_parameters:
        case Function_Parameter_Parse_Result::missing_parameters:
          v.visit_enter_function_scope_body();
          this->parse_and_visit_statement_block_no_scope(v);
          break;

        case Function_Parameter_Parse_Result::missing_parameters_ignore_body:
          break;

        case Function_Parameter_Parse_Result::parsed_parameters_missing_body:
          if (this->options_.typescript) {
            Overload_Signature_Parse_Result r =
                this->parse_end_of_typescript_overload_signature(function_name);
            if (r.is_overload_signature) {
              if (generator_star.has_value()) {
                this->diags_.add(
                    Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star{
                        .generator_star = *generator_star,
                    });
              }

              if (!found_export_keyword.has_value() &&
                  r.second_function_export_keyword.has_value()) {
                found_export_keyword = *r.second_function_export_keyword;
              }
              if (!r.second_function_export_keyword.has_value() &&
                  r.second_function_expected_export.has_value()) {
                missing_export_locations.push_back(
                    *r.second_function_expected_export);
              }

              v.visit_exit_function_scope();
              options.attributes = r.second_function_attributes;
              generator_star = r.second_function_generator_star;
              if (overload_names.empty()) {
                // Lazily initialize overload_names with the first function's
                // name.
                overload_names.push_back(function_name);
              }
              overload_names.push_back(*r.second_function_name);
              goto next_overload;
            }
            if (!r.has_missing_body_error) {
              goto invalid_typescript_function_overload_signature;
            }
          }
          this->diags_.add(Diag_Missing_Function_Body{
              .expected_body = Source_Code_Span::unit(
                  this->lexer_.end_of_previous_token())});
        invalid_typescript_function_overload_signature:
          break;
        }
      }
      v.visit_exit_function_scope();

      if (!overload_names.empty()) {
        QLJS_ASSERT(overload_names.size() >= 2);
        Identifier &real_function_name = overload_names.back();

        // Detect mismatched function names.
        //
        // We just parsed code like the following:
        //
        //     function f();        // #0
        //     function f(a);       // #1
        //     function f(a, b);    // #2
        //     function f(a, b) {}  // #3
        //
        // The last overload (#3 above) is the real function.
        for (Vector_Size i = 0; i < overload_names.size() - 1; ++i) {
          Identifier &overload_name = overload_names[i];
          if (overload_name.normalized_name() !=
              real_function_name.normalized_name()) {
            v.visit_variable_declaration(overload_name,
                                         Variable_Kind::_function,
                                         Variable_Declaration_Flags::none);

            this->diags_.add(
                Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name{
                    .overload_name = overload_name.span(),
                    .function_name = real_function_name.span(),
                });
          }
        }
        function_name = overload_names[overload_names.size() - 1];
      }

      if (found_export_keyword.has_value()) {
        for (Source_Code_Span missing_export : missing_export_locations) {
          this->diags_.add(
              Diag_Missing_Export_For_Function_With_Overload_Signature{
                  .expected_export = missing_export,
                  .existing_export = *found_export_keyword,
              });
        }
      }
    }

    v.visit_variable_declaration(function_name, Variable_Kind::_function,
                                 Variable_Declaration_Flags::none);
    break;
  }

    // export default function() {}
  case Token_Type::left_paren:
    switch (options.require_name) {
    case Name_Requirement::required_for_statement: {
      const Char8 *left_paren_begin = this->peek().begin;
      const Char8 *left_paren_end = this->peek().end;

      if (options.declare_keyword.has_value()) {
        // declare function();  // Invalid.
        this->diags_.add(Diag_Missing_Name_In_Function_Statement{
            .where = Source_Code_Span::unit(left_paren_begin),
        });
        parse_and_visit_declare_function_parameters_and_body(
            /*function_name=*/std::nullopt);
      } else {
        // function() {}  // Invalid.
        // function() {}()  // Invalid.

        // The function should have a name, but doesn't have a name. Perhaps the
        // user intended to include parentheses. Parse the function as an
        // expression instead of as a declaration.
        this->parse_and_visit_function_parameters_and_body(
            v, /*name=*/std::nullopt, options.attributes,
            Parameter_List_Options());
        const Char8 *function_end = this->lexer_.end_of_previous_token();
        Expression *function = this->make_expression<Expression::Function>(
            options.attributes,
            Source_Code_Span(function_token_begin, function_end));

        if (this->peek().type != Token_Type::left_paren) {
          this->diags_.add(Diag_Missing_Name_In_Function_Statement{
              .where = Source_Code_Span::unit(left_paren_begin),
          });
        } else {
          this->diags_.add(Diag_Missing_Name_Or_Parentheses_For_Function{
              .where = Source_Code_Span(function_token_begin, left_paren_end),
              .function =
                  Source_Code_Span(options.begin, function->span().end()),
          });
          Expression *full_expression =
              this->parse_expression_remainder(v, function, Precedence{});
          this->visit_expression(full_expression, v, Variable_Context::rhs);
        }
      }

      break;
    }

    case Name_Requirement::required_for_export: {
      this->diags_.add(Diag_Missing_Name_Of_Exported_Function{
          .function_keyword = function_token_span,
      });
      this->parse_and_visit_function_parameters_and_body(
          v, /*name=*/std::nullopt, options.attributes,
          Parameter_List_Options());
      break;
    }

    case Name_Requirement::optional:
      this->parse_and_visit_function_parameters_and_body(
          v, /*name=*/std::nullopt, options.attributes,
          Parameter_List_Options());
      break;
    }
    break;

    // { function }  // Invalid.
  default:
    this->diags_.add(Diag_Missing_Name_In_Function_Statement{
        .where = function_token_span,
    });
    break;
  }
}

void Parser::parse_and_visit_function_parameters_and_body(
    Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
    Function_Attributes attributes, Parameter_List_Options options) {
  v.visit_enter_function_scope();
  this->parse_and_visit_function_parameters_and_body_no_scope(
      v, name, attributes, options);
  v.visit_exit_function_scope();
}

void Parser::parse_and_visit_function_parameters_and_body_no_scope(
    Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
    Function_Attributes attributes, Parameter_List_Options options) {
  Function_Guard guard = this->enter_function(attributes);
  Function_Parameter_Parse_Result result =
      this->parse_and_visit_function_parameter_list(v, name, options);
  switch (result) {
  case Function_Parameter_Parse_Result::parsed_parameters:
  case Function_Parameter_Parse_Result::missing_parameters:
    v.visit_enter_function_scope_body();
    this->parse_and_visit_statement_block_no_scope(v);
    break;

  case Function_Parameter_Parse_Result::missing_parameters_ignore_body:
    break;

  case Function_Parameter_Parse_Result::parsed_parameters_missing_body:
    this->diags_.add(Diag_Missing_Function_Body{
        .expected_body =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token())});
    break;
  }
}

void Parser::parse_and_visit_abstract_function_parameters_and_body_no_scope(
    Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
    Function_Attributes attributes, Parameter_List_Options options) {
  Function_Guard guard = this->enter_function(attributes);
  Function_Parameter_Parse_Result result =
      this->parse_and_visit_function_parameter_list(v, name, options);
  switch (result) {
  case Function_Parameter_Parse_Result::missing_parameters_ignore_body:
  case Function_Parameter_Parse_Result::parsed_parameters_missing_body:
    this->consume_semicolon<Diag_Missing_Semicolon_After_Abstract_Method>();
    break;

  case Function_Parameter_Parse_Result::parsed_parameters:
  case Function_Parameter_Parse_Result::missing_parameters:
    this->diags_.add(Diag_Abstract_Methods_Cannot_Contain_Bodies{
        .body_start = this->peek().span(),
    });
    v.visit_enter_function_scope_body();
    this->parse_and_visit_statement_block_no_scope(v);
    break;
  }
}

void Parser::parse_and_visit_declare_class_method_parameters_and_body(
    Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
    Function_Attributes attributes, Parameter_List_Options options) {
  QLJS_ASSERT(options.declare_class_keyword.has_value() ||
              this->options_.typescript_definition_file);
  v.visit_enter_function_scope();
  Function_Guard guard = this->enter_function(attributes);
  Function_Parameter_Parse_Result result =
      this->parse_and_visit_function_parameter_list(v, name, options);
  switch (result) {
  case Function_Parameter_Parse_Result::missing_parameters_ignore_body:
  case Function_Parameter_Parse_Result::parsed_parameters_missing_body:
    this->consume_semicolon<
        Diag_Missing_Semicolon_After_Declare_Class_Method>();
    break;

  case Function_Parameter_Parse_Result::parsed_parameters:
  case Function_Parameter_Parse_Result::missing_parameters:
    this->diags_.add(Diag_Declare_Class_Methods_Cannot_Contain_Bodies{
        .body_start = this->peek().span(),
    });
    v.visit_enter_function_scope_body();
    this->parse_and_visit_statement_block_no_scope(v);
    break;
  }
  v.visit_exit_function_scope();
}

void Parser::parse_and_visit_interface_function_parameters_and_body_no_scope(
    Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
    Function_Attributes attributes, Parameter_List_Options options) {
  QLJS_ASSERT(options.is_interface_method);
  Function_Guard guard = this->enter_function(attributes);
  Function_Parameter_Parse_Result result =
      this->parse_and_visit_function_parameter_list(v, name, options);
  switch (result) {
  case Function_Parameter_Parse_Result::missing_parameters_ignore_body:
  case Function_Parameter_Parse_Result::parsed_parameters_missing_body:
    this->consume_semicolon_or_comma<
        Diag_Missing_Semicolon_After_Interface_Method>();
    break;

  case Function_Parameter_Parse_Result::parsed_parameters:
  case Function_Parameter_Parse_Result::missing_parameters:
    this->diags_.add(Diag_Interface_Methods_Cannot_Contain_Bodies{
        .body_start = this->peek().span(),
    });
    v.visit_enter_function_scope_body();
    this->parse_and_visit_statement_block_no_scope(v);
    break;
  }
}

Parser::Function_Parameter_Parse_Result
Parser::parse_and_visit_function_parameter_list(
    Parse_Visitor_Base &v, std::optional<Source_Code_Span> name,
    Parameter_List_Options options) {
  if (this->peek().type == Token_Type::star) {
    if (!name.has_value()) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    // TODO(strager): Emit a different error if a star was already present
    // (e.g. function* f*() {}).
    this->diags_.add(Diag_Generator_Function_Star_Belongs_Before_Name{
        .function_name = *name,
        .star = this->peek().span(),
    });
    // in_generator_function_ is restored by an existing function_guard.
    // TODO(strager): Make an explicit guard ourselves instead. We don't
    // guarantee that the caller made a guard.
    this->in_generator_function_ = true;
    this->skip();
  }

  if (this->peek().type == Token_Type::less) {
    // function f<T>() {}  // TypeScript only.
    if (!this->options_.typescript) {
      this->diags_.add(Diag_TypeScript_Generics_Not_Allowed_In_JavaScript{
          .opening_less = this->peek().span(),
      });
    }
    this->parse_and_visit_typescript_generic_parameters(v);
  }

  switch (this->peek().type) {
    // function f(arg0, arg1) {}
  case Token_Type::left_paren:
    this->skip();

    this->parse_and_visit_function_parameters(
        v, Variable_Kind::_function_parameter, options);

    if (this->peek().type != Token_Type::right_paren) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->skip();

    if (this->peek().type == Token_Type::colon) {
      this->parse_and_visit_typescript_colon_type_expression(
          v, TypeScript_Type_Parse_Options{
                 .allow_assertion_signature_or_type_predicate = true,

                 // Force ASI before '<' if '<' might be legal after a ';'.
                 // Otherwise, don't force ASI, improving diagnostics.
                 //
                 // declare function f(): C  // Force ASI.
                 // <Component />;
                 //
                 // interface I {
                 //   f(): C          // Force ASI.
                 //   <N>(): number;
                 // }
                 .stop_parsing_type_at_newline_before_generic_arguments =
                     options.is_declare_function || options.is_interface_method,
             });
    }

    if (this->peek().type == Token_Type::equal_greater) {
      this->diags_.add(Diag_Functions_Or_Methods_Should_Not_Have_Arrow_Operator{
          .arrow_operator = this->peek().span(),
      });
      this->skip();
    }

    if (this->peek().type != Token_Type::left_curly) {
      return Function_Parameter_Parse_Result::parsed_parameters_missing_body;
    }
    return Function_Parameter_Parse_Result::parsed_parameters;

    // function f {}  // Invalid.
  case Token_Type::left_curly:
    this->diags_.add(Diag_Missing_Function_Parameter_List{
        .expected_parameter_list =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
    });
    return Function_Parameter_Parse_Result::missing_parameters;

    // { function f }  // Invalid.
  case Token_Type::comma:
  case Token_Type::dot:
  case Token_Type::number:
  case Token_Type::right_curly:
    this->diags_.add(Diag_Missing_Function_Parameter_List{
        .expected_parameter_list =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
    });
    return Function_Parameter_Parse_Result::missing_parameters_ignore_body;

    // function async f() {}  // Invalid. Should be async function f() {}
  case Token_Type::identifier:
    // TODO: Make parse_and_visit_function_parameters accept a token instead of
    // a Source_Code_Span so we can compare the token type instead of strings.
    if (name->string_view() == u8"async"_sv) {
      this->diags_.add(Diag_Function_Async_Function{
          .function_async = this->peek().span(),
      });
      this->skip();
      return this->parse_and_visit_function_parameter_list(v, name, options);
    }
    QLJS_PARSER_UNIMPLEMENTED();
    return Function_Parameter_Parse_Result::parsed_parameters;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    return Function_Parameter_Parse_Result::parsed_parameters;
  }
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
void Parser::parse_and_visit_function_parameters(
    Parse_Visitor_Base &v, Variable_Kind parameter_kind,
    Parameter_List_Options options) {
  std::optional<Source_Code_Span> last_parameter_spread_span = std::nullopt;
  std::optional<Source_Code_Span> previous_optional_span = std::nullopt;
  bool first_parameter = true;

  const Char8 *first_parameter_begin = this->peek().begin;
  for (;;) {
    std::optional<Source_Code_Span> comma_span = std::nullopt;
    if (!first_parameter) {
      if (this->peek().type != Token_Type::comma) {
        break;
      }
      comma_span = this->peek().span();
      this->skip();
    }
    if (this->peek().type == Token_Type::right_paren) {
      if (last_parameter_spread_span.has_value()) {
        // function f(...args,)  // Trailing comma is illegal.
        QLJS_ASSERT(comma_span.has_value());
        this->diags_.add(Diag_Comma_Not_Allowed_After_Spread_Parameter{
            .comma = *comma_span,
            .spread = *last_parameter_spread_span,
        });
      }
      goto done;
    }

    auto is_after_parameter_name = [this]() -> bool {
      switch (this->peek().type) {
      // function foo(paramName = def) {}
      // function foo(paramName: any) {}   // TypeScript only.
      // function foo(paramName?) {}       // TypeScript only.
      // function foo(paramName) {}
      case Token_Type::colon:
      case Token_Type::comma:
      case Token_Type::equal:
      case Token_Type::question:
      case Token_Type::right_paren:
        return true;

      // constructor(paramName myField) {}     // TypeScript only.
      // constructor(paramName [myField]) {}   // Invalid.
      // constructor(paramName @decorator) {}  // Invalid.
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
      case Token_Type::at:
      case Token_Type::dot_dot_dot:
      case Token_Type::identifier:
      case Token_Type::kw_await:
      case Token_Type::kw_yield:
      case Token_Type::left_curly:
      case Token_Type::left_square:
        return false;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }
    };

    std::optional<Source_Code_Span> parameter_property_keyword = std::nullopt;
    auto parse_parameter_property_keyword = [&]() -> void {
      Source_Code_Span accessor_span = this->peek().span();
      Lexer_Transaction transaction = this->lexer_.begin_transaction();
      this->skip();
      if (is_after_parameter_name()) {
        this->lexer_.roll_back_transaction(std::move(transaction));
      } else {
        if (!parameter_property_keyword.has_value()) {
          if (!options.is_class_constructor) {
            this->diags_.add(
                Diag_TypeScript_Parameter_Property_Only_Allowed_In_Class_Constructor{
                    .property_keyword = accessor_span,
                });
          }
          if (options.declare_class_keyword) {
            this->diags_.add(
                Diag_TypeScript_Parameter_Property_Not_Allowed_In_Declare_Class{
                    .property_keyword = accessor_span,
                    .declare_keyword = *options.declare_class_keyword,
                });
          }
          if (!this->options_.typescript) {
            this->diags_.add(
                Diag_TypeScript_Parameter_Property_Not_Allowed_In_JavaScript{
                    .property_keyword = accessor_span,
                });
          }
        }
        parameter_property_keyword = accessor_span;
        this->lexer_.commit_transaction(std::move(transaction));
      }
    };

    if (this->peek().type == Token_Type::at) {
      // class C { method(@myDecorator parameter) {} }  // TypeScript only.
      if (!this->options_.typescript) {
        this->diags_.add(
            Diag_TypeScript_Parameter_Decorator_Not_Allowed_In_JavaScript{
                .at = this->peek().span(),
            });
      } else {
        if (!options.is_class_method) {
          this->diags_.add(Diag_Parameter_Decorator_In_Non_Class_Method{
              .decorator_at = this->peek().span(),
          });
        }
        if (options.declare_class_keyword.has_value()) {
          this->diags_.add(Diag_Parameter_Decorator_In_Declare_Class{
              .decorator_at = this->peek().span(),
              .declare_keyword = *options.declare_class_keyword,
          });
        }
        if (options.abstract_method_keyword.has_value()) {
          this->diags_.add(Diag_Parameter_Decorator_In_Abstract_Method{
              .decorator_at = this->peek().span(),
              .abstract_keyword = *options.abstract_method_keyword,
          });
        }
      }
      this->parse_and_visit_one_or_more_decorators(v);
    }

    switch (this->peek().type) {
    // function foo(public) {}
    // constructor(public myField) {}  // TypeScript only.
    case Token_Type::kw_private:
    case Token_Type::kw_protected:
    case Token_Type::kw_public:
      // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
      parse_parameter_property_keyword();
      break;
    default:
      break;
    }
    if (this->peek().type == Token_Type::kw_readonly) {
      Source_Code_Span readonly_span = this->peek().span();

      // function foo(readonly) {}
      // constructor(readonly myField) {}         // TypeScript only.
      // constructor(public readonly myField) {}  // TypeScript only.
      parse_parameter_property_keyword();

      // constructor(readonly public value) {}  // Invalid
      // constructor(public readonly value) {}  // Ok, TypeScript only
      // constructor(readonly public) {}        // Ok, TypeScript only
      //                                        // public used as identifier
      if (this->options_.typescript) {
        switch (this->peek().type) {
        case Token_Type::kw_private:
        case Token_Type::kw_public:
        case Token_Type::kw_protected: {
          Source_Code_Span access_specifier_span = this->peek().span();
          Parser_Transaction transaction = this->begin_transaction();
          this->skip();

          if (is_after_parameter_name()) {
            this->roll_back_transaction(std::move(transaction));
          } else {
            this->commit_transaction(std::move(transaction));
            this->diags_.add(Diag_Access_Specifier_Must_Precede_Other_Modifiers{
                .second_modifier = access_specifier_span,
                .first_modifier = readonly_span,
            });
          }
          break;
        }
        default:
          break;
        }
      }
    }

    if (this->peek().type == Token_Type::at) {
      // class C { method(readonly @myDecorator parameter) {} }  // Invalid.
      QLJS_ASSERT(parameter_property_keyword.has_value());
      this->diags_.add(Diag_Parameter_Decorator_Must_Preceed_Modifiers{
          .modifier = *parameter_property_keyword,
          .decorator_at = this->peek().span(),
      });
      this->parse_and_visit_one_or_more_decorators(v);
    }

    switch (this->peek().type) {
    QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
      [[fallthrough]];
    case Token_Type::kw_await:
      // TODO(#241): Disallow parameters named 'await' for async functions.
      [[fallthrough]];
    QLJS_CASE_CONTEXTUAL_KEYWORD:
    case Token_Type::dot_dot_dot:
    case Token_Type::identifier:
    case Token_Type::kw_this:
    case Token_Type::kw_yield:
    case Token_Type::left_curly:
    case Token_Type::left_paren:
    case Token_Type::left_square:
    case Token_Type::less:
    case Token_Type::number:
    case Token_Type::reserved_keyword_with_escape_sequence: {
      Expression *parameter = this->parse_expression(
          v,
          Precedence{
              .commas = false,
              .in_operator = true,
              .colon_type_annotation = Allow_Type_Annotations::always,
              .colon_question_is_typescript_optional_with_type_annotation =
                  true,
              .stop_parsing_type_at_newline_before_generic_arguments_in_type_annotation =
                  false,
          });
      switch (parameter->kind()) {
      case Expression_Kind::Array:
        if (parameter_property_keyword.has_value()) {
          // constructor(private [field])  // Invalid.
          this->diags_.add(
              Diag_TypeScript_Parameter_Property_Cannot_Be_Destructured{
                  .destructure_token =
                      expression_cast<const Expression::Array *>(parameter)
                          ->left_square_span(),
                  .property_keyword = *parameter_property_keyword,
              });
        }
        break;
      case Expression_Kind::Object:
        if (parameter_property_keyword.has_value()) {
          // constructor(private {field})  // Invalid.
          this->diags_.add(
              Diag_TypeScript_Parameter_Property_Cannot_Be_Destructured{
                  .destructure_token =
                      expression_cast<const Expression::Object *>(parameter)
                          ->left_curly_span(),
                  .property_keyword = *parameter_property_keyword,
              });
        }
        break;
      case Expression_Kind::Spread:
        if (parameter_property_keyword.has_value()) {
          // constructor(private ...field)  // Invalid.
          this->diags_.add(Diag_TypeScript_Parameter_Property_Cannot_Be_Rest{
              .spread = expression_cast<const Expression::Spread *>(parameter)
                            ->spread_operator_span(),
              .property_keyword = *parameter_property_keyword,
          });
        }
        break;
      default:
        break;
      }
      this->visit_binding_element(
          parameter, v,
          Binding_Element_Info{
              .declaration_kind = parameter_kind,
              .declaring_token = std::nullopt,
              .flags = Variable_Declaration_Flags::none,
              .first_parameter_begin = first_parameter_begin,
          });

      if (parameter->kind() == Expression_Kind::Optional ||
          (parameter->kind() == Expression_Kind::Type_Annotated &&
           parameter->child_0()->kind() == Expression_Kind::Optional)) {
        previous_optional_span = parameter->span();
      } else {
        if (previous_optional_span.has_value() &&
            !(parameter->kind() == Expression_Kind::Spread ||
              parameter->kind() == Expression_Kind::Assignment)) {
          this->diags_.add(
              Diag_Optional_Parameter_Cannot_Be_Followed_By_Required_Parameter{
                  .optional_parameter = *previous_optional_span,
                  .required_parameter = parameter->span()});
        }
        previous_optional_span = std::nullopt;
      }
      if (parameter->kind() == Expression_Kind::Spread) {
        last_parameter_spread_span = parameter->span();
      } else {
        last_parameter_spread_span = std::nullopt;
      }
      break;
    }
    default:
      if (is_after_parameter_name()) {
        this->diags_.add(Diag_Missing_Parameter_Name{
            .expected_parameter_name =
                Source_Code_Span::unit(this->peek().begin),
        });
      } else {
        QLJS_PARSER_UNIMPLEMENTED();
      }
      break;
    }
    first_parameter = false;
  }
done:;
}
QLJS_WARNING_POP

Parser::Overload_Signature_Parse_Result
Parser::parse_end_of_typescript_overload_signature(
    const Identifier &function_name) {
  // Check if this is a function overload signature by parsing everything before
  // the following function's parameter list.
  //
  // function f()  // ASI
  // function f() {}
  //
  // function f(); function f() {}
  // function f(); function g() {}  // Invalid (not an overload).
  // function f(); banana();        // Invalid (not an overload).

  Lexer_Transaction transaction = this->lexer_.begin_transaction();
  Function_Attributes second_function_attributes = Function_Attributes::normal;

  std::optional<Source_Code_Span> second_function_generator_star = std::nullopt;
  std::optional<Source_Code_Span> second_function_export_keyword = std::nullopt;
  std::optional<Source_Code_Span> second_function_expected_export =
      std::nullopt;

  auto roll_back_missing_body = [&]() -> Overload_Signature_Parse_Result {
    this->lexer_.roll_back_transaction(std::move(transaction));
    return Overload_Signature_Parse_Result{
        .is_overload_signature = false,
        .has_missing_body_error = true,
        .second_function_attributes = second_function_attributes,
        .second_function_generator_star = second_function_generator_star,
        .second_function_export_keyword = second_function_export_keyword,
        .second_function_expected_export = second_function_expected_export,
    };
  };

  std::optional<Source_Code_Span> semicolon_span;
  if (this->peek().type == Token_Type::semicolon) {
    semicolon_span = this->peek().span();
    this->skip();
  } else if (!this->peek().has_leading_newline) {
    return roll_back_missing_body();
  }

  second_function_expected_export = Source_Code_Span::unit(this->peek().begin);
  if (this->peek().type == Token_Type::kw_export) {
    second_function_export_keyword = this->peek().span();
    this->skip();
  }

  if (this->peek().type == Token_Type::kw_default) {
    // export default function f(); export default function f() {}
    if (!second_function_export_keyword.has_value()) {
      // function f(); default function f() {}  // Invalid.
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->skip();
  }

  std::optional<Source_Code_Span> async_keyword;
  if (this->peek().type == Token_Type::kw_async) {
    async_keyword = this->peek().span();
    this->skip();
    second_function_attributes = Function_Attributes::async;
  }

  if (this->peek().type != Token_Type::kw_function) {
    return roll_back_missing_body();
  }

  Source_Code_Span function_keyword = this->peek().span();
  bool has_newline_after_async_keyword =
      async_keyword.has_value() && this->peek().has_leading_newline;
  this->skip();

  second_function_generator_star =
      this->parse_generator_star(&second_function_attributes);

  switch (this->peek().type) {
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case Token_Type::identifier:
    break;

  default:
    return roll_back_missing_body();
  }

  Identifier second_function_name = this->peek().identifier_name();
  if (second_function_name.normalized_name() !=
      function_name.normalized_name()) {
    if (semicolon_span.has_value()) {
      // function f(); function g() {}
      // The caller will report
      // Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name. Do
      // nothing special here.
    } else {
      // function f()  // ASI
      // function g() {}
      return roll_back_missing_body();
    }
  }

  this->skip();
  this->lexer_.commit_transaction(std::move(transaction));
  if (has_newline_after_async_keyword) {
    QLJS_ASSERT(async_keyword.has_value());
    this->diags_.add(
        Diag_Newline_Not_Allowed_Between_Async_And_Function_Keyword{
            .async_keyword = *async_keyword,
            .function_keyword = function_keyword,
        });
  }
  return Overload_Signature_Parse_Result{
      .is_overload_signature = true,
      .has_missing_body_error = false,
      .second_function_name = second_function_name,
      .second_function_attributes = second_function_attributes,
      .second_function_generator_star = second_function_generator_star,
      .second_function_export_keyword = second_function_export_keyword,
      .second_function_expected_export = second_function_expected_export,
  };
}

void Parser::parse_and_visit_decorator(Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::at);
  this->skip();

  switch (this->peek().type) {
  // @myDecorator
  // @myDecorator()
  // @myNamespace.myDecorator
  // TODO(strager): Only allow 'await' in non-async functions.
  // TODO(strager): Only allow 'yield' in non-generator functions.
  case Token_Type::kw_await:
  case Token_Type::kw_yield:
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case Token_Type::identifier: {
    Expression *ast = this->make_expression<Expression::Variable>(
        this->peek().identifier_name(), this->peek().type);
    this->skip();

    while (this->peek().type == Token_Type::dot) {
      Source_Code_Span op_span_ = this->peek().span();
      this->skip();
      switch (this->peek().type) {
      // @myNamespace.myDecorator
      QLJS_CASE_KEYWORD:
      case Token_Type::identifier:
      case Token_Type::private_identifier:
        ast = this->make_expression<Expression::Dot>(
            ast, this->peek().identifier_name(), op_span_);
        this->skip();
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }

    if (this->peek().type == Token_Type::left_paren) {
      // @decorator()
      // @decorator(arg1, arg2)
      ast = this->parse_call_expression_remainder(v, ast, std::nullopt);
    }

    this->visit_expression(ast, v, Variable_Context::rhs);
    break;
  }

  // @(myDecorator)
  case Token_Type::left_paren:
    this->skip();
    this->parse_and_visit_expression(v);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_paren);
    this->skip();
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

void Parser::parse_and_visit_one_or_more_decorators(Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::at);
  do {
    this->parse_and_visit_decorator(v);
  } while (this->peek().type == Token_Type::at);
}

void Parser::parse_and_visit_decorator_statement(Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::at);
  Source_Code_Span decorator_at = this->peek().span();
  Stacked_Buffering_Visitor decorator_visits =
      this->buffering_visitor_stack_.push();
  this->parse_and_visit_one_or_more_decorators(decorator_visits.visitor());

  switch (this->peek().type) {
  case Token_Type::kw_abstract:
  case Token_Type::kw_class:
    this->parse_and_visit_class(
        v, Parse_Class_Options{
               .require_name = Name_Requirement::required_for_statement,
               .abstract_keyword_span = std::nullopt,
           });
    break;

  case Token_Type::kw_export:
    this->parse_and_visit_export(v, Parse_Export_Options{
                                        .decorator_at_span = decorator_at,
                                    });
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
  // NOTE[class-decorator-deferred-visits]: Decorators are executed after the
  // class's definition.
  decorator_visits.visitor().move_into(v);
}

void Parser::parse_and_visit_switch(Parse_Visitor_Base &v) {
  Switch_Guard s = this->enter_switch();

  QLJS_ASSERT(this->peek().type == Token_Type::kw_switch);
  Source_Code_Span switch_token_span = this->peek().span();
  this->skip();

  if (this->peek().type == Token_Type::left_curly) {
    // switch { case 1: break; }  // Invalid.
    this->diags_.add(Diag_Missing_Condition_For_Switch_Statement{
        .switch_keyword = switch_token_span,
    });
  } else {
    this->parse_and_visit_parenthesized_expression<
        Diag_Expected_Parentheses_Around_Switch_Condition,
        Diag_Expected_Parenthesis_Around_Switch_Condition,
        /*CheckForSketchyConditions=*/false,
        /*CheckForCommaOperator=*/true>(v, switch_token_span);
  }

  switch (this->peek().type) {
  case Token_Type::left_curly:
    this->skip();
    break;

  case Token_Type::kw_case:
  case Token_Type::kw_default:
    this->diags_.add(Diag_Expected_Left_Curly{
        .expected_left_curly =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
    });
    break;

  default:
    this->diags_.add(Diag_Missing_Body_For_Switch_Statement{
        .switch_and_condition =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
    });
    return;
  }
  v.visit_enter_block_scope();

  bool keep_going = true;
  bool is_before_first_switch_case = true;
  std::optional<Token> previous_statement_first_token;
  Hash_Set<String8_View> cases;
  auto is_valid_end_of_case = [](Token_Type tk) {
    switch (tk) {
    case Token_Type::kw_return:
    case Token_Type::kw_continue:
    case Token_Type::kw_throw:
    case Token_Type::kw_break:
    case Token_Type::kw_case:

    // Temporarily return true to omit diag with these statments
    case Token_Type::kw_do:
    case Token_Type::kw_for:
    case Token_Type::kw_if:
    case Token_Type::kw_switch:
    case Token_Type::kw_try:
    case Token_Type::kw_while:
    case Token_Type::kw_with:
    case Token_Type::left_curly:
      return true;
    default:
      return false;
    }
  };
  while (keep_going) {
    switch (this->peek().type) {
    case Token_Type::right_curly:
      this->skip();
      keep_going = false;
      break;

    case Token_Type::kw_case: {
      if (!is_before_first_switch_case &&
          !is_valid_end_of_case(previous_statement_first_token->type) &&
          !this->peek().has_leading_comment) {
        this->diags_.add(Diag_Fallthrough_Without_Comment_In_Switch{
            .end_of_case = Source_Code_Span::unit(this->peek().begin)});
      }
      previous_statement_first_token = this->peek();
      is_before_first_switch_case = false;
      Source_Code_Span case_token_span = this->peek().span();
      this->skip();
      if (this->peek().type == Token_Type::colon) {
        this->diags_.add(Diag_Expected_Expression_For_Switch_Case{
            .case_token = case_token_span,
        });
        this->skip();
      } else {
        Expression *ast = this->parse_expression(
            v, Precedence{
                   .colon_type_annotation = Allow_Type_Annotations::never,
               });

        Source_Code_Span expression_case_span = ast->span();
        auto [it, inserted] = cases.insert(expression_case_span.string_view());
        if (!inserted) {
          this->diags_.add(Diag_Duplicated_Cases_In_Switch_Statement{
              .first_switch_case =
                  Source_Code_Span(it->data(), it->data() + it->size()),
              .duplicated_switch_case = expression_case_span});
        }
        this->visit_expression(ast, v, Variable_Context::rhs);
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::colon);
        this->skip();
      }
      break;
    }

    case Token_Type::kw_default:
      if (!is_before_first_switch_case &&
          !is_valid_end_of_case(previous_statement_first_token->type) &&
          !this->peek().has_leading_comment) {
        this->diags_.add(Diag_Fallthrough_Without_Comment_In_Switch{
            .end_of_case = Source_Code_Span::unit(this->peek().begin)});
      }
      is_before_first_switch_case = false;
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::colon);
      this->skip();
      break;

    default: {
      if (is_before_first_switch_case) {
        this->diags_.add(Diag_Statement_Before_First_Switch_Case{
            .unexpected_statement = this->peek().span(),
        });
      }
      previous_statement_first_token = this->peek();
      bool parsed_statement = this->parse_and_visit_statement(
          v, Parse_Statement_Options{
                 .possibly_followed_by_another_statement = true,
             });
      if (!parsed_statement) {
        QLJS_PARSER_UNIMPLEMENTED();
      }
      break;
    }
    }
  }

  v.visit_exit_block_scope();
}

void Parser::parse_and_visit_typescript_namespace(
    Parse_Visitor_Base &v, std::optional<Source_Code_Span> export_keyword_span,
    Source_Code_Span namespace_keyword_span) {
  std::optional<Identifier> namespace_declaration =
      this->parse_and_visit_typescript_namespace_or_module_head(
          v,
          /*export_keyword_span=*/export_keyword_span,
          /*declare_keyword_span=*/std::nullopt, namespace_keyword_span);

  bool is_nested_namespace =
      this->in_typescript_namespace_or_module_.has_value();
  if (!is_nested_namespace) {
    this->is_current_typescript_namespace_non_empty_ = false;
  }

  // FIXME(strager): is_module should be true if we performed error recovery to
  // treat 'module "foo"' as 'declare module "foo"'.
  bool is_module = false;
  TypeScript_Namespace_Or_Module_Guard namespace_guard =
      this->enter_typescript_namespace_or_module(namespace_keyword_span,
                                                 is_module);
  {
    if (this->peek().type != Token_Type::left_curly) {
      this->diags_.add(Diag_Missing_Body_For_TypeScript_Namespace{
          .expected_body =
              Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
      });
      goto done_parsing_body;
    }

    v.visit_enter_namespace_scope();
    this->parse_and_visit_statement_block_no_scope(
        v, Parse_Statement_Options{
               .possibly_followed_by_another_statement = true,
               .require_declaration = this->options_.typescript_definition_file,
           });
    v.visit_exit_namespace_scope();
  }

done_parsing_body:
  if (namespace_declaration.has_value()) {
    Variable_Declaration_Flags namespace_flags =
        this->is_current_typescript_namespace_non_empty_
            ? Variable_Declaration_Flags::non_empty_namespace
            : Variable_Declaration_Flags::none;
    v.visit_variable_declaration(*namespace_declaration,
                                 Variable_Kind::_namespace, namespace_flags);
  }

  if (!is_nested_namespace) {
    this->is_current_typescript_namespace_non_empty_ = false;
  }
}

std::optional<Identifier>
Parser::parse_and_visit_typescript_namespace_or_module_head(
    Parse_Visitor_Base &, std::optional<Source_Code_Span> export_keyword_span,
    std::optional<Source_Code_Span> declare_keyword_span,
    Source_Code_Span namespace_or_module_keyword_span) {
  if (this->peek().has_leading_newline) {
    this->diags_.add(Diag_Newline_Not_Allowed_After_Namespace_Keyword{
        .namespace_keyword = namespace_or_module_keyword_span,
    });
  }
  if (!this->options_.typescript) {
    this->diags_.add(Diag_TypeScript_Namespaces_Not_Allowed_In_JavaScript{
        .namespace_keyword = namespace_or_module_keyword_span,
    });
  }

  switch (this->peek().type) {
  // namespace ns { }
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case Token_Type::identifier: {
    if (this->options_.typescript_definition_file &&
        !this->in_typescript_namespace_or_module_ &&
        !declare_keyword_span.has_value() && !export_keyword_span.has_value()) {
      this->diags_.add(Diag_DTS_Missing_Declare_Or_Export{
          .expected =
              Source_Code_Span::unit(namespace_or_module_keyword_span.begin()),
          .declaring_token = namespace_or_module_keyword_span,
      });
    }

    Identifier namespace_identifier = this->peek().identifier_name();
    this->skip();
    while (this->peek().type == Token_Type::dot) {
      this->skip();
      switch (this->peek().type) {
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
      case Token_Type::identifier:
      case Token_Type::kw_await:
      case Token_Type::kw_yield:
        this->skip();
        break;
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
    return namespace_identifier;
  }

  // module 'my namespace' { }
  // namespace 'ns' { }         // Invalid.
  case Token_Type::string: {
    bool namespace_keyword_is_module =
        namespace_or_module_keyword_span.string_view()[0] == u8'm';
    if (!namespace_keyword_is_module || !declare_keyword_span.has_value() ||
        export_keyword_span.has_value()) {
      this->diags_.add(
          Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module{
              .module_name = this->peek().span(),
          });
    } else if (this->in_typescript_namespace_or_module_ &&
               !this->in_typescript_module_) {
      this->diags_.add(Diag_String_Namespace_Name_Not_Allowed_In_Namespace{
          .module_name = this->peek().span(),
      });
    }
    this->skip();
    return std::nullopt;
  }

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    return std::nullopt;
  }
}

void Parser::parse_and_visit_typescript_declare_namespace_or_module(
    Parse_Visitor_Base &v, Source_Code_Span declare_keyword_span) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_module ||
              this->peek().type == Token_Type::kw_namespace);
  Source_Code_Span namespace_keyword_span = this->peek().span();
  this->skip();
  std::optional<Identifier> namespace_declaration =
      this->parse_and_visit_typescript_namespace_or_module_head(
          v,
          /*export_keyword_span=*/std::nullopt,
          /*declare_keyword_span=*/declare_keyword_span,
          namespace_keyword_span);

  // True if 'module "modulename" { ... }'.
  bool is_module = !namespace_declaration.has_value();
  TypeScript_Declare_Context declare_context{
      .declare_namespace_declare_keyword = declare_keyword_span,
      .in_module = is_module,
  };

  TypeScript_Namespace_Or_Module_Guard namespace_guard =
      this->enter_typescript_namespace_or_module(namespace_keyword_span,
                                                 is_module);
  if (this->peek().type != Token_Type::left_curly) {
    // module 'foo';
    // namespace ns;  // Invalid.
    if (declare_context.in_module) {
      this->consume_semicolon_after_statement();
    } else {
      this->diags_.add(Diag_Missing_Body_For_TypeScript_Namespace{
          .expected_body =
              Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
      });
    }
    goto done_parsing_body;
  }

  v.visit_enter_namespace_scope();
  this->parse_and_visit_typescript_declare_block(v, declare_context);
  v.visit_exit_namespace_scope();

done_parsing_body:
  if (namespace_declaration.has_value()) {
    v.visit_variable_declaration(*namespace_declaration,
                                 Variable_Kind::_namespace,
                                 Variable_Declaration_Flags::none);
  }
}

void Parser::parse_and_visit_typescript_declare_block(
    Parse_Visitor_Base &v, const TypeScript_Declare_Context &declare_context) {
  QLJS_ASSERT(this->peek().type == Token_Type::left_curly);
  Source_Code_Span left_curly_span = this->peek().span();
  this->skip();

  for (;;) {
    switch (this->peek().type) {
    case Token_Type::kw_export:
      this->is_current_typescript_namespace_non_empty_ = true;
      this->parse_and_visit_export(v, Parse_Export_Options{
                                          .declare_context = declare_context,
                                      });
      break;

    case Token_Type::right_curly:
      this->skip();
      return;

    case Token_Type::end_of_file:
      this->diags_.add(Diag_Unclosed_Code_Block{
          .block_open = left_curly_span,
      });
      return;

    case Token_Type::kw_declare: {
      this->is_current_typescript_namespace_non_empty_ = true;
      this->diags_.add(
          Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace{
              .declare_keyword = this->peek().span(),
              .declare_namespace_declare_keyword =
                  declare_context.declare_keyword_span(),
          });
      bool parsed_statement = this->parse_and_visit_statement(
          v, Parse_Statement_Options{
                 .possibly_followed_by_another_statement = true,
             });
      QLJS_ASSERT(parsed_statement);
      break;
    }

    default:
      if (this->is_declare_statement_start_token(this->peek().type)) {
        this->is_current_typescript_namespace_non_empty_ = true;
        this->parse_and_visit_declare_statement(v, declare_context);
      } else {
        // require_declaration will cause parse_and_visit_statement to report
        // Diag_Declare_Namespace_Cannot_Contain_Statement.
        [[maybe_unused]] bool parsed_statement =
            this->parse_and_visit_statement(
                v,
                Parse_Statement_Options{
                    .possibly_followed_by_another_statement = true,
                    .require_declaration = true,
                    .declare_keyword = declare_context.declare_keyword_span(),
                });
      }
      break;

    case Token_Type::regexp:
      QLJS_UNREACHABLE();
      break;
    }
  }
}

void Parser::parse_and_visit_typescript_type_alias(
    Parse_Visitor_Base &v, Source_Code_Span type_token) {
  if (this->peek().has_leading_newline) {
    this->diags_.add(Diag_Newline_Not_Allowed_After_Type_Keyword{
        .type_keyword = type_token,
    });
  }
  if (!this->options_.typescript) {
    this->diags_.add(Diag_TypeScript_Type_Alias_Not_Allowed_In_JavaScript{
        .type_keyword = type_token,
    });
  }
  Identifier name = this->peek().identifier_name();
  Variable_Kind kind = Variable_Kind::_type_alias;
  v.visit_variable_declaration(name, kind, Variable_Declaration_Flags::none);
  this->skip();

  v.visit_enter_type_scope();
  if (this->peek().type == Token_Type::less) {
    this->parse_and_visit_typescript_generic_parameters(v);
  }
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::equal);
  this->skip();
  this->parse_and_visit_typescript_type_expression_no_scope(
      v, TypeScript_Type_Parse_Options{
             .type_being_declared =
                 TypeScript_Type_Parse_Options::Declaring_Type{
                     .name = name,
                     .kind = kind,
                 },
             .stop_parsing_type_at_newline_before_extends = false,
         });
  v.visit_exit_type_scope();

  this->consume_semicolon_after_statement();
}

void Parser::parse_and_visit_typescript_enum(Parse_Visitor_Base &v,
                                             Enum_Kind kind) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_enum);
  if (!this->options_.typescript) {
    this->diags_.add(Diag_TypeScript_Enum_Is_Not_Allowed_In_JavaScript{
        .enum_keyword = this->peek().span(),
    });
  }
  this->skip();

  switch (kind) {
  case Enum_Kind::declare_enum:
  case Enum_Kind::normal:
    this->is_current_typescript_namespace_non_empty_ = true;
    break;
  case Enum_Kind::const_enum:
  case Enum_Kind::declare_const_enum:
    break;
  }

  switch (this->peek().type) {
  case Token_Type::kw_abstract:
  case Token_Type::kw_accessor:
  case Token_Type::kw_as:
  case Token_Type::kw_assert:
  case Token_Type::kw_asserts:
  case Token_Type::kw_async:
  case Token_Type::kw_constructor:
  case Token_Type::kw_declare:
  case Token_Type::kw_from:
  case Token_Type::kw_get:
  case Token_Type::kw_global:
  case Token_Type::kw_infer:
  case Token_Type::kw_intrinsic:
  case Token_Type::kw_is:
  case Token_Type::kw_keyof:
  case Token_Type::kw_module:
  case Token_Type::kw_namespace:
  case Token_Type::kw_of:
  case Token_Type::kw_out:
  case Token_Type::kw_override:
  case Token_Type::kw_readonly:
  case Token_Type::kw_require:
  case Token_Type::kw_satisfies:
  case Token_Type::kw_set:
  case Token_Type::kw_type:
  case Token_Type::kw_unique:
  case Token_Type::identifier:
    break;

  case Token_Type::kw_await:
    if (this->in_async_function_) {
      this->diags_.add(Diag_Cannot_Declare_Await_In_Async_Function{
          .name = this->peek().span(),
      });
    }
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  v.visit_variable_declaration(this->peek().identifier_name(),
                               Variable_Kind::_enum,
                               Variable_Declaration_Flags::none);
  this->skip();

  v.visit_enter_enum_scope();
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::left_curly);
  this->skip();
  this->parse_and_visit_typescript_enum_members(v, kind);
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_curly);
  this->skip();
  v.visit_exit_enum_scope();
}

void Parser::parse_and_visit_typescript_enum_members(Parse_Visitor_Base &v,
                                                     Enum_Kind kind) {
  std::optional<Enum_Value_Kind> last_enum_value_kind;
  std::optional<Source_Code_Span> last_enum_value;

  auto auto_member = [&](Source_Code_Span member_name) {
    if (kind == Enum_Kind::normal &&
        last_enum_value_kind == Enum_Value_Kind::computed) {
      QLJS_ASSERT(last_enum_value.has_value());
      this->diags_.add(
          Diag_TypeScript_Enum_Auto_Member_Needs_Initializer_After_Computed{
              .auto_member_name = member_name,
              .computed_expression = *last_enum_value,
          });
    }
    last_enum_value_kind = std::nullopt;
  };

  auto parse_after_member_name = [&](Source_Code_Span name) {
    switch (this->peek().type) {
    // enum E { A, B }
    case Token_Type::comma:
      auto_member(name);
      this->skip();
      break;

    // enum E { A }
    case Token_Type::right_curly:
      auto_member(name);
      break;

    // enum E { A = 1 }
    case Token_Type::equal: {
      this->skip();

      Expression *ast = this->parse_expression(v, Precedence{.commas = false});
      this->visit_expression(ast, v, Variable_Context::rhs);
      Source_Code_Span ast_span = ast->span();

      Enum_Value_Kind value_kind = this->classify_enum_value_expression(ast);
      last_enum_value_kind = value_kind;
      last_enum_value = ast_span;
      switch (kind) {
      case Enum_Kind::declare_const_enum:
      case Enum_Kind::const_enum:
      case Enum_Kind::declare_enum: {
        if (value_kind == Enum_Value_Kind::computed) {
          this->diags_.add(Diag_TypeScript_Enum_Value_Must_Be_Constant{
              .expression = ast_span,
              .declared_enum_kind = kind,
          });
        }
        break;
      }
      case Enum_Kind::normal:
        break;
      }

      if (this->peek().type == Token_Type::comma) {
        // enum E { A = 1, }
        this->skip();
      }
      break;
    }

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  };

next_member:
  switch (this->peek().type) {
  // enum E { A }
  // enum E { A, }
  // enum E { A = 1 }
  // enum E { const = 69 }
  // enum E { "member" }
  QLJS_CASE_KEYWORD:
  case Token_Type::identifier:
  case Token_Type::string: {
    Source_Code_Span member_name = this->peek().span();
    this->skip();
    parse_after_member_name(member_name);
    goto next_member;
  }

  // enum E { ["member"] }
  // enum E { ["member"] = 42 }
  case Token_Type::left_square: {
    const Char8 *name_begin = this->peek().begin;
    this->skip();

    Expression *ast = this->parse_expression(v);
    switch (ast->kind()) {
    // TODO(#758): Error on number literals.
    case Expression_Kind::Literal:
      break;
    default:
      this->diags_.add(Diag_TypeScript_Enum_Computed_Name_Must_Be_Simple{
          .expression = ast->span(),
      });
      break;
    }
    this->visit_expression(ast, v, Variable_Context::rhs);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_square);
    const Char8 *name_end = this->peek().end;
    this->skip();

    parse_after_member_name(Source_Code_Span(name_begin, name_end));
    goto next_member;
  }

  // enum E { 42 = 69 }  // Invalid.
  case Token_Type::number: {
    Source_Code_Span member_name = this->peek().span();
    this->diags_.add(Diag_TypeScript_Enum_Member_Name_Cannot_Be_Number{
        .number = member_name,
    });
    this->skip();
    parse_after_member_name(member_name);
    goto next_member;
  }

  // enum E { A }
  case Token_Type::right_curly:
    return;

  // enum E { , }    // Invalid.
  // enum E { A,, }  // Invalid.
  case Token_Type::comma:
    this->diags_.add(Diag_Extra_Comma_Not_Allowed_Between_Enum_Members{
        .comma = this->peek().span(),
    });
    this->skip();
    goto next_member;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

Parser::Enum_Value_Kind Parser::classify_enum_value_expression(
    const Expression *ast) {
  auto visit_children = [&]() -> Enum_Value_Kind {
    Enum_Value_Kind kind = Enum_Value_Kind::constant;
    for (Expression *child : ast->children()) {
      Enum_Value_Kind child_kind = classify_enum_value_expression(child);
      switch (child_kind) {
      case Enum_Value_Kind::computed:
        if (kind != Enum_Value_Kind::unknown) {
          kind = Enum_Value_Kind::computed;
        }
        break;
      case Enum_Value_Kind::unknown:
        kind = Enum_Value_Kind::unknown;
        break;
      case Enum_Value_Kind::constant:
        break;
      }
    }
    return kind;
  };
  switch (ast->kind()) {
  case Expression_Kind::Call:
  case Expression_Kind::This_Variable:
    return Enum_Value_Kind::computed;

  case Expression_Kind::Literal:
    return Enum_Value_Kind::constant;

  case Expression_Kind::Binary_Operator:
  case Expression_Kind::Paren:
    return visit_children();

  case Expression_Kind::Class:
  case Expression_Kind::Delete:
  case Expression_Kind::Invalid:
  case Expression_Kind::Missing:
  case Expression_Kind::New:
  case Expression_Kind::Template:
  case Expression_Kind::Typeof:
  case Expression_Kind::Angle_Type_Assertion:
  case Expression_Kind::Array:
  case Expression_Kind::Arrow_Function:
  case Expression_Kind::As_Type_Assertion:
  case Expression_Kind::Assignment:
  case Expression_Kind::Await:
  case Expression_Kind::Compound_Assignment:
  case Expression_Kind::Conditional:
  case Expression_Kind::Conditional_Assignment:
  case Expression_Kind::Dot:
  case Expression_Kind::Function:
  case Expression_Kind::Import:
  case Expression_Kind::Index:
  case Expression_Kind::JSX_Element:
  case Expression_Kind::JSX_Element_With_Members:
  case Expression_Kind::JSX_Element_With_Namespace:
  case Expression_Kind::JSX_Fragment:
  case Expression_Kind::Named_Function:
  case Expression_Kind::New_Target:
  case Expression_Kind::Non_Null_Assertion:
  case Expression_Kind::Object:
  case Expression_Kind::Optional:
  case Expression_Kind::Paren_Empty:
  case Expression_Kind::Private_Variable:
  case Expression_Kind::RW_Unary_Prefix:
  case Expression_Kind::RW_Unary_Suffix:
  case Expression_Kind::Satisfies:
  case Expression_Kind::Spread:
  case Expression_Kind::Super:
  case Expression_Kind::Tagged_Template_Literal:
  case Expression_Kind::Trailing_Comma:
  case Expression_Kind::Type_Annotated:
  case Expression_Kind::Unary_Operator:
  case Expression_Kind::Variable:
  case Expression_Kind::Yield_Many:
  case Expression_Kind::Yield_None:
  case Expression_Kind::Yield_One:
    return Enum_Value_Kind::unknown;
  }
  QLJS_UNREACHABLE();
}

void Parser::parse_and_visit_try_maybe_catch_maybe_finally(
    Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_try);
  Source_Code_Span try_token_span = this->peek().span();
  this->skip();

  bool parsed_try_body = false;
  if (this->peek().type == Token_Type::left_curly) {
    parsed_try_body = true;
    v.visit_enter_block_scope();
    this->parse_and_visit_statement_block_no_scope(v);
    v.visit_exit_block_scope();
  } else {
    this->diags_.add(Diag_Missing_Body_For_Try_Statement{
        .try_token = try_token_span,
    });
  }

  bool parsed_catch_or_finally =
      this->parse_and_visit_catch_or_finally_or_both(v);
  if (parsed_try_body && !parsed_catch_or_finally) {
    const Char8 *expected_catch_or_finally =
        this->lexer_.end_of_previous_token();
    this->diags_.add(Diag_Missing_Catch_Or_Finally_For_Try_Statement{
        .expected_catch_or_finally = Source_Code_Span(
            expected_catch_or_finally, expected_catch_or_finally),
        .try_token = try_token_span,
    });
  }
}

bool Parser::parse_and_visit_catch_or_finally_or_both(Parse_Visitor_Base &v) {
  bool parsed_catch = false;
  bool parsed_finally = false;

  if (this->peek().type == Token_Type::kw_catch) {
    parsed_catch = true;
    this->skip();

    v.visit_enter_block_scope();
    if (this->peek().type == Token_Type::left_paren) {
      Source_Code_Span catch_left_paren_span = this->peek().span();
      this->skip();

      switch (this->peek().type) {
      case Token_Type::kw_await:
        if (this->in_async_function_) {
          this->diags_.add(Diag_Cannot_Declare_Await_In_Async_Function{
              .name = this->peek().span(),
          });
        }
        goto catch_identifier;

      case Token_Type::kw_yield:
        if (this->in_generator_function_) {
          this->diags_.add(Diag_Cannot_Declare_Yield_In_Generator_Function{
              .name = this->peek().span(),
          });
        }
        goto catch_identifier;

      QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
        // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
        goto catch_identifier;

      catch_identifier:
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case Token_Type::identifier:
        v.visit_variable_declaration(this->peek().identifier_name(),
                                     Variable_Kind::_catch,
                                     Variable_Declaration_Flags::none);
        this->skip();
        break;

      case Token_Type::left_curly:
      case Token_Type::left_square: {
        Expression *ast = this->parse_expression(
            v, Precedence{.commas = false, .in_operator = false});
        this->visit_binding_element(
            ast, v,
            Binding_Element_Info{
                .declaration_kind = Variable_Kind::_catch,
                .declaring_token = std::nullopt,
                .flags = Variable_Declaration_Flags::none,
            });
        break;
      }

      case Token_Type::right_paren:
        this->diags_.add(Diag_Missing_Catch_Variable_Between_Parentheses{
            .left_paren_to_right_paren = Source_Code_Span(
                catch_left_paren_span.begin(), this->peek().end),
            .left_paren = catch_left_paren_span,
            .right_paren = this->peek().span(),
        });
        break;

        // catch ("junk") {}
      case Token_Type::string:
        this->diags_.add(Diag_Expected_Variable_Name_For_Catch{
            .unexpected_token = this->peek().span(),
        });
        this->skip();
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }

      if (this->peek().type == Token_Type::colon) {
        // catch (e: Type)  // TypeScript only.
        this->parse_typescript_colon_for_type();
        switch (this->peek().type) {
        // catch (e: *)
        // catch (e: any)
        // catch (e: unknown)
        case Token_Type::kw_any:
        case Token_Type::kw_unknown:
        case Token_Type::star:
          this->skip();
          break;

        default: {
          const Char8 *type_expression_begin = this->peek().begin;
          this->parse_and_visit_typescript_type_expression_no_scope(
              Null_Visitor::instance);
          const Char8 *type_expression_end =
              this->lexer_.end_of_previous_token();
          if (this->options_.typescript) {
            this->diags_.add(Diag_TypeScript_Catch_Type_Annotation_Must_Be_Any{
                .type_expression = Source_Code_Span(type_expression_begin,
                                                    type_expression_end),
            });
          }
          break;
        }
        }
      }

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_paren);
      this->skip();
    }

    if (this->peek().type == Token_Type::left_curly) {
      this->parse_and_visit_statement_block_no_scope(v);
    } else {
      this->diags_.add(Diag_Missing_Body_For_Catch_Clause{
          .catch_token =
              Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
      });
    }
    v.visit_exit_block_scope();
  }

  if (this->peek().type == Token_Type::kw_finally) {
    parsed_finally = true;
    Source_Code_Span finally_token_span = this->peek().span();
    this->skip();

    if (this->peek().type == Token_Type::left_curly) {
      v.visit_enter_block_scope();
      this->parse_and_visit_statement_block_no_scope(v);
      v.visit_exit_block_scope();
    } else {
      this->diags_.add(Diag_Missing_Body_For_Finally_Clause{
          .finally_token = finally_token_span,
      });
    }
  }

  return parsed_catch || parsed_finally;
}

void Parser::parse_and_visit_do_while(Parse_Visitor_Base &v) {
  Loop_Guard guard = this->enter_loop();

  QLJS_ASSERT(this->peek().type == Token_Type::kw_do);
  Source_Code_Span do_token_span = this->peek().span();
  this->skip();

  bool body_is_while = this->peek().type == Token_Type::kw_while;

  this->error_on_class_statement(Statement_Kind::do_while_loop);
  this->error_on_function_statement(Statement_Kind::do_while_loop);
  this->error_on_lexical_declaration(Statement_Kind::do_while_loop);
  bool parsed_statement = this->parse_and_visit_statement(v);
  if (!parsed_statement) {
    QLJS_PARSER_UNIMPLEMENTED();
  }

  if (this->peek().type != Token_Type::kw_while) {
    if (body_is_while) {
      // { do while (cond); }  // Invalid
      this->diags_.add(Diag_Missing_Body_For_Do_While_Statement{
          .do_token = do_token_span,
      });
    } else {
      // do { } if (cond);  // Invalid
      this->diags_.add(Diag_Missing_While_And_Condition_For_Do_While_Statement{
          .do_token = do_token_span,
          .expected_while =
              Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
      });
    }
    return;
  }
  this->skip();

  this->parse_and_visit_parenthesized_expression<
      Diag_Expected_Parentheses_Around_Do_While_Condition,
      Diag_Expected_Parenthesis_Around_Do_While_Condition,
      /*CheckForSketchyConditions=*/true,
      /*CheckForCommaOperator=*/true>(v, do_token_span);

  if (this->peek().type == Token_Type::semicolon) {
    this->skip();
  }
}

void Parser::parse_and_visit_for(Parse_Visitor_Base &v) {
  Loop_Guard guard = this->enter_loop();

  QLJS_ASSERT(this->peek().type == Token_Type::kw_for);
  Source_Code_Span for_token_span = this->peek().span();
  this->skip();

  bool is_for_await = this->peek().type == Token_Type::kw_await;
  if (is_for_await) {
    if (this->in_top_level_) {
      // TODO: Emit a diagnostic if the top level await mode does not allow
      // await operators (Parser_Top_Level_Await_Mode::no_await_operator).
    } else {
      if (!this->in_async_function_) {
        this->diags_.add(
            Diag_Await_Operator_Outside_Async{this->peek().span()});
      }
    }
    this->skip();
  }

  if (this->peek().type != Token_Type::left_paren) {
    this->diags_.add(Diag_Missing_For_Loop_Header{
        .for_token = for_token_span,
    });
    return;
  }
  const Char8 *left_paren_token_begin = this->peek().begin;
  this->skip();

  std::optional<Expression *> after_expression;
  auto parse_c_style_head_remainder =
      [&](Source_Code_Span first_semicolon_span) {
        if (this->peek().type != Token_Type::semicolon) {
          Expression *ast = this->parse_expression(v);
          this->visit_expression(ast, v, Variable_Context::rhs);
          this->error_on_sketchy_condition(ast);
          this->warn_on_comma_operator_in_conditional_statement(ast);
        }

        switch (this->peek().type) {
          // for (init; cond; update) {}
        semicolon:
        case Token_Type::semicolon:
          this->skip();
          if (this->peek().type != Token_Type::right_paren) {
            after_expression = this->parse_expression(v);
          }
          break;

          // for (init; cond update) {}  // Invalid.
        case Token_Type::identifier:
        default:
          this->lexer_.insert_semicolon();
          this->diags_.add(
              Diag_Missing_Semicolon_Between_For_Loop_Condition_And_Update{
                  .expected_semicolon = this->peek().span(),
              });
          goto semicolon;

          // for (init; cond) {}  // Invalid.
        case Token_Type::right_paren:
          this->diags_.add(Diag_C_Style_For_Loop_Is_Missing_Third_Component{
              .expected_last_component = this->peek().span(),
              .existing_semicolon = first_semicolon_span,
          });
          break;
        }
      };

  bool entered_for_scope = false;
  enum class Loop_Style {
    c_style,
    for_in,
    for_of,
    other,
  };
  Loop_Style for_loop_style;

  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wshadow-local")
  auto parse_in_or_of_or_condition_update =
      [&, this](auto &v, Expression *init_expression) -> void {
    QLJS_WARNING_POP
    switch (this->peek().type) {
      // for (init; condition; update) {}
    semicolon:
    case Token_Type::semicolon: {
      Source_Code_Span first_semicolon_span = this->peek().span();
      this->skip();
      this->visit_expression(init_expression, v, Variable_Context::rhs);
      for_loop_style = Loop_Style::c_style;
      parse_c_style_head_remainder(first_semicolon_span);
      break;
    }

      // for (lhs rhs) {}                 // Invalid.
      // for (init condition; update) {}  // Invalid.
    case Token_Type::identifier:
    default:
      this->lexer_.insert_semicolon();
      this->diags_.add(
          Diag_Missing_Semicolon_Between_For_Loop_Init_And_Condition{
              .expected_semicolon = this->peek().span(),
          });
      goto semicolon;

      // for (lhs in rhs) {}
      // for (lhs in rhs; condition; update) {}  // Invalid.
    case Token_Type::kw_in: {
      Source_Code_Span in_token_span = this->peek().span();
      this->skip();

      Expression *rhs = this->parse_expression(v);
      this->visit_assignment_expression(init_expression, rhs, v);

      if (this->peek().type == Token_Type::semicolon) {
        this->diags_.add(Diag_In_Disallowed_In_C_Style_For_Loop{
            .in_token = in_token_span,
        });
        Source_Code_Span first_semicolon_span = this->peek().span();
        this->skip();
        for_loop_style = Loop_Style::for_in;
        parse_c_style_head_remainder(first_semicolon_span);
      }
      break;
    }

      // for (lhs of rhs) {}
    case Token_Type::kw_of: {
      this->skip();
      Expression *rhs = this->parse_expression(v);
      this->visit_assignment_expression(init_expression, rhs, v);
      for_loop_style = Loop_Style::for_of;
      break;
    }

      // for (expression) {}    // Invalid.
    case Token_Type::right_paren:
      this->diags_.add(Diag_Missing_For_Loop_Rhs_Or_Components_After_Expression{
          .header = Source_Code_Span(left_paren_token_begin, this->peek().end),
          .for_token = for_token_span,
      });
      this->visit_expression(init_expression, v, Variable_Context::rhs);
      for_loop_style = Loop_Style::c_style;
      break;
    }
  };
  switch (this->peek().type) {
    // for (;;) {}
  case Token_Type::semicolon: {
    Source_Code_Span first_semicolon_span = this->peek().span();
    this->skip();
    for_loop_style = Loop_Style::c_style;
    parse_c_style_head_remainder(first_semicolon_span);
    break;
  }

    // for (let i = 0; i < length; ++length) {}
    // for (let x of xs) {}
    // for (let in xs) {}
  case Token_Type::kw_const:
  case Token_Type::kw_let:
    v.visit_enter_for_scope();
    entered_for_scope = true;
    [[fallthrough]];
  case Token_Type::kw_var: {
    Token declaring_token = this->peek();

    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    Stacked_Buffering_Visitor lhs = this->buffering_visitor_stack_.push();
    if (declaring_token.type == Token_Type::kw_let &&
        this->is_let_token_a_variable_reference(this->peek(),
                                                /*allow_declarations=*/true)) {
      // for (let = expression; cond; up) {}
      // for (let(); cond; up) {}
      // for (let; cond; up) {}
      // for (let in myArray) {}
      this->lexer_.roll_back_transaction(std::move(transaction));
      Expression *ast =
          this->parse_expression(v, Precedence{.in_operator = false});
      this->visit_expression(ast, lhs.visitor(), Variable_Context::lhs);
      this->maybe_visit_assignment(ast, lhs.visitor(),
                                   Variable_Assignment_Flags::none);
    } else if (declaring_token.type == Token_Type::kw_let &&
               this->peek().type == Token_Type::kw_of) {
      this->skip();
      switch (this->peek().type) {
        // for (let of xs) {}  // Invalid.
      case Token_Type::identifier:
        this->lexer_.roll_back_transaction(std::move(transaction));
        this->skip();  // Re-parse 'let'.
        this->diags_.add(Diag_Let_With_No_Bindings{
            .where = declaring_token.span(),
        });
        break;

        // for (let of of xs) {}
        // for (let of in xs) {}
        // for (let of = 3; cond; update) {}
        // for (let of; cond; update) {}
        // for (let of, x; cond; update) {}
      default:
        this->lexer_.roll_back_transaction(std::move(transaction));
        this->skip();  // Re-parse 'let'.
        this->parse_and_visit_let_bindings(
            lhs.visitor(), Parse_Let_Bindings_Options{
                               .declaring_token = declaring_token,
                               .allow_in_operator = false,
                               .allow_const_without_initializer = false,
                               .is_in_for_initializer = true,
                           });
        break;
      }
    } else {
      // for (let i = 0; i < length; ++length) {}
      // for (let x of xs) {}
      this->lexer_.commit_transaction(std::move(transaction));
      this->parse_and_visit_let_bindings(
          lhs.visitor(), Parse_Let_Bindings_Options{
                             .declaring_token = declaring_token,
                             .allow_in_operator = false,
                             .allow_const_without_initializer = true,
                             .is_in_for_initializer = true,
                         });
    }
    switch (this->peek().type) {
      // for (let i = 0; i < length; ++length) {}
    case Token_Type::semicolon: {
      Source_Code_Span first_semicolon_span = this->peek().span();
      this->skip();
      lhs.visitor().move_into(v);
      for_loop_style = Loop_Style::c_style;
      parse_c_style_head_remainder(first_semicolon_span);
      break;
    }

      // for (let x of xs) {}
    case Token_Type::kw_in:
    case Token_Type::kw_of: {
      for_loop_style = this->peek().type == Token_Type::kw_in
                           ? Loop_Style::for_in
                           : Loop_Style::for_of;
      bool is_var_in = declaring_token.type == Token_Type::kw_var &&
                       for_loop_style == Loop_Style::for_in;
      this->skip();
      Expression *rhs = this->parse_expression(v);
      if (is_var_in) {
        // In the following code, 'init' is evaluated before 'array':
        //
        //   for (var x = init in array) {}
        lhs.visitor().move_into(v);
      }
      this->visit_expression(rhs, v, Variable_Context::rhs);
      if (!is_var_in) {
        // In the following code, 'array' is evaluated before 'x' is declared:
        //
        //   for (let x in array) {}
        lhs.visitor().move_into(v);
      }
      break;
    }

      // for (let myVariable) {}    // Invalid.
    case Token_Type::right_paren:
      this->diags_.add(
          Diag_Missing_For_Loop_Rhs_Or_Components_After_Declaration{
              .header =
                  Source_Code_Span(left_paren_token_begin, this->peek().end),
              .for_token = for_token_span,
          });
      lhs.visitor().move_into(v);
      for_loop_style = Loop_Style::for_of;
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }

    break;
  }

    // for (async; condition; update) {}
    // for (async.prop; condition; update) {}
    // for (async in things) {}
    // for (async.prop of things) {}
    // for (async of => {}; condition; update) {}
    // for (async of things) {}  // Invalid.
  case Token_Type::kw_async: {
    Token async_token = this->peek();

    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    bool is_async_of = false;
    this->skip();
    if (this->peek().type == Token_Type::kw_of) {
      this->skip();
      if (this->peek().type != Token_Type::equal_greater) {
        is_async_of = true;
      }
    }
    this->lexer_.roll_back_transaction(std::move(transaction));

    Expression *init_expression(nullptr);
    if (is_async_of) {
      // for await (async of things) {}
      // for (async of things) {}  // Invalid.
      if (!is_for_await) {
        this->diags_.add(
            Diag_Cannot_Assign_To_Variable_Named_Async_In_For_Of_Loop{
                .async_identifier = async_token.span(),
            });
      }

      this->skip();
      QLJS_ASSERT(this->peek().type == Token_Type::kw_of);
      init_expression = this->make_expression<Expression::Variable>(
          async_token.identifier_name(), async_token.type);
    } else {
      init_expression =
          this->parse_expression(v, Precedence{.in_operator = false});
    }
    parse_in_or_of_or_condition_update(v, init_expression);
    break;
  }

    // for (init; condition; update) {}
    // for (item of things) {}
    // for (item in things) {}
  default: {
    Expression *init_expression =
        this->parse_expression(v, Precedence{.in_operator = false});
    parse_in_or_of_or_condition_update(v, init_expression);
    break;
  }

    // for () {}  // Invalid.
  case Token_Type::right_paren:
    this->diags_.add(Diag_Missing_Header_Of_For_Loop{
        .where = Source_Code_Span(left_paren_token_begin, this->peek().end),
    });
    for_loop_style = Loop_Style::other;
    break;
  }

  // for (;;;) {}  // Invalid.
  // for (x of y; z) {}  // Invalid.
  while (this->peek().type == Token_Type::semicolon) {
    switch (for_loop_style) {
    case Loop_Style::c_style:
    case Loop_Style::other:
      this->diags_.add(Diag_Unexpected_Semicolon_In_C_Style_For_Loop{
          .semicolon = this->peek().span(),
      });
      break;
    case Loop_Style::for_in:
      this->diags_.add(Diag_Unexpected_Semicolon_In_For_In_Loop{
          .semicolon = this->peek().span(),
      });
      break;
    case Loop_Style::for_of:
      this->diags_.add(Diag_Unexpected_Semicolon_In_For_Of_Loop{
          .semicolon = this->peek().span(),
      });
      break;
    }
    this->skip();
    switch (this->peek().type) {
    case Token_Type::semicolon:
    case Token_Type::right_paren:
      break;
    default:
      this->parse_and_visit_expression(v);
      break;
    }
  }

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_paren);
  this->skip();

  this->error_on_class_statement(Statement_Kind::for_loop);
  this->error_on_function_statement(Statement_Kind::for_loop);
  this->error_on_lexical_declaration(Statement_Kind::for_loop);
  bool parsed_body =
      this->parse_and_visit_statement(v, Parse_Statement_Options{
                                             .allow_let_declaration = false,
                                         });
  if (!parsed_body) {
    this->diags_.add(Diag_Missing_Body_For_For_Statement{
        .for_and_header =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
    });
  }

  if (after_expression.has_value()) {
    this->visit_expression(*after_expression, v, Variable_Context::rhs);
  }
  if (entered_for_scope) {
    v.visit_exit_for_scope();
  }
}

void Parser::parse_and_visit_while(Parse_Visitor_Base &v) {
  Loop_Guard guard = this->enter_loop();

  QLJS_ASSERT(this->peek().type == Token_Type::kw_while);
  Source_Code_Span while_token_span = this->peek().span();
  this->skip();

  if (this->peek().type == Token_Type::left_curly) {
    // while { body; }  // Invalid.
    this->diags_.add(Diag_Missing_Condition_For_While_Statement{
        .while_keyword = while_token_span,
    });
  } else {
    this->parse_and_visit_parenthesized_expression<
        Diag_Expected_Parentheses_Around_While_Condition,
        Diag_Expected_Parenthesis_Around_While_Condition,
        /*CheckForSketchyConditions=*/true,
        /*CheckForCommaOperator=*/true>(v, while_token_span);
  }

  this->error_on_class_statement(Statement_Kind::while_loop);
  this->error_on_function_statement(Statement_Kind::while_loop);
  this->error_on_lexical_declaration(Statement_Kind::while_loop);
  bool parsed_body =
      this->parse_and_visit_statement(v, Parse_Statement_Options{
                                             .allow_let_declaration = false,
                                         });
  if (!parsed_body) {
    this->diags_.add(Diag_Missing_Body_For_While_Statement{
        .while_and_condition =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
    });
  }
}

void Parser::parse_and_visit_with(Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_with);
  Source_Code_Span with_token_span = this->peek().span();
  this->skip();

  this->parse_and_visit_parenthesized_expression<
      Diag_Expected_Parentheses_Around_With_Expression,
      Diag_Expected_Parenthesis_Around_With_Expression,
      /*CheckForSketchyConditions=*/false,
      /*CheckForCommaOperator=*/false>(v, with_token_span);

  this->error_on_class_statement(Statement_Kind::with_statement);
  this->error_on_function_statement(Statement_Kind::with_statement);
  this->error_on_lexical_declaration(Statement_Kind::with_statement);

  v.visit_enter_with_scope();
  bool parsed_body =
      this->parse_and_visit_statement(v, Parse_Statement_Options{
                                             .allow_let_declaration = false,
                                         });
  if (!parsed_body) {
    QLJS_PARSER_UNIMPLEMENTED();
  }
  v.visit_exit_with_scope();
}

void Parser::parse_and_visit_if(Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_if);
  Source_Code_Span if_token_span = this->peek().span();
  this->skip();

  if (this->peek().type == Token_Type::left_curly) {
    // if { body; }  // Invalid.
    this->diags_.add(Diag_Missing_Condition_For_If_Statement{
        .if_keyword = if_token_span,
    });
  } else {
    this->parse_and_visit_parenthesized_expression<
        Diag_Expected_Parentheses_Around_If_Condition,
        Diag_Expected_Parenthesis_Around_If_Condition,
        /*CheckForSketchyConditions=*/true,
        /*CheckForCommaOperator=*/true>(v, if_token_span);
  }

  auto parse_and_visit_body = [this, &v]() -> void {
    bool entered_block_scope = false;

    this->error_on_class_statement(Statement_Kind::if_statement);
    this->error_on_lexical_declaration(Statement_Kind::if_statement);
    if (this->is_maybe_function_statement()) {
      v.visit_enter_block_scope();
      entered_block_scope = true;
    }

    bool parsed_if_body =
        this->parse_and_visit_statement(v, Parse_Statement_Options{
                                               .allow_let_declaration = false,
                                           });
    if (!parsed_if_body) {
      QLJS_PARSER_UNIMPLEMENTED();
    }

    if (entered_block_scope) {
      v.visit_exit_block_scope();
    }
  };

  switch (this->peek().type) {
  default:
    parse_and_visit_body();
    break;

  // if (cond);  // Invalid TypeScript.
  case Token_Type::semicolon:
    if (this->options_.typescript) {
      this->diags_.add(Diag_Missing_Body_For_If_Statement{
          .expected_body = this->peek().span(),
      });
    } else {
      parse_and_visit_body();
    }
    break;
  case Token_Type::end_of_file:
  case Token_Type::kw_else:
  case Token_Type::right_curly:
    this->diags_.add(Diag_Missing_Body_For_If_Statement{
        .expected_body =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
    });
    break;
  }

parse_maybe_else:
  if (this->peek().type == Token_Type::kw_else) {
    this->skip();
    const Char8 *end_of_else = this->lexer_.end_of_previous_token();
    bool has_left_paren = this->peek().type == Token_Type::left_paren;
    if (has_left_paren) {
      this->parse_and_visit_expression(
          v, Precedence{
                 .trailing_curly_is_arrow_body = false,
             });
    } else {
      parse_and_visit_body();
    }
    if (has_left_paren) {
      bool has_left_curly = this->peek().type == Token_Type::left_curly;
      if (!this->peek().has_leading_newline && has_left_curly) {
        // if (cond) {} else (cond) {} // Invalid
        this->diags_.add(Diag_Missing_If_After_Else{
            .expected_if = Source_Code_Span::unit(end_of_else),
        });
        parse_and_visit_body();
        goto parse_maybe_else;
      } else {
        // if (cond) {} else (expr);
        this->consume_semicolon_after_statement();
      }
    }
  }
}

void Parser::parse_and_visit_import(Parse_Visitor_Base &v) {
  this->parse_and_visit_import(
      v, /*declare_context=*/TypeScript_Declare_Context());
}

void Parser::parse_and_visit_import(
    Parse_Visitor_Base &v, const TypeScript_Declare_Context &declare_context) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_import);
  Source_Code_Span import_span = this->peek().span();
  this->skip();

  bool possibly_typescript_import_namespace_alias = false;
  // For 'import fs from "node:fs";', declared_variable is 'fs'.
  std::optional<Token> declared_variable = std::nullopt;
  Variable_Kind declared_variable_kind = Variable_Kind::_import;
  auto declare_variable_if_needed = [&]() {
    if (declared_variable.has_value()) {
      switch (declared_variable->type) {
      // import var from "module";  // Invalid.
      QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD:
        this->diags_.add(Diag_Cannot_Import_Variable_Named_Keyword{
            .import_name = declared_variable->span(),
        });
        break;

      // import implements from "module";  // Invalid.
      // import implements = myNamespace;  // TypeScript only.
      QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
      case Token_Type::kw_await:
      case Token_Type::kw_yield:
      case Token_Type::kw_let:
        if (declared_variable_kind == Variable_Kind::_import_alias) {
          // NOTE[TypeScript-namespace-alias-name]: TypeScript namespace aliases
          // can be named 'await' because TypeScript namespace aliases do not
          // enforce strict mode.
          //
          // TODO(strager): We should should assume that 'export import' is
          // strict mode. See
          // TODO[TypeScript-export-namespace-alias-keyword-name].
        } else {
          if (declared_variable->type == Token_Type::kw_let) {
            this->diags_.add(Diag_Cannot_Import_Let{
                .import_name = declared_variable->span()});
          } else {
            this->diags_.add(Diag_Cannot_Import_Variable_Named_Keyword{
                .import_name = declared_variable->span(),
            });
          }
        }
        break;

      // import {banana} from "grocery-store";
      default:
        break;
      }

      v.visit_variable_declaration(declared_variable->identifier_name(),
                                   declared_variable_kind,
                                   Variable_Declaration_Flags::none);
      declared_variable = std::nullopt;  // Prevent double declaration.
    }
  };

  std::optional<Source_Code_Span> type_span = std::nullopt;
  switch (this->peek().type) {
  // import var from "module";  // Invalid.
  QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD:
    this->is_current_typescript_namespace_non_empty_ = true;
    goto identifier;

    // import \u{76}ar from "module";  // Invalid.
  case Token_Type::reserved_keyword_with_escape_sequence:
    this->lexer_.peek().add_diags_for_escape_sequences_in_keyword(
        this->diag_reporter_->diags());
    this->is_current_typescript_namespace_non_empty_ = true;
    goto identifier;

  // import let from "module";
  // import fs from "fs";
  // import fs = require("fs");        // TypeScript only.
  // import implements = ns;           // TypeScript only.
  // import implements from "module";  // Invalid.
  identifier:
  QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET_AND_STATIC_AND_TYPE:
  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
  case Token_Type::identifier:
  case Token_Type::kw_async:
  case Token_Type::kw_await:
  case Token_Type::kw_get:
  case Token_Type::kw_set:
  case Token_Type::kw_static:
  case Token_Type::kw_yield:
    // Do not set is_current_typescript_namespace_non_empty_.
    declared_variable = this->peek();
    this->skip();
    if (this->peek().type == Token_Type::comma) {
      declare_variable_if_needed();
      this->skip();
      switch (this->peek().type) {
        // import fs, {readFile} from "fs";
      case Token_Type::left_curly:
        this->parse_and_visit_named_exports_for_import(v);
        break;

        // import fs, * as fs2 from "fs";
      case Token_Type::star:
        this->parse_and_visit_name_space_import(v);
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    } else {
      // import fs from "fs";
      // import myns = ns;
      // import fs = require("fs");  // TypeScript only.
      possibly_typescript_import_namespace_alias = true;
    }
    break;

    // import {readFile} from "fs";
  case Token_Type::left_curly:
    // Do not set is_current_typescript_namespace_non_empty_.
    this->parse_and_visit_named_exports_for_import(v);
    break;

    // import expression statement:
    //
    // import(url).then(() => { /* ... */ })
    // import.meta
  case Token_Type::dot:
  case Token_Type::left_paren: {
    this->is_current_typescript_namespace_non_empty_ = true;
    Expression *ast = this->parse_expression_remainder(
        v, this->make_expression<Expression::Import>(import_span),
        Precedence{});
    this->visit_expression(ast, v, Variable_Context::rhs);
    this->consume_semicolon_after_statement();
    return;
  }

    // import * as fs from "fs";
  case Token_Type::star:
    // Do not set is_current_typescript_namespace_non_empty_.
    this->parse_and_visit_name_space_import(v);
    break;

    // import "foo";
  case Token_Type::string:
    // Do not set is_current_typescript_namespace_non_empty_.
    this->visited_module_import(this->peek());
    this->skip();
    this->consume_semicolon_after_statement();
    return;

  // import type T from "module";       // TypeScript only
  // import type {T} from "module";     // TypeScript only
  // import type * as M from "module";  // TypeScript only
  // import type from "module";
  case Token_Type::kw_type: {
    // Do not set is_current_typescript_namespace_non_empty_.
    type_span = this->peek().span();
    auto report_type_only_import_in_javascript_if_needed = [&] {
      if (!this->options_.typescript) {
        this->diags_.add(Diag_TypeScript_Type_Import_Not_Allowed_In_JavaScript{
            .type_keyword = *type_span,
        });
      }
    };
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    switch (this->peek().type) {
    // import type T from "module";       // TypeScript only
    // import type T, {U} from "module";  // Invalid.
    QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD_EXCEPT_TYPE:
    case Token_Type::identifier:
    case Token_Type::kw_accessor:
    case Token_Type::kw_as:
    case Token_Type::kw_async:
    case Token_Type::kw_get:
    case Token_Type::kw_let:
    case Token_Type::kw_of:
    case Token_Type::kw_satisfies:
    case Token_Type::kw_set:
    case Token_Type::kw_static:
    case Token_Type::kw_type:
      this->lexer_.commit_transaction(std::move(transaction));
      report_type_only_import_in_javascript_if_needed();
      declared_variable = this->peek();
      declared_variable_kind = Variable_Kind::_import_type;
      this->skip();
      if (this->peek().type == Token_Type::comma) {
        this->skip();
        switch (this->peek().type) {
        // import type T, {U} from "module";  // Invalid.
        case Token_Type::left_curly:
          this->diags_.add(
              Diag_TypeScript_Type_Only_Import_Cannot_Import_Default_And_Named{
                  .type_keyword = *type_span,
              });
          // Parse the named exports as if 'type' didn't exist. The user might
          // be thinking that 'type' only applies to 'T' and not '{U}'.
          this->parse_and_visit_named_exports_for_import(v);
          break;

        // import type T, * as U from "module";  // Invalid.
        case Token_Type::star:
          this->diags_.add(
              Diag_TypeScript_Type_Only_Import_Cannot_Import_Default_And_Named{
                  .type_keyword = *type_span,
              });
          this->parse_and_visit_name_space_import(v);
          break;

        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
        }
      } else {
        possibly_typescript_import_namespace_alias = true;
      }
      break;

    // import type {T} from "module";  // TypeScript only
    case Token_Type::left_curly:
      this->lexer_.commit_transaction(std::move(transaction));
      report_type_only_import_in_javascript_if_needed();
      this->parse_and_visit_named_exports_for_typescript_type_only_import(
          v, *type_span);
      break;

    // import type * as M from "module";  // TypeScript only
    case Token_Type::star:
      this->lexer_.commit_transaction(std::move(transaction));
      report_type_only_import_in_javascript_if_needed();
      this->parse_and_visit_name_space_import(v);
      break;

    // import type from "module";
    default:
      this->lexer_.roll_back_transaction(std::move(transaction));
      goto identifier;
    }
    break;
  }

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  switch (this->peek().type) {
  case Token_Type::kw_from:
    declare_variable_if_needed();
    this->skip();
    break;

  case Token_Type::string:
    declare_variable_if_needed();
    this->diags_.add(Diag_Expected_From_Before_Module_Specifier{
        .module_specifier = this->peek().span(),
    });
    break;

  // import fs = require("fs");  // TypeScript only.
  // import myns = ns;           // TypeScript only.
  // import C = ns.C;            // TypeScript only.
  case Token_Type::equal:
    if (possibly_typescript_import_namespace_alias) {
      Source_Code_Span equal_span = this->peek().span();
      this->skip();
      switch (this->peek().type) {
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case Token_Type::identifier: {
        Identifier namespace_name = this->peek().identifier_name();
        this->skip();
        if (this->peek().type == Token_Type::left_paren &&
            namespace_name.normalized_name() == u8"require"_sv) {
          // import fs = require("fs");
          // import type fs = require("fs");

          if (!this->options_.typescript) {
            this->diags_.add(
                Diag_TypeScript_Import_Alias_Not_Allowed_In_JavaScript{
                    .import_keyword = import_span,
                    .equal = equal_span,
                });
          }
          // NOTE[TypeScript-type-import-alias]: 'import fs = ' and
          // 'import type fs = ...' both declare variables which conflict with
          // 'let', 'class', etc. Overwrite Variable_Kind::_import_type.
          //
          // FIXME(strager): Should this behave like an import or an import
          // alias or some other kind?
          declared_variable_kind = Variable_Kind::_import;
          declare_variable_if_needed();
          if (declare_context.declare_namespace_declare_keyword.has_value() &&
              !declare_context.in_module) {
            this->diags_.add(Diag_Declare_Namespace_Cannot_Import_Module{
                .importing_keyword = import_span,
                .declare_keyword =
                    *declare_context.declare_namespace_declare_keyword,
            });
          }

          this->skip();
          QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::string);
          this->visited_module_import(this->peek());
          this->skip();
          QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_paren);
          this->skip();
        } else {
          if (declared_variable_kind == Variable_Kind::_import_type) {
            // import type a = b; // Invalid.
            this->diags_.add(
                Diag_TypeScript_Namespace_Alias_Cannot_Use_Import_Type{
                    .type_keyword = *type_span});
          } else {
            if (!this->options_.typescript) {
              // import a = b; // Invalid.
              this->diags_.add(
                  Diag_TypeScript_Namespace_Alias_Not_Allowed_In_JavaScript{
                      .import_keyword = import_span,
                      .equal = equal_span,
                  });
            }
          }
          // import myns = ns;
          // import C = ns.C;
          declared_variable_kind = Variable_Kind::_import_alias;
          declare_variable_if_needed();
          v.visit_variable_namespace_use(namespace_name);
          while (this->peek().type == Token_Type::dot) {
            this->skip();
            switch (this->peek().type) {
            QLJS_CASE_CONTEXTUAL_KEYWORD:
            QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
            case Token_Type::identifier:
            case Token_Type::kw_await:
            case Token_Type::kw_yield:
              this->skip();
              break;

            default:
              QLJS_PARSER_UNIMPLEMENTED();
              break;
            }
          }
        }
        break;
      }

      default:
        declare_variable_if_needed();
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }

      this->consume_semicolon_after_statement();
      return;
    }

    declare_variable_if_needed();
    [[fallthrough]];
  default:
    declare_variable_if_needed();
    this->diags_.add(Diag_Expected_From_And_Module_Specifier{
        .where = Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
    });
    return;
  }

  switch (this->peek().type) {
  // import fs from 'fs';
  case Token_Type::string:
    if (declare_context.declare_namespace_declare_keyword.has_value() &&
        !declare_context.in_module) {
      this->diags_.add(Diag_Declare_Namespace_Cannot_Import_Module{
          .importing_keyword = import_span,
          .declare_keyword = *declare_context.declare_namespace_declare_keyword,
      });
    }
    this->visited_module_import(this->peek());
    this->skip();
    break;

  // import foo from bar;  // Invalid.
  QLJS_CASE_KEYWORD:
  case Token_Type::identifier:
    this->diags_.add(Diag_Cannot_Import_From_Unquoted_Module{
        .import_name = this->peek().span(),
    });
    this->skip();
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  this->consume_semicolon_after_statement();
}

void Parser::parse_and_visit_name_space_import(Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::star);
  Source_Code_Span star_span = this->peek().span();
  this->skip();

  switch (this->peek().type) {
  case Token_Type::kw_as:
    this->skip();
    break;

  case Token_Type::identifier:
    this->diags_.add(Diag_Expected_As_Before_Imported_Namespace_Alias{
        .star_through_alias_token =
            Source_Code_Span(star_span.begin(), this->peek().end),
        .alias = this->peek().span(),
        .star_token = star_span,
    });
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  switch (this->peek().type) {
    // import * as var from "module";  // Invalid.
  QLJS_CASE_STRICT_RESERVED_KEYWORD:
    this->diags_.add(Diag_Cannot_Import_Variable_Named_Keyword{
        .import_name = this->peek().span(),
    });
    goto identifier;

    // import * as \u{76}ar from "module";  // Invalid.
  case Token_Type::reserved_keyword_with_escape_sequence:
    this->lexer_.peek().add_diags_for_escape_sequences_in_keyword(
        this->diag_reporter_->diags());
    goto identifier;

  identifier:
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case Token_Type::identifier:
    if (this->peek().type == Token_Type::kw_let) {
      this->diags_.add(Diag_Cannot_Import_Let{
          .import_name = this->peek().identifier_name().span()});
    }
    v.visit_variable_declaration(this->peek().identifier_name(),
                                 Variable_Kind::_import,
                                 Variable_Declaration_Flags::none);
    this->skip();
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

void Parser::parse_and_visit_named_exports_for_import(Parse_Visitor_Base &v) {
  this->parse_and_visit_named_exports(
      v,
      /*typescript_type_only_keyword=*/std::nullopt,
      /*out_exported_bad_tokens=*/nullptr);
}

void Parser::parse_and_visit_named_exports_for_typescript_type_only_import(
    Parse_Visitor_Base &v, Source_Code_Span type_keyword) {
  this->parse_and_visit_named_exports(
      v,
      /*typescript_type_only_keyword=*/type_keyword,
      /*out_exported_bad_tokens=*/nullptr);
}

void Parser::parse_and_visit_named_exports(
    Parse_Visitor_Base &v,
    std::optional<Source_Code_Span> typescript_type_only_keyword,
    Vector<Token> *out_exported_bad_tokens) {
  QLJS_ASSERT(this->peek().type == Token_Type::left_curly);
  this->skip();

  bool is_export = out_exported_bad_tokens != nullptr;

  for (;;) {
    bool is_local_type_export = false;
    bool left_is_keyword = false;
    auto is_type_export = [&]() -> bool {
      return is_local_type_export || typescript_type_only_keyword.has_value();
    };
    auto imported_variable_kind = [&]() -> Variable_Kind {
      return is_type_export() ? Variable_Kind::_import_type
                              : Variable_Kind::_import;
    };
    switch (this->peek().type) {
    QLJS_CASE_STRICT_RESERVED_KEYWORD:
    case Token_Type::reserved_keyword_with_escape_sequence:
      if (out_exported_bad_tokens) {
        out_exported_bad_tokens->emplace_back(this->peek());
      }
      left_is_keyword = true;
      goto named_export;

    named_export:
    QLJS_CASE_CONTEXTUAL_KEYWORD_EXCEPT_ASYNC_AND_GET_AND_SET_AND_STATIC_AND_TYPE
        :
    case Token_Type::identifier:
    case Token_Type::kw_async:
    case Token_Type::kw_get:
    case Token_Type::kw_set:
    case Token_Type::kw_static: {
      Identifier left_name = this->peek().identifier_name();
      Token right_token = this->peek();
      this->skip();
      bool has_as = this->peek().type == Token_Type::kw_as;
      if (has_as) {
        this->skip();
        switch (this->peek().type) {
        case Token_Type::string:
          // TODO(strager): Check that the string is valid Unicode
          // (standard: IsStringWellFormedUnicode).
          [[fallthrough]];
        QLJS_CASE_KEYWORD:
        case Token_Type::identifier:
        case Token_Type::reserved_keyword_with_escape_sequence:
          right_token = this->peek();
          this->skip();
          break;
        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
        }
      }
      if (is_export) {
        if (left_is_keyword) {
          // Ignore. We will emit Diag_Cannot_Export_Variable_Named_Keyword
          // later.
        } else {
          if (is_type_export()) {
            // export type {T};        // TypeScript only.
            // export {type T as TT};  // TypeScript only.
            v.visit_variable_type_use(left_name);
          } else if (right_token.type == Token_Type::kw_default) {
            // export {C as default};
            this->found_default_export(/*default_keyword=*/right_token.span(),
                                       /*is_mergeable_interface=*/false);
            v.visit_variable_export_default_use(left_name);
          } else {
            // export {C};
            v.visit_variable_export_use(left_name);
          }
        }
      } else {
        switch (right_token.type) {
          // import {myFunc} from 'other';
          // import {myFunc as let} from 'other';  // Invalid.
          // import {myFunc as static} from 'other';
        QLJS_CASE_CONTEXTUAL_KEYWORD:
        case Token_Type::identifier:
          if (right_token.type == Token_Type::kw_let) {
            this->diags_.add(
                Diag_Cannot_Import_Let{.import_name = right_token.span()});
          }
          v.visit_variable_declaration(right_token.identifier_name(),
                                       imported_variable_kind(),
                                       Variable_Declaration_Flags::none);
          break;

          // import {var} from 'other';  // Invalid.
        QLJS_CASE_STRICT_RESERVED_KEYWORD:
          this->diags_.add(Diag_Cannot_Import_Variable_Named_Keyword{
              .import_name = right_token.span(),
          });
          // FIXME(strager): Declaring a variable with a keyword name is
          // sketchy. Delete this?
          v.visit_variable_declaration(right_token.identifier_name(),
                                       Variable_Kind::_import,
                                       Variable_Declaration_Flags::none);
          break;

          // import {\u{76}ar} from 'other';  // Invalid.
        case Token_Type::reserved_keyword_with_escape_sequence:
          right_token.add_diags_for_escape_sequences_in_keyword(
              this->diag_reporter_->diags());
          // FIXME(strager): Declaring a variable with a keyword name is
          // sketchy. Delete this?
          v.visit_variable_declaration(right_token.identifier_name(),
                                       Variable_Kind::_import,
                                       Variable_Declaration_Flags::none);
          break;

        case Token_Type::string:
          QLJS_ASSERT(has_as);
          this->diags_.add(Diag_Expected_Variable_Name_For_Import_As{
              .unexpected_token = right_token.span(),
          });
          break;

        default:
          QLJS_UNIMPLEMENTED();
          break;
        }
      }
      break;
    }

    // import {type} from "other";
    // import {type as alias} from "other";
    // import {type T} from "other";         // TypeScript only
    case Token_Type::kw_type: {
      Source_Code_Span type_span = this->peek().span();
      auto report_diag_for_inline_type_import_if_needed = [&] {
        if (!this->options_.typescript) {
          if (is_export) {
            this->diags_.add(
                Diag_TypeScript_Type_Export_Not_Allowed_In_JavaScript{
                    .type_keyword = type_span,
                });
          } else {
            this->diags_.add(
                Diag_TypeScript_Type_Import_Not_Allowed_In_JavaScript{
                    .type_keyword = type_span,
                });
          }
        }
        if (typescript_type_only_keyword.has_value()) {
          if (is_export) {
            this->diags_.add(
                Diag_TypeScript_Inline_Type_Export_Not_Allowed_In_Type_Only_Export{
                    .inline_type_keyword = type_span,
                    .type_only_keyword = *typescript_type_only_keyword,
                });
          } else {
            this->diags_.add(
                Diag_TypeScript_Inline_Type_Import_Not_Allowed_In_Type_Only_Import{
                    .inline_type_keyword = type_span,
                    .type_only_keyword = *typescript_type_only_keyword,
                });
          }
        }
      };
      Lexer_Transaction transaction = this->lexer_.begin_transaction();
      this->skip();
      switch (this->peek().type) {
      // import {type as U} from "other";
      // import {type T} from "other";     // TypeScript only
      // import {type as} from "other";    // TypeScript only
      QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD_EXCEPT_TYPE:
      case Token_Type::identifier:
      case Token_Type::kw_accessor:
      case Token_Type::kw_async:
      case Token_Type::kw_from:
      case Token_Type::kw_get:
      case Token_Type::kw_let:
      case Token_Type::kw_of:
      case Token_Type::kw_satisfies:
      case Token_Type::kw_set:
      case Token_Type::kw_static:
      case Token_Type::kw_type:
        is_local_type_export = true;
        this->lexer_.commit_transaction(std::move(transaction));
        report_diag_for_inline_type_import_if_needed();
        goto named_export;

      case Token_Type::kw_as:
        this->skip();
        switch (this->peek().type) {
        // import {type as} from "mod";  // TypeScript only
        case Token_Type::comma:
        case Token_Type::right_curly:
          is_local_type_export = true;
          this->lexer_.roll_back_transaction(std::move(transaction));
          report_diag_for_inline_type_import_if_needed();
          this->skip();  // Skip 'type'.
          QLJS_ASSERT(this->peek().type == Token_Type::kw_as);
          goto named_export;

        // import {type as alias} from "mod";
        default:
          this->lexer_.roll_back_transaction(std::move(transaction));
          goto named_export;
        }

      // import {type} from "other";
      default:
        this->lexer_.roll_back_transaction(std::move(transaction));
        goto named_export;
      }
      break;
    }

      // import {"export name" as varName} from "other";
      // export {"export name"} from "other";
    case Token_Type::string:
      // TODO(strager): Check that the string is valid Unicode
      // (standard: IsStringWellFormedUnicode).
      if (is_export) {
        if (out_exported_bad_tokens) {
          out_exported_bad_tokens->emplace_back(this->peek());
        }
        this->skip();
      } else {
        this->skip();

        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_as);
        this->skip();

        switch (this->peek().type) {
          // import {'name' as bread} from 'other';
          // import {'name' as let} from 'other';  // Invalid.
          // import {'name' as static} from 'other';
        QLJS_CASE_CONTEXTUAL_KEYWORD:
        case Token_Type::identifier:
          if (this->peek().type == Token_Type::kw_let) {
            this->diags_.add(
                Diag_Cannot_Import_Let{.import_name = this->peek().span()});
          }
          v.visit_variable_declaration(this->peek().identifier_name(),
                                       Variable_Kind::_import,
                                       Variable_Declaration_Flags::none);
          this->skip();
          break;

          // import {'name' as debugger} from 'other';  // Invalid.
        QLJS_CASE_STRICT_RESERVED_KEYWORD:
          this->diags_.add(Diag_Cannot_Import_Variable_Named_Keyword{
              .import_name = this->peek().span(),
          });
          v.visit_variable_declaration(this->peek().identifier_name(),
                                       Variable_Kind::_import,
                                       Variable_Declaration_Flags::none);
          this->skip();
          break;

          // import {'name' as \u{76}ar} from 'other';  // Invalid.
        case Token_Type::reserved_keyword_with_escape_sequence:
          this->peek().add_diags_for_escape_sequences_in_keyword(
              this->diag_reporter_->diags());
          v.visit_variable_declaration(this->peek().identifier_name(),
                                       Variable_Kind::_import,
                                       Variable_Declaration_Flags::none);
          this->skip();
          break;

        case Token_Type::string:
          this->diags_.add(Diag_Expected_Variable_Name_For_Import_As{
              .unexpected_token = this->peek().span(),
          });
          this->skip();
          break;

        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
        }
      }
      break;

    case Token_Type::right_curly:
      goto done;
    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    if (this->peek().type == Token_Type::comma) {
      this->skip();
    }
  }
done:
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_curly);
  this->skip();
}

void Parser::visited_module_import(const Token &module_name) {
  QLJS_ASSERT(module_name.type == Token_Type::string);
  // TODO(#1159): Write a proper routine to decode string literals.
  String8_View module_name_unescaped =
      make_string_view(module_name.begin + 1, module_name.end - 1);
  if (module_name_unescaped == u8"react"_sv ||
      module_name_unescaped == u8"react-dom"_sv ||
      starts_with(module_name_unescaped, u8"react-dom/"_sv)) {
    this->imported_react_ = true;
  }
  if (module_name_unescaped == u8"preact"_sv ||
      starts_with(module_name_unescaped, u8"preact/"_sv)) {
    this->imported_preact_ = true;
  }
}

void Parser::parse_and_visit_variable_declaration_statement(
    Parse_Visitor_Base &v,
    bool is_top_level_typescript_definition_without_declare_or_export) {
  Token declaring_token = this->peek();
  QLJS_ASSERT(declaring_token.type == Token_Type::kw_const ||
              declaring_token.type == Token_Type::kw_let ||
              declaring_token.type == Token_Type::kw_var);
  this->skip();
  if (this->peek().type == Token_Type::kw_enum &&
      declaring_token.type == Token_Type::kw_const) {
    if (is_top_level_typescript_definition_without_declare_or_export) {
      this->diags_.add(Diag_DTS_Missing_Declare_Or_Export{
          .expected = Source_Code_Span::unit(declaring_token.begin),
          .declaring_token = this->peek().span(),
      });
    }
    this->parse_and_visit_typescript_enum(v, Enum_Kind::const_enum);
  } else {
    this->is_current_typescript_namespace_non_empty_ = true;
    this->parse_and_visit_let_bindings(
        v, Parse_Let_Bindings_Options{
               .declaring_token = declaring_token,
               .is_top_level_typescript_definition_without_declare_or_export =
                   is_top_level_typescript_definition_without_declare_or_export,
           });
    this->consume_semicolon_after_statement();
  }
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
void Parser::parse_and_visit_let_bindings(
    Parse_Visitor_Base &v, const Parse_Let_Bindings_Options &options) {
  Variable_Kind declaration_kind;
  switch (options.declaring_token.type) {
  case Token_Type::kw_const:
    declaration_kind = Variable_Kind::_const;
    break;
  case Token_Type::kw_let:
    declaration_kind = Variable_Kind::_let;
    break;
  case Token_Type::kw_var:
    declaration_kind = Variable_Kind::_var;
    break;
  default:
    QLJS_ASSERT(false);
    declaration_kind = Variable_Kind::_let;
    break;
  }

  if (options.is_top_level_typescript_definition_without_declare_or_export) {
    this->diags_.add(Diag_DTS_Missing_Declare_Or_Export{
        .expected = Source_Code_Span::unit(options.declaring_token.begin),
        .declaring_token = options.declaring_token.span(),
    });
  }

  Variable_Declaration_Flags flags_without_initializer =
      options.is_in_for_initializer
          ? Variable_Declaration_Flags::inside_for_loop_head
          : Variable_Declaration_Flags::none;
  Variable_Declaration_Flags flags_with_initializer =
      options.is_in_for_initializer
          ? Variable_Declaration_Flags::
                inside_for_loop_head_initialized_with_equals
          : Variable_Declaration_Flags::initialized_with_equals;

  Source_Code_Span let_span = options.declaring_token.span();
  bool first_binding = true;
  for (;;) {
    std::optional<Source_Code_Span> comma_span = std::nullopt;
    if (!first_binding) {
      switch (this->peek().type) {
      case Token_Type::comma:
        comma_span = this->peek().span();
        this->skip();
        break;

      case Token_Type::identifier:
      case Token_Type::left_curly:
      case Token_Type::left_square:
        if (this->peek().has_leading_newline) {
          // Caller will insert our semicolon if needed.
          return;
        } else {
          // let x y
          this->diags_.add(Diag_Missing_Comma_Between_Variable_Declarations{
              .expected_comma =
                  Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
          });
        }
        break;

      default:
        // Caller will insert our semicolon if needed.
        return;
      }
    }

    switch (this->peek().type) {
    case Token_Type::kw_await:
      if (this->in_async_function_) {
        this->diags_.add(Diag_Cannot_Declare_Await_In_Async_Function{
            .name = this->peek().span(),
        });
      }
      goto variable_name;

    case Token_Type::kw_yield:
      if (this->in_generator_function_) {
        this->diags_.add(Diag_Cannot_Declare_Yield_In_Generator_Function{
            .name = this->peek().span(),
        });
      }
      goto variable_name;

    // let protected = 42;
    QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
      // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
      goto variable_name;

      // let x;
      // let x = 42;
    QLJS_CASE_TYPESCRIPT_ONLY_CONTEXTUAL_KEYWORD:
    variable_name:
    case Token_Type::identifier:
    case Token_Type::kw_as:
    case Token_Type::kw_accessor:
    case Token_Type::kw_async:
    case Token_Type::kw_from:
    case Token_Type::kw_get:
    case Token_Type::kw_let:
    case Token_Type::kw_of:
    case Token_Type::kw_satisfies:
    case Token_Type::kw_set:
    case Token_Type::kw_static: {
      Expression *variable = this->make_expression<Expression::Variable>(
          this->peek().identifier_name(), this->peek().type);
      this->skip();

      // '!' in 'let x!: string;' (if present) (TypeScript only).
      std::optional<Source_Code_Span> definite_assignment_assertion =
          std::nullopt;
      bool reported_issue_with_definite_assignment_assertion = false;
      bool has_type_annotation;

      if (this->peek().type == Token_Type::bang) {
        // let x!: Type;
        if (this->peek().has_leading_newline) {
          // let x /* ASI */ !expr();
          Lexer_Transaction transaction = this->lexer_.begin_transaction();
          this->skip();
          bool has_type_annotation_after_definite_assignment_assertion =
              this->peek().type == Token_Type::colon;
          this->lexer_.roll_back_transaction(std::move(transaction));

          if (has_type_annotation_after_definite_assignment_assertion) {
            // let x /* ASI */ !: Type;  // Invalid.
            this->diags_.add(
                Diag_Newline_Not_Allowed_Before_Definite_Assignment_Assertion{
                    .definite_assignment_assertion = this->peek().span(),
                });
            // Behave as if the newline wasn't there.
          } else {
            // let x /* ASI */ !expr();
            QLJS_ASSERT(!definite_assignment_assertion.has_value());
            // Stop parsing.
            goto asi_after_variable_name;
          }
        }
        definite_assignment_assertion = this->peek().span();
        this->skip();
        if (!reported_issue_with_definite_assignment_assertion &&
            options.declare_keyword.has_value()) {
          this->diags_.add(
              Diag_TypeScript_Definite_Assignment_Assertion_In_Ambient_Context{
                  .definite_assignment_assertion =
                      *definite_assignment_assertion,
                  .declare_keyword = *options.declare_keyword,
              });
          reported_issue_with_definite_assignment_assertion = true;
        }
        if (!reported_issue_with_definite_assignment_assertion &&
            declaration_kind == Variable_Kind::_const) {
          this->diags_.add(
              Diag_TypeScript_Definite_Assignment_Assertion_On_Const{
                  .definite_assignment_assertion =
                      *definite_assignment_assertion,
                  .const_keyword = options.declaring_token.span(),
              });
          reported_issue_with_definite_assignment_assertion = true;
        }
      }

      has_type_annotation = this->peek().type == Token_Type::colon;
      if (has_type_annotation) {
        // let x: Type;
        this->parse_and_visit_typescript_colon_type_expression(v);
      }

      if (definite_assignment_assertion.has_value() && !has_type_annotation) {
        // let x!;  // Invalid.
        if (!reported_issue_with_definite_assignment_assertion) {
          if (this->options_.typescript) {
            this->diags_.add(
                Diag_TypeScript_Definite_Assignment_Assertion_Without_Type_Annotation{
                    .definite_assignment_assertion =
                        *definite_assignment_assertion,
                });
          } else {
            this->diags_.add(
                Diag_TypeScript_Definite_Assignment_Assertion_Not_Allowed_In_JavaScript{
                    .definite_assignment_assertion =
                        *definite_assignment_assertion,
                });
          }
          reported_issue_with_definite_assignment_assertion = true;
        }
      }

      switch (this->peek().type) {
        // let x = 3;
        // let x += 42;  // Invalid.
      QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
      case Token_Type::equal: {
        Token equal_token = this->peek();
        if (options.is_declare(this) &&
            declaration_kind != Variable_Kind::_const) {
          if (this->options_.typescript_definition_file) {
            this->diags_.add(Diag_DTS_Var_Cannot_Have_Initializer{
                .equal = equal_token.span(),
                .declaring_token = options.declaring_token.span(),
            });
          } else {
            this->diags_.add(Diag_Declare_Var_Cannot_Have_Initializer{
                .equal = equal_token.span(),
                .declare_keyword = *options.declare_keyword,
                .declaring_token = options.declaring_token.span(),
            });
          }
        }
        if (definite_assignment_assertion.has_value()) {
          // let x!: string = '';  // Invalid.
          if (!reported_issue_with_definite_assignment_assertion) {
            this->diags_.add(
                Diag_TypeScript_Definite_Assignment_Assertion_With_Initializer{
                    .definite_assignment_assertion =
                        *definite_assignment_assertion,
                    .equal = equal_token.span(),
                });
            reported_issue_with_definite_assignment_assertion = true;
          }
        }

        auto *assignment_ast = expression_cast<Expression::Assignment *>(
            this->parse_expression_remainder(
                v, variable,
                Precedence{.commas = false,
                           .in_operator = options.allow_in_operator}));

        if (options.is_in_for_initializer &&
            this->peek().type == Token_Type::kw_in) {
          // for (var x = "initial" in obj)
          // for (let x = "prop" in obj)  // Invalid.
          // for (let x = "prop" in obj; i < 10; ++i)  // Invalid.
          Source_Code_Span in_token_span = this->peek().span();
          QLJS_ASSERT(!options.allow_in_operator);

          // FIXME(#831): v should not be used here. We should use a
          // buffering_visitor.
          this->try_parse(
              [&](Parser_Transaction &) {
                Expression *in_ast = this->parse_expression_remainder(
                    v, assignment_ast->child_1(), Precedence{.commas = false});
                if (this->peek().type != Token_Type::semicolon) {
                  return false;
                }
                // for (let x = "prop" in obj; i < 10; ++i)  // Invalid.
                assignment_ast->children_[1] = in_ast;
                this->diags_.add(Diag_In_Disallowed_In_C_Style_For_Loop{
                    .in_token = in_token_span,
                });
                return true;
              },
              [&] {
                if (declaration_kind == Variable_Kind::_var) {
                  // for (var x = "initial" in obj)
                } else {
                  // for (let x = "prop" in obj)  // Invalid.
                  this->diags_.add(
                      Diag_Cannot_Assign_To_Loop_Variable_In_For_Of_Or_In_Loop{
                          .equal_token = equal_token.span()});
                }
              });
        } else if (options.is_in_for_initializer &&
                   this->peek().type == Token_Type::kw_of) {
          // for (var x = "initial" of obj)  // Invalid.
          this->diags_.add(
              Diag_Cannot_Assign_To_Loop_Variable_In_For_Of_Or_In_Loop{
                  .equal_token = equal_token.span()});
        }

        this->visit_binding_element(
            assignment_ast, v,
            Binding_Element_Info{
                .declaration_kind = declaration_kind,
                .declaring_token = options.declaring_token.span(),
                .flags = flags_with_initializer,
            });
        break;
      }

      asi_after_variable_name:
      case Token_Type::kw_await:
      case Token_Type::kw_class:
      case Token_Type::kw_function:
      case Token_Type::kw_new:
      case Token_Type::kw_null:
      case Token_Type::kw_this:
      case Token_Type::kw_typeof: {
        if (this->peek().has_leading_newline) {
          // let x  // ASI
          // null;
          this->visit_binding_element(
              variable, v,
              Binding_Element_Info{
                  .declaration_kind = declaration_kind,
                  .declaring_token = options.declaring_token.span(),
                  .flags = flags_without_initializer,
              });
          this->lexer_.insert_semicolon();
          return;
        }
        // let x null;  // ERROR
        this->diags_.add(Diag_Missing_Equal_After_Variable{
            .expected_equal =
                Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
        });
        this->parse_and_visit_expression(
            v, Precedence{.commas = false,
                          .in_operator = options.allow_in_operator});
        this->visit_binding_element(
            variable, v,
            Binding_Element_Info{
                .declaration_kind = declaration_kind,
                .declaring_token = options.declaring_token.span(),
                .flags = flags_with_initializer,
            });
        break;
      }

        // let x;
        // let x, y;
      default:
        if (declaration_kind == Variable_Kind::_const) {
          if (!options.allow_const_without_initializer &&
              !options.is_declare(this)) {
            if (definite_assignment_assertion.has_value()) {
              // const x!: string;  // Invalid.
              // We already reported
              // Diag_TypeScript_Definite_Assignment_Assertion_On_Const.
            } else {
              this->diags_.add(Diag_Missing_Initializer_In_Const_Declaration{
                  .variable_name = variable->span()});
            }
          }
        }
        this->visit_binding_element(
            variable, v,
            Binding_Element_Info{
                .declaration_kind = declaration_kind,
                .declaring_token = options.declaring_token.span(),
                .flags = flags_without_initializer,
            });
        break;
      }
      break;
    }

      // \u{69}\u{66} // 'if', but escaped.
    case Token_Type::reserved_keyword_with_escape_sequence:
      this->lexer_.peek().add_diags_for_escape_sequences_in_keyword(
          this->diag_reporter_->diags());
      goto variable_name;

      // let {x} = xs;
      // let [head, ...tail] = xs;
      // for (let {prop} of xs) {}
    case Token_Type::left_curly:
    case Token_Type::left_square: {
      Expression *ast = this->parse_expression(
          v, Precedence{.commas = false,
                        .in_operator = options.allow_in_operator});
      // TODO(strager): Report error if initializer is missing.
      this->visit_binding_element(
          ast, v,
          Binding_Element_Info{
              .declaration_kind = declaration_kind,
              .declaring_token = options.declaring_token.span(),
              .flags = flags_without_initializer,
          });
      break;
    }

      // let switch = 3;  // Invalid.
      // let if (x) {}    // Invalid.
    QLJS_CASE_RESERVED_KEYWORD_EXCEPT_AWAIT_AND_YIELD : {
      Source_Code_Span keyword_span = this->peek().span();
      Lexer_Transaction transaction = this->lexer_.begin_transaction();
      this->skip();

      switch (this->peek().type) {
        // let switch = 3;  // Invalid.
      case Token_Type::end_of_file:
      case Token_Type::equal:
      case Token_Type::semicolon:
        this->lexer_.commit_transaction(std::move(transaction));
        this->diags_.add(Diag_Cannot_Declare_Variable_With_Keyword_Name{
            .keyword = keyword_span,
        });
        this->skip();
        this->parse_and_visit_expression(
            v, Precedence{.commas = false,
                          .in_operator = options.allow_in_operator});
        break;

        // let if (x) {}    // Invalid.
      default:
        this->lexer_.roll_back_transaction(std::move(transaction));
        if (this->peek().has_leading_newline) {
          this->diags_.add(Diag_Let_With_No_Bindings{let_span});
        } else {
          this->diags_.add(
              Diag_Unexpected_Token_In_Variable_Declaration{keyword_span});
          this->lexer_.insert_semicolon();
        }
        break;
      }
      break;
    }

      // let 42;  // Invalid.
    case Token_Type::complete_template:
    case Token_Type::number:
      this->diags_.add(Diag_Unexpected_Token_In_Variable_Declaration{
          .unexpected_token = this->peek().span(),
      });
      this->lexer_.insert_semicolon();
      break;

      // let v, `hello${world}`;  // Invalid.
    case Token_Type::incomplete_template:
      // TODO(strager): Improve the span.
      this->diags_.add(Diag_Unexpected_Token_In_Variable_Declaration{
          .unexpected_token = this->peek().span(),
      });
      this->lexer_.insert_semicolon();
      break;

    QLJS_CASE_COMPOUND_ASSIGNMENT_OPERATOR:
    case Token_Type::comma:
    case Token_Type::dot:
    case Token_Type::equal_greater:
    case Token_Type::left_paren:
    case Token_Type::minus:
    case Token_Type::plus:
    case Token_Type::question:
    case Token_Type::slash:
      QLJS_PARSER_UNIMPLEMENTED();
      break;

    case Token_Type::equal:
      this->diags_.add(Diag_Missing_Variable_Name_In_Declaration{
          .equal_token = this->peek().span(),
      });
      this->skip();
      this->parse_and_visit_expression(
          v, Precedence{.commas = false,
                        .in_operator = options.allow_in_operator});
      break;

    case Token_Type::semicolon:
    default:
      if (first_binding) {
        this->diags_.add(Diag_Let_With_No_Bindings{let_span});
      } else {
        QLJS_ASSERT(comma_span.has_value());
        this->diags_.add(Diag_Stray_Comma_In_Let_Statement{*comma_span});
      }
      break;
    }
    first_binding = false;
  }
}
QLJS_WARNING_POP

void Parser::visit_binding_element(Expression *ast, Parse_Visitor_Base &v,
                                   const Binding_Element_Info &info) {
  switch (info.declaration_kind) {
  case Variable_Kind::_const:
  case Variable_Kind::_let:
  case Variable_Kind::_var:
    break;
  default:
    QLJS_ASSERT(!enum_has_flags(
        info.flags, Variable_Declaration_Flags::initialized_with_equals));
    break;
  }

  auto visit_variable_declaration = [&](const Identifier &ident) -> void {
    v.visit_variable_declaration(ident, info.declaration_kind, info.flags);
  };

  switch (ast->kind()) {
  case Expression_Kind::Array: {
    Binding_Element_Info child_info = info.with_destructuring();
    for (Expression *item : ast->children()) {
      this->visit_binding_element(item, v, child_info);
    }
    break;
  }

  case Expression_Kind::Compound_Assignment:
    if (info.declaring_token.has_value()) {
      auto *assignment = expression_cast<Expression::Assignment *>(ast);
      this->diags_.add(Diag_Cannot_Update_Variable_During_Declaration{
          .declaring_token = *info.declaring_token,
          .updating_operator = assignment->operator_span_,
      });
    } else {
      this->diags_.add(Diag_Invalid_Parameter{
          .parameter = ast->span(),
      });
    }
    [[fallthrough]];
  case Expression_Kind::Assignment: {
    auto *assignment = expression_cast<const Expression::Assignment *>(ast);
    Expression *lhs = assignment->children_[0];
    Expression *rhs = assignment->children_[1];

    if (lhs->kind() == Expression_Kind::Optional) {
      // TODO(strager): Only report this for parameters, not for other variable
      // kinds.
      this->diags_.add(Diag_Optional_Parameter_Cannot_Have_Initializer{
          .equal = assignment->operator_span_,
          .question = expression_cast<const Expression::Optional *>(lhs)
                          ->question_span(),
      });
    }

    this->visit_expression(rhs, v, Variable_Context::rhs);
    Variable_Declaration_Flags lhs_flags = enum_select_flags(
        info.flags, Variable_Declaration_Flags::inside_for_loop_head);
    switch (info.declaration_kind) {
    case Variable_Kind::_const:
    case Variable_Kind::_let:
    case Variable_Kind::_var:
      lhs_flags = enum_set_flags(
          lhs_flags, Variable_Declaration_Flags::initialized_with_equals);
      break;
    default:
      break;
    }
    this->visit_binding_element(lhs, v, info.with_flags(lhs_flags));
    break;
  }

  case Expression_Kind::Variable: {
    Identifier ident = ast->variable_identifier();
    if ((info.declaration_kind == Variable_Kind::_const ||
         info.declaration_kind == Variable_Kind::_import ||
         info.declaration_kind == Variable_Kind::_let) &&
        ast->variable_identifier_token_type() == Token_Type::kw_let) {
      // If this is an import, we would emit Diag_Cannot_Import_Let
      // instead.
      QLJS_ASSERT(info.declaration_kind != Variable_Kind::_import);
      this->diags_.add(Diag_Cannot_Declare_Variable_Named_Let_With_Let{
          .name = ident.span()});
    }
    visit_variable_declaration(ident);
    break;
  }

  case Expression_Kind::Object: {
    Binding_Element_Info child_info = info.with_destructuring();
    for (int i = 0; i < ast->object_entry_count(); ++i) {
      const Object_Property_Value_Pair &entry = ast->object_entry(i);
      if (entry.init) {
        this->visit_expression(entry.init, v, Variable_Context::rhs);
      }
      this->visit_binding_element(entry.value, v, child_info);
    }
    break;
  }

  case Expression_Kind::Spread: {
    Expression::Spread *spread = expression_cast<Expression::Spread *>(ast);
    if (spread->child_0()->kind() == Expression_Kind::Missing) {
      this->diags_.add(Diag_Spread_Must_Precede_Variable_Name{
          spread->spread_operator_span()});
    }
    this->visit_binding_element(
        spread->child_0(), v, info.with_spread(spread->spread_operator_span()));
    break;
  }

  case Expression_Kind::Await: {
    auto *await = expression_cast<Expression::Await *>(ast);
    Identifier ident(await->unary_operator_span());
    visit_variable_declaration(ident);
    this->diags_.add(Diag_Cannot_Declare_Await_In_Async_Function{
        .name = ident.span(),
    });
    break;
  }

  case Expression_Kind::Yield_None: {
    Identifier ident(ast->span());
    visit_variable_declaration(ident);
    this->diags_.add(Diag_Cannot_Declare_Yield_In_Generator_Function{
        .name = ident.span(),
    });
    break;
  }
  case Expression_Kind::Class:
  case Expression_Kind::Delete:
  case Expression_Kind::New:
  case Expression_Kind::Template:
  case Expression_Kind::Typeof:
  case Expression_Kind::Arrow_Function:
  case Expression_Kind::Binary_Operator:
  case Expression_Kind::Conditional:
  case Expression_Kind::Conditional_Assignment:
  case Expression_Kind::Dot:
  case Expression_Kind::Function:
  case Expression_Kind::Import:
  case Expression_Kind::Index:
  case Expression_Kind::JSX_Element:
  case Expression_Kind::JSX_Element_With_Members:
  case Expression_Kind::JSX_Element_With_Namespace:
  case Expression_Kind::JSX_Fragment:
  case Expression_Kind::Named_Function:
  case Expression_Kind::New_Target:
  case Expression_Kind::RW_Unary_Prefix:
  case Expression_Kind::RW_Unary_Suffix:
  case Expression_Kind::Super:
  case Expression_Kind::Tagged_Template_Literal:
  case Expression_Kind::Unary_Operator:
  case Expression_Kind::Yield_Many:
  case Expression_Kind::Yield_One:
    this->diags_.add(Diag_Invalid_Parameter{
        .parameter = ast->span(),
    });
    break;

  // function f(x!) {}  // Invalid.
  case Expression_Kind::Non_Null_Assertion: {
    auto *assertion =
        expression_cast<const Expression::Non_Null_Assertion *>(ast);
    this->diags_.add(Diag_Non_Null_Assertion_Not_Allowed_In_Parameter{
        .bang = assertion->bang_span(),
    });
    this->visit_binding_element(assertion->child_, v, info);
    break;
  }

  // function f(<T>p) {}  // Invalid.
  case Expression_Kind::Angle_Type_Assertion: {
    auto *assertion =
        expression_cast<const Expression::Angle_Type_Assertion *>(ast);
    this->diags_.add(Diag_Invalid_Parameter{
        .parameter = assertion->span(),
    });
    this->visit_binding_element(assertion->child_, v, info);
    break;
  }

  // function f(x as y) {}  // Invalid.
  case Expression_Kind::As_Type_Assertion: {
    auto *assertion =
        expression_cast<const Expression::As_Type_Assertion *>(ast);
    this->diags_.add(
        Diag_TypeScript_As_Or_Satisfies_Used_For_Parameter_Type_Annotation{
            .bad_keyword = assertion->as_span(),
        });
    this->visit_binding_element(assertion->child_, v, info);
    break;
  }

  // function f(x satisfies T) {}  // Invalid.
  case Expression_Kind::Satisfies: {
    auto *s = expression_cast<const Expression::Satisfies *>(ast);
    this->diags_.add(
        Diag_TypeScript_As_Or_Satisfies_Used_For_Parameter_Type_Annotation{
            .bad_keyword = s->satisfies_span(),
        });
    this->visit_binding_element(s->child_, v, info);
    break;
  }

    // function f([(p,)]) {}  // Invalid.
  case Expression_Kind::Trailing_Comma:
    this->diags_.add(Diag_Stray_Comma_In_Parameter{
        .comma =
            expression_cast<Expression::Trailing_Comma *>(ast)->comma_span(),
    });
    this->visit_binding_element(ast->child_0(), v, info);
    break;

    // function f(#bananas) {}  // Invalid.
    // function f(:) {}  // Invalid.
  case Expression_Kind::Invalid:
  case Expression_Kind::Missing:
  case Expression_Kind::Private_Variable:
    // parse_expression already reported an error. Don't report another error
    // here.
    break;

  case Expression_Kind::Call:
    this->diags_.add(Diag_Invalid_Parameter{
        .parameter = ast->span(),
    });
    break;

  // function f(param?) {}  // TypeScript only.
  // let [x?] = xs;         // Invalid.
  case Expression_Kind::Optional: {
    auto *optional = expression_cast<const Expression::Optional *>(ast);
    if (info.is_destructuring) {
      // let [x?] = xs;  // Invalid.
      this->diags_.add(Diag_Unexpected_Question_When_Destructuring{
          .question = optional->question_span(),
      });
    } else {
      // function f(param?) {}  // TypeScript only.
      if (!this->options_.typescript) {
        this->diags_.add(
            Diag_TypeScript_Optional_Parameters_Not_Allowed_In_JavaScript{
                .question = optional->question_span(),
            });
      }
    }
    this->visit_binding_element(optional->child_, v, info);
    break;
  }

    // function f([(arg)]) {}  // Invalid.
  case Expression_Kind::Paren: {
    auto paren = expression_cast<Expression::Paren *>(ast);
    this->diags_.add(Diag_Unexpected_Function_Parameter_Is_Parenthesized{
        .left_paren_to_right_paren = paren->span()});
    this->visit_binding_element(ast->child_0(), v, info);
    break;
  }

  // function f(()) {}  // Invalid.
  case Expression_Kind::Paren_Empty: {
    Expression::Paren_Empty *paren_empty =
        expression_cast<Expression::Paren_Empty *>(ast);
    paren_empty->add_missing_expression_error(this->diags_);
    break;
  }

  case Expression_Kind::Literal:
    this->diags_.add(Diag_Unexpected_Literal_In_Parameter_List{
        .literal = ast->span(),
    });
    break;

  // function f(this) {}
  case Expression_Kind::This_Variable: {
    Source_Code_Span this_span = ast->span();
    if (info.declaration_kind == Variable_Kind::_arrow_parameter &&
        this->options_.typescript) {
      this->diags_.add(Diag_This_Parameter_Not_Allowed_In_Arrow_Functions{
          .this_keyword = this_span,
      });
    } else if (info.has_spread_operator()) {
      this->diags_.add(Diag_Spread_Parameter_Cannot_Be_This{
          .this_keyword = this_span,
          .spread_operator = info.spread_operator_span(),
      });
    } else if (info.is_destructuring) {
      this->diags_.add(Diag_This_Parameter_Not_Allowed_When_Destructuring{
          .this_keyword = this_span,
      });
    } else if (!this->options_.typescript) {
      this->diags_.add(Diag_This_Parameter_Not_Allowed_In_JavaScript{
          .this_keyword = this_span,
      });
    } else if (info.first_parameter_begin != this_span.begin()) {
      this->diags_.add(Diag_This_Parameter_Must_Be_First{
          .this_keyword = this_span,
          .first_parameter_begin =
              Source_Code_Span::unit(info.first_parameter_begin),
      });
    }
    break;
  }

  // const [x]: []number = xs;
  case Expression_Kind::Type_Annotated: {
    Expression::Type_Annotated *annotated =
        expression_cast<Expression::Type_Annotated *>(ast);
    annotated->visit_type_annotation(v);
    this->visit_binding_element(annotated->child_, v, info);
    break;
  }
  }
}

Parser::Parse_Possible_Declare_Result
Parser::parse_and_visit_possible_declare_statement(Parse_Visitor_Base &v) {
  Lexer_Transaction transaction = this->lexer_.begin_transaction();
  Source_Code_Span declare_keyword_span = this->peek().span();
  this->skip();

  if (this->peek().has_leading_newline) {
    // declare  // ASI
    // enum E {}
    this->lexer_.roll_back_transaction(std::move(transaction));
    return Parse_Possible_Declare_Result::declare_is_expression_or_loop_label;
  }

  if (this->is_declare_statement_start_token(this->peek().type)) {
    // declare class C { }
    // declare import fs from 'fs';  // Invalid.
    // declare import ns = otherns;  // Invalid.
    this->lexer_.commit_transaction(std::move(transaction));
    this->parse_and_visit_declare_statement(
        v, TypeScript_Declare_Context{
               .direct_declare_keyword = declare_keyword_span,
           });
    return Parse_Possible_Declare_Result::parsed;
  } else {
    // declare:  // Label.
    // declare();
    this->lexer_.roll_back_transaction(std::move(transaction));
    return Parse_Possible_Declare_Result::declare_is_expression_or_loop_label;
  }
}

void Parser::parse_and_visit_declare_statement(
    Parse_Visitor_Base &v, const TypeScript_Declare_Context &declare_context) {
  QLJS_ASSERT(declare_context.declare_namespace_declare_keyword.has_value() ||
              declare_context.direct_declare_keyword.has_value());

  auto maybe_visit_enter_declare_scope = [&]() -> void {
    if (declare_context.direct_declare_keyword.has_value()) {
      v.visit_enter_declare_scope();
    }
  };
  auto maybe_visit_exit_declare_scope = [&]() -> void {
    if (declare_context.direct_declare_keyword.has_value()) {
      v.visit_exit_declare_scope();
    }
  };

  Function_Attributes func_attributes = Function_Attributes::normal;
  std::optional<Source_Code_Span> async_keyword = std::nullopt;

  switch (this->peek().type) {
  // declare enum E {}
  case Token_Type::kw_enum:
    // declare enum E {}
    // is_current_typescript_namespace_non_empty_ is set by
    // parse_and_visit_typescript_enum.
    maybe_visit_enter_declare_scope();
    this->parse_and_visit_typescript_enum(v, Enum_Kind::declare_enum);
    maybe_visit_exit_declare_scope();
    break;

  // declare const enum E {}
  // declare const myVariable: any;
  case Token_Type::kw_const: {
    Token const_keyword = this->peek();
    this->skip();
    if (this->peek().type == Token_Type::kw_enum) {
      // declare const enum E {}
      // Do not set is_current_typescript_namespace_non_empty_.
      maybe_visit_enter_declare_scope();
      this->parse_and_visit_typescript_enum(v, Enum_Kind::declare_const_enum);
      maybe_visit_exit_declare_scope();
    } else {
      // declare const myVariable: any;
      this->is_current_typescript_namespace_non_empty_ = true;
      if (!this->options_.typescript) {
        this->diags_.add(Diag_Declare_Var_Not_Allowed_In_JavaScript{
            .declare_keyword = declare_context.declare_keyword_span(),
            .declaring_token = const_keyword.span(),
        });
      }
      maybe_visit_enter_declare_scope();
      this->parse_and_visit_let_bindings(
          v, Parse_Let_Bindings_Options{
                 .declaring_token = const_keyword,
                 .declare_keyword = declare_context.declare_keyword_span(),
             });
      maybe_visit_exit_declare_scope();
      this->consume_semicolon_after_statement();
    }
    break;
  }

  // declare class C {}
  case Token_Type::kw_class:
    this->is_current_typescript_namespace_non_empty_ = true;
    maybe_visit_enter_declare_scope();
    this->parse_and_visit_class(
        v, Parse_Class_Options{
               .require_name = Name_Requirement::required_for_statement,
               .abstract_keyword_span = std::nullopt,
               .declare_keyword_span = declare_context.declare_keyword_span(),
           });
    maybe_visit_exit_declare_scope();
    break;

  // declare abstract class C {}
  //
  // declare abstract
  // class C {}        // Invalid.
  case Token_Type::kw_abstract: {
    this->is_current_typescript_namespace_non_empty_ = true;
    Source_Code_Span abstract_token = this->peek().span();
    this->skip();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_class);
    if (this->peek().has_leading_newline) {
      this->diags_.add(Diag_Newline_Not_Allowed_After_Abstract_Keyword{
          .abstract_keyword = abstract_token,
      });
    }
    maybe_visit_enter_declare_scope();
    this->parse_and_visit_class(
        v, Parse_Class_Options{
               .require_name = Name_Requirement::required_for_statement,
               .abstract_keyword_span = abstract_token,
               .declare_keyword_span = declare_context.declare_keyword_span(),
           });
    maybe_visit_exit_declare_scope();
    break;
  }

  // declare var x;
  // declare let y, z;
  case Token_Type::kw_let:
  case Token_Type::kw_var: {
    this->is_current_typescript_namespace_non_empty_ = true;
    if (!this->options_.typescript) {
      this->diags_.add(Diag_Declare_Var_Not_Allowed_In_JavaScript{
          .declare_keyword = declare_context.declare_keyword_span(),
          .declaring_token = this->peek().span(),
      });
    }
    Token declaring_token = this->peek();
    this->skip();
    maybe_visit_enter_declare_scope();
    this->parse_and_visit_let_bindings(
        v, Parse_Let_Bindings_Options{
               .declaring_token = declaring_token,
               .declare_keyword = declare_context.declare_keyword_span(),
           });
    maybe_visit_exit_declare_scope();
    this->consume_semicolon_after_statement();
    break;
  }

  // declare type T = U;
  case Token_Type::kw_type: {
    // Do not set is_current_typescript_namespace_non_empty_.
    Source_Code_Span type_keyword_span = this->peek().span();
    this->skip();
    this->parse_and_visit_typescript_type_alias(v, type_keyword_span);
    break;
  }

  // declare interface I { }
  case Token_Type::kw_interface: {
    // Do not set is_current_typescript_namespace_non_empty_.
    Source_Code_Span interface_keyword_span = this->peek().span();
    this->skip();
    this->parse_and_visit_typescript_interface(v, interface_keyword_span);
    break;
  }

  // declare async function f(); // Invalid.
  case Token_Type::kw_async:
    this->is_current_typescript_namespace_non_empty_ = true;
    async_keyword = this->peek().span();
    if (this->options_.typescript_definition_file) {
      this->diags_.add(Diag_DTS_Function_Cannot_Be_Async{
          .async_keyword = *async_keyword,
      });
    } else {
      this->diags_.add(Diag_Declare_Function_Cannot_Be_Async{
          .async_keyword = *async_keyword,
      });
    }
    this->skip();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_function);
    func_attributes = Function_Attributes::async;
    goto parse_declare_function;

  // declare function f();
  parse_declare_function:
  case Token_Type::kw_function:
    this->is_current_typescript_namespace_non_empty_ = true;
    maybe_visit_enter_declare_scope();
    this->parse_and_visit_function_declaration(
        v, Function_Declaration_Options{
               .attributes = func_attributes,
               .begin = async_keyword.has_value() ? async_keyword->begin()
                                                  : this->peek().begin,
               .require_name = Name_Requirement::required_for_statement,
               .async_keyword = async_keyword,
               .declare_keyword = declare_context.declare_keyword_span(),
           });
    maybe_visit_exit_declare_scope();
    break;

  // declare namespace ns {}
  case Token_Type::kw_module:
  case Token_Type::kw_namespace:
    // is_current_typescript_namespace_non_empty_ is set by
    // parse_and_visit_typescript_declare_namespace_or_module if necessary.
    maybe_visit_enter_declare_scope();
    this->parse_and_visit_typescript_declare_namespace_or_module(
        v, declare_context.declare_keyword_span());
    maybe_visit_exit_declare_scope();
    break;

  // export declare import a = b;                 // Invalid.
  // declare import a from 'b';                   // Invalid.
  // declare namespace ns { import a = b; }
  // declare namespace ns { import a from "b"; }  // Invalid.
  case Token_Type::kw_import:
    // NOTE[declare-import]: There are several cases for 'declare' and 'import':
    //
    // * 'declare namespace ns { import a = b; }':
    //   This code is legal.
    //
    // * 'declare import a from "b";':
    //   We report Diag_Import_Cannot_Have_Declare_Keyword below.
    //
    // * 'declare namespace ns { import a from "b"; }':
    //   parse_and_visit_import (called below) reports
    //   Diag_Declare_Namespace_Cannot_Import_Module.
    //
    // * 'export declare import a from "b";':
    //   We report Diag_Import_Cannot_Have_Declare_Keyword below.
    if (declare_context.direct_declare_keyword.has_value()) {
      this->diags_.add(Diag_Import_Cannot_Have_Declare_Keyword{
          .declare_keyword = *declare_context.direct_declare_keyword,
      });
    }
    // is_current_typescript_namespace_non_empty_ is set by
    // parse_and_visit_import if necessary.
    this->parse_and_visit_import(v, declare_context);
    break;

  // declare global { /* ... */ }
  case Token_Type::kw_global:
    this->parse_and_visit_declare_global(v, declare_context);
    break;

  // declare:               // Label.
  // declare();
  // export declare export  // Invalid?
  case Token_Type::colon:
  case Token_Type::left_paren:
  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

void Parser::parse_and_visit_declare_global(
    Parse_Visitor_Base &v, const TypeScript_Declare_Context &declare_context) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_global);
  Source_Code_Span global_keyword_span = this->peek().span();
  this->skip();

  if (!this->options_.typescript) {
    this->diags_.add(Diag_TypeScript_Global_Block_Not_Allowed_In_JavaScript{
        .global_keyword = global_keyword_span,
    });
  }
  if (this->in_typescript_namespace_or_module_.has_value() &&
      !this->in_typescript_module_) {
    this->diags_.add(Diag_TypeScript_Global_Block_Not_Allowed_In_Namespace{
        .global_keyword = global_keyword_span,
        .namespace_keyword = *this->in_typescript_namespace_or_module_,
    });
  }
  v.visit_enter_declare_global_scope();
  this->parse_and_visit_typescript_declare_block(
      v, TypeScript_Declare_Context{
             .declare_namespace_declare_keyword =
                 declare_context.declare_keyword_span(),
             .direct_declare_keyword = std::nullopt,
             .in_module = false,
         });
  v.visit_exit_declare_global_scope();
}

bool Parser::is_declare_statement_start_token(Token_Type token_type) {
  switch (token_type) {
  case Token_Type::kw_abstract:
  case Token_Type::kw_async:
  case Token_Type::kw_class:
  case Token_Type::kw_const:
  case Token_Type::kw_enum:
  case Token_Type::kw_function:
  case Token_Type::kw_global:
  case Token_Type::kw_import:
  case Token_Type::kw_interface:
  case Token_Type::kw_let:
  case Token_Type::kw_module:
  case Token_Type::kw_namespace:
  case Token_Type::kw_type:
  case Token_Type::kw_var:
    return true;
  default:
    return false;
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
