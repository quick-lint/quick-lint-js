// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
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
#include <utility>

namespace quick_lint_js {
void Parser::parse_typescript_colon_for_type() {
  QLJS_ASSERT(this->peek().type == Token_Type::colon);
  if (!this->options_.typescript && !this->in_typescript_only_construct_) {
    this->diag_reporter_->report(
        Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript{
            .type_colon = this->peek().span(),
        });
  }
  this->skip();
}

void Parser::parse_and_visit_typescript_colon_type_expression(
    Parse_Visitor_Base &v) {
  this->parse_and_visit_typescript_colon_type_expression(
      v, TypeScript_Type_Parse_Options());
}

void Parser::parse_and_visit_typescript_colon_type_expression(
    Parse_Visitor_Base &v, const TypeScript_Type_Parse_Options &parse_options) {
  this->parse_typescript_colon_for_type();
  this->parse_and_visit_typescript_type_expression(v, parse_options);
}

void Parser::parse_and_visit_typescript_type_expression(Parse_Visitor_Base &v) {
  v.visit_enter_type_scope();
  this->parse_and_visit_typescript_type_expression_no_scope(
      v, TypeScript_Type_Parse_Options());
  v.visit_exit_type_scope();
}

void Parser::parse_and_visit_typescript_type_expression(
    Parse_Visitor_Base &v, const TypeScript_Type_Parse_Options &parse_options) {
  v.visit_enter_type_scope();
  this->parse_and_visit_typescript_type_expression_no_scope(v, parse_options);
  v.visit_exit_type_scope();
}

void Parser::parse_and_visit_typescript_type_expression_no_scope(
    Parse_Visitor_Base &v) {
  this->parse_and_visit_typescript_type_expression_no_scope(
      v, TypeScript_Type_Parse_Options());
}

void Parser::parse_and_visit_typescript_type_expression_no_scope(
    Parse_Visitor_Base &v, const TypeScript_Type_Parse_Options &parse_options) {
  Depth_Guard guard(this);
  TypeScript_Only_Construct_Guard ts_guard =
      this->enter_typescript_only_construct();

  bool is_array_type = false;
  bool is_tuple_type = false;

  std::optional<Source_Code_Span> leading_binary_operator;  // '|' or '&'
  if (this->peek().type == Token_Type::ampersand ||
      this->peek().type == Token_Type::pipe) {
    // | Type
    // & Type
    leading_binary_operator = this->peek().span();
    this->skip();
  }

  auto maybe_parse_dots_after_generic_arguments = [this]() -> void {
    while (this->peek().type == Token_Type::dot) {
      Source_Code_Span dot_span = this->peek().span();
      this->skip();
      switch (this->peek().type) {
      QLJS_CASE_KEYWORD:
      case Token_Type::identifier:
        this->diag_reporter_->report(
            Diag_Dot_Not_Allowed_After_Generic_Arguments_In_Type{
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
  };

again:
  std::optional<Source_Code_Span> readonly_keyword;
  if (this->peek().type == Token_Type::kw_readonly) {
    // readonly Type[]
    // readonly [Type, Type]
    // readonly is Type
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    Identifier readonly = this->peek().identifier_name();
    this->skip();

    if (parse_options.allow_assertion_signature_or_type_predicate &&
        this->peek().type == Token_Type::kw_is) {
      // readonly is Type
      this->lexer_.roll_back_transaction(std::move(transaction));
      goto type_variable_or_namespace_or_type_predicate;
    }
    this->lexer_.commit_transaction(std::move(transaction));
    readonly_keyword = readonly.span();
  }

  switch (this->peek().type) {
  case Token_Type::complete_template:
  case Token_Type::kw_false:
  case Token_Type::kw_null:
  case Token_Type::kw_true:
  case Token_Type::kw_void:
  case Token_Type::number:
  case Token_Type::string:
    this->skip();
    break;

  // -42
  case Token_Type::minus:
    this->skip();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::number);
    this->skip();
    break;

  // any
  // any is Type
  case Token_Type::kw_any:
  case Token_Type::kw_bigint:
  case Token_Type::kw_boolean:
  case Token_Type::kw_never:
  case Token_Type::kw_number:
  case Token_Type::kw_object:
  case Token_Type::kw_string:
  case Token_Type::kw_symbol:
  case Token_Type::kw_undefined:
  case Token_Type::kw_unknown: {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    if (this->peek().type == Token_Type::kw_is) {
      // param is Type
      this->lexer_.roll_back_transaction(std::move(transaction));
      goto type_variable_or_namespace_or_type_predicate;
    }
    this->lexer_.commit_transaction(std::move(transaction));
    break;
  }

  // this
  // this is Type
  case Token_Type::kw_this: {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    if (this->peek().type == Token_Type::kw_is) {
      // this is Type
      this->lexer_.roll_back_transaction(std::move(transaction));
      goto type_variable_or_namespace_or_type_predicate;
    }
    this->lexer_.commit_transaction(std::move(transaction));
    break;
  }

  // `template ${sometype}`
  case Token_Type::incomplete_template:
    this->parse_and_visit_typescript_template_type_expression(
        v, TypeScript_Type_Parse_Options{
               .type_being_declared = parse_options.type_being_declared,
           });
    break;

  //: ?Type // invalid
  case Token_Type::question:
    this->diag_reporter_->report(
        Diag_TypeScript_Question_In_Type_Expression_Should_Be_Void{
            .question = this->peek().span()});
    this->skip();
    goto again;

  // Type
  // ns.Type<T>
  // param is Type
  type_variable_or_namespace_or_type_predicate:
  case Token_Type::kw_accessor:
  case Token_Type::kw_as:
  case Token_Type::kw_assert:
  case Token_Type::kw_async:
  case Token_Type::kw_await:
  case Token_Type::kw_constructor:
  case Token_Type::kw_declare:
  case Token_Type::kw_from:
  case Token_Type::kw_get:
  case Token_Type::kw_global:
  case Token_Type::kw_implements:
  case Token_Type::kw_interface:
  case Token_Type::kw_intrinsic:
  case Token_Type::kw_is:
  case Token_Type::kw_let:
  case Token_Type::kw_module:
  case Token_Type::kw_namespace:
  case Token_Type::kw_of:
  case Token_Type::kw_out:
  case Token_Type::kw_override:
  case Token_Type::kw_package:
  case Token_Type::kw_private:
  case Token_Type::kw_protected:
  case Token_Type::kw_public:
  case Token_Type::kw_readonly:
  case Token_Type::kw_require:
  case Token_Type::kw_satisfies:
  case Token_Type::kw_set:
  case Token_Type::kw_static:
  case Token_Type::kw_type:
  case Token_Type::kw_yield:
  case Token_Type::identifier: {
    Token_Type name_type = this->peek().type;
    Identifier name = this->peek().identifier_name();
    this->skip();
    if (this->peek().type == Token_Type::kw_is) {
      // param is Type
      // this is Type
      if (this->peek().has_leading_newline) {
        if (parse_options.stop_parsing_type_at_newline_before_is) {
          // interface I {
          //   f(): p       // ASI.
          //   is();
          // }
          break;
        } else {
          this->diag_reporter_->report(
              Diag_Newline_Not_Allowed_Before_Is_In_Assertion_Signature{
                  .is_keyword = this->peek().span(),
              });
        }
      }
      Source_Code_Span is_keyword = this->peek().span();
      this->skip();
      if (name_type != Token_Type::kw_this) {
        // this is Type
        if (parse_options.allow_assertion_signature_or_type_predicate) {
          v.visit_variable_type_predicate_use(name);
        } else {
          v.visit_variable_use(name);
        }
      }
      if (!parse_options.allow_assertion_signature_or_type_predicate) {
        this->diag_reporter_->report(
            Diag_TypeScript_Type_Predicate_Only_Allowed_As_Return_Type{
                .is_keyword = is_keyword,
            });
      }
      this->parse_and_visit_typescript_type_expression_no_scope(v);
      return;
    }

    bool had_dot = false;
    while (this->peek().type == Token_Type::dot) {
      // ns.Type
      had_dot = true;
      this->skip();
      switch (this->peek().type) {
      QLJS_CASE_KEYWORD:
      case Token_Type::identifier:
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
    if (this->peek().type == Token_Type::less ||
        this->peek().type == Token_Type::less_less) {
      if (parse_options.stop_parsing_type_at_newline_before_generic_arguments &&
          this->peek().has_leading_newline) {
        // let x: MyType /* ASI */
        // <h1></h1>;
      } else {
        if (this->peek().has_leading_newline) {
          this->diag_reporter_->report(
              Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type{
                  .less = Source_Code_Span(this->peek().begin,
                                           this->peek().begin + 1),
              });
        }
        this->parse_and_visit_typescript_generic_arguments_no_scope(v);
      }
    }

    if (parse_options.type_being_declared.has_value() &&
        this->peek().type != Token_Type::left_square && !had_dot) {
      // Shallow use.
      if (name.normalized_name() ==
          parse_options.type_being_declared->name.normalized_name()) {
        // type T = T;  // Invalid
        // NOTE[TypeScript-cyclic-type]:
        this->diag_reporter_->report(Diag_Cyclic_TypeScript_Type_Definition{
            .use = name.span(),
            .declaration = parse_options.type_being_declared->name.span(),
            .kind = parse_options.type_being_declared->kind,
        });
      }
    }
    break;
  }

  // asserts
  // asserts param
  // asserts param is Type
  case Token_Type::kw_asserts: {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    Source_Code_Span asserts_keyword = this->peek().span();
    this->skip();
    if (this->peek().has_leading_newline) {
      if (parse_options.stop_parsing_type_at_newline_after_asserts) {
        // interface I {
        //   f(): asserts  // ASI.
        //   notParam();
        // }
        this->lexer_.roll_back_transaction(std::move(transaction));
        goto type_variable_or_namespace_or_type_predicate;
      } else {
        this->diag_reporter_->report(
            Diag_Newline_Not_Allowed_After_Asserts_In_Assertion_Signature{
                .asserts_keyword = asserts_keyword,
            });
      }
    }
    switch (this->peek().type) {
    // asserts param
    // asserts param is Type
    QLJS_CASE_CONTEXTUAL_KEYWORD:
    case Token_Type::kw_this:
    case Token_Type::identifier:
      if (this->peek().type == Token_Type::kw_is) {
        // asserts is Type     // Type predicate for parameter 'asserts'.
        // asserts is is Type  // Invalid.
        this->lexer_.roll_back_transaction(std::move(transaction));
        goto type_variable_or_namespace_or_type_predicate;
      }

      this->lexer_.commit_transaction(std::move(transaction));
      if (this->peek().type == Token_Type::kw_this) {
        // TODO(#881): Only allow 'this' within class and interface method
        // signatures.
      } else {
        if (parse_options.allow_assertion_signature_or_type_predicate) {
          v.visit_variable_assertion_signature_use(
              this->peek().identifier_name());
        } else {
          v.visit_variable_use(this->peek().identifier_name());
        }
      }
      if (!parse_options.allow_assertion_signature_or_type_predicate) {
        this->diag_reporter_->report(
            Diag_TypeScript_Assertion_Signature_Only_Allowed_As_Return_Types{
                .asserts_keyword = asserts_keyword,
            });
      }
      this->skip();
      if (this->peek().type == Token_Type::kw_is) {
        // asserts param is Type
        this->skip();
        goto again;  // Parse a type.
      }
      return;

    // asserts  // Parameter name.
    default:
      this->lexer_.roll_back_transaction(std::move(transaction));
      goto type_variable_or_namespace_or_type_predicate;
    }
    break;
  }

  // infer T  // Invalid.
  // T extends infer U ? V : W
  // T extends infer U extends X ? V : W
  case Token_Type::kw_infer: {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    Source_Code_Span infer_keyword_span = this->peek().span();
    this->skip();
    switch (this->peek().type) {
    case Token_Type::identifier:
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
    case Token_Type::kw_undefined:
    case Token_Type::kw_unique:
      break;

    // infer is       // 'is' is the declared name.
    // infer is Type  // Type predicate where 'infer' is the parameter name.
    case Token_Type::kw_is:
      if (parse_options.allow_assertion_signature_or_type_predicate) {
        // infer is Type
        this->lexer_.roll_back_transaction(std::move(transaction));
        goto type_variable_or_namespace_or_type_predicate;
      }
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    this->lexer_.commit_transaction(std::move(transaction));
    Identifier variable = this->peek().identifier_name();
    this->skip();

    if (this->typescript_infer_declaration_buffer_ == nullptr) {
      this->diag_reporter_->report(
          Diag_TypeScript_Infer_Outside_Conditional_Type{
              .infer_keyword = infer_keyword_span,
          });
    } else {
      this->typescript_infer_declaration_buffer_->visit_variable_declaration(
          variable, Variable_Kind::_infer_type,
          Variable_Declaration_Flags::none);
    }

    if (this->peek().type == Token_Type::left_square) {
      // T extends infer U[] ? V : W  // Invalid.
      this->diag_reporter_->report(Diag_TypeScript_Infer_Requires_Parentheses{
          .infer_and_type = Source_Code_Span(infer_keyword_span.begin(),
                                             variable.span().end()),
          .type = variable.span(),
      });
    }

    // Parse infer constraint.
    if (this->peek().type == Token_Type::kw_extends) {
      // T extends infer U extends X ? V : W
      //                   ^^^^^^^
      // T extends (infer U extends X ? V : W) ? Y : Z
      //                    ^^^^^^^
      Parser_Transaction extends_transaction = this->begin_transaction();
      Stacked_Buffering_Visitor extends_type_visitor =
          this->buffering_visitor_stack_.push();
      this->skip();
      this->parse_and_visit_typescript_type_expression_no_scope(
          extends_type_visitor.visitor(),
          TypeScript_Type_Parse_Options{
              .parse_question_as_invalid = false,
          });
      if (this->peek().type == Token_Type::question &&
          parse_options.extends_is_conditional_type) {
        // T extends (infer U extends X ? V : W) ? Y : Z
        //                              ^
        // The '?' belongs to a separate 'extends' expression, as if the
        // following was written instead:
        //   T extends ((infer U) extends X ? V : W) ? Y : Z
        this->roll_back_transaction(std::move(extends_transaction));
      } else {
        this->commit_transaction(std::move(extends_transaction));
        extends_type_visitor.visitor().move_into(v);
      }
    }
    break;
  }

  // unique
  // unique symbol
  // unique is Type
  case Token_Type::kw_unique: {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    if (parse_options.allow_assertion_signature_or_type_predicate &&
        this->peek().type == Token_Type::kw_is) {
      // unique is Type
      this->lexer_.roll_back_transaction(std::move(transaction));
      goto type_variable_or_namespace_or_type_predicate;
    }
    this->lexer_.commit_transaction(std::move(transaction));
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_symbol);
    this->skip();
    break;
  }

  // [A, B, C]
  case Token_Type::left_square:
    is_tuple_type = true;
    this->parse_and_visit_typescript_tuple_type_expression(v);
    break;

  // (typeexpr)
  // (param, param) => ReturnType
  case Token_Type::left_paren:
    this->parse_and_visit_typescript_arrow_or_paren_type_expression(
        v, TypeScript_Type_Parse_Options{
               .type_being_declared = parse_options.type_being_declared,
           });
    break;

  // new (param, param) => ReturnType
  case Token_Type::kw_new:
    this->skip();
    this->parse_and_visit_typescript_arrow_type_expression(v);
    break;

  // abstract
  // abstract new (param, param) => ReturnType
  case Token_Type::kw_abstract: {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    switch (this->peek().type) {
    // abstract new (param, param) => ReturnType
    case Token_Type::kw_new:
      this->lexer_.commit_transaction(std::move(transaction));
      this->skip();
      this->parse_and_visit_typescript_arrow_type_expression(v);
      break;

    // type T = abstract /*ASI*/ (param, param) => body;
    // abstract (param, param) => ReturnType  // Invalid.
    case Token_Type::left_paren:
      if (this->peek().has_leading_newline) {
        // ASI.
        this->lexer_.roll_back_transaction(std::move(transaction));
        goto type_variable_or_namespace_or_type_predicate;
      }
      // Missing 'new' keyword.
      this->lexer_.commit_transaction(std::move(transaction));
      this->diag_reporter_->report(
          Diag_Missing_New_In_Abstract_Constructor_Type{
              .expected_new = Source_Code_Span::unit(this->peek().begin),
          });
      this->parse_and_visit_typescript_arrow_type_expression(v);
      break;

    // type T = abstract;
    // [abstract]
    default:
      this->lexer_.roll_back_transaction(std::move(transaction));
      goto type_variable_or_namespace_or_type_predicate;
    }
    break;
  }

  // <T>(param, param) => ReturnType
  case Token_Type::less:
    this->parse_and_visit_typescript_arrow_type_expression(v);
    break;

  // { key: value }
  case Token_Type::left_curly:
    this->parse_and_visit_typescript_object_type_expression(v);
    break;

  // & & Type  // Invalid.
  // | | Type  // Invalid.
  case Token_Type::ampersand:
  case Token_Type::pipe:
    this->diag_reporter_->report(
        Diag_Missing_Type_Between_Intersection_Or_Union{
            .left_operator = leading_binary_operator.value(),
            .right_operator = this->peek().span(),
        });
    break;

  // typeof varname
  // typeof import("modulename")
  // typeof ns.varname[KeyType]
  // typeof varname[]
  // typeof MyClass<T>
  case Token_Type::kw_typeof:
    this->skip();
    switch (this->peek().type) {
    case Token_Type::identifier:
    case Token_Type::kw_abstract:
    case Token_Type::kw_accessor:
    case Token_Type::kw_as:
    case Token_Type::kw_assert:
    case Token_Type::kw_asserts:
    case Token_Type::kw_async:
    case Token_Type::kw_await:
    case Token_Type::kw_bigint:
    case Token_Type::kw_boolean:
    case Token_Type::kw_break:
    case Token_Type::kw_case:
    case Token_Type::kw_catch:
    case Token_Type::kw_class:
    case Token_Type::kw_const:
    case Token_Type::kw_constructor:
    case Token_Type::kw_continue:
    case Token_Type::kw_debugger:
    case Token_Type::kw_declare:
    case Token_Type::kw_default:
    case Token_Type::kw_delete:
    case Token_Type::kw_do:
    case Token_Type::kw_else:
    case Token_Type::kw_enum:
    case Token_Type::kw_export:
    case Token_Type::kw_extends:
    case Token_Type::kw_false:
    case Token_Type::kw_finally:
    case Token_Type::kw_for:
    case Token_Type::kw_from:
    case Token_Type::kw_function:
    case Token_Type::kw_get:
    case Token_Type::kw_global:
    case Token_Type::kw_if:
    case Token_Type::kw_in:
    case Token_Type::kw_infer:
    case Token_Type::kw_instanceof:
    case Token_Type::kw_intrinsic:
    case Token_Type::kw_is:
    case Token_Type::kw_keyof:
    case Token_Type::kw_let:
    case Token_Type::kw_module:
    case Token_Type::kw_namespace:
    case Token_Type::kw_new:
    case Token_Type::kw_null:
    case Token_Type::kw_number:
    case Token_Type::kw_object:
    case Token_Type::kw_of:
    case Token_Type::kw_out:
    case Token_Type::kw_override:
    case Token_Type::kw_readonly:
    case Token_Type::kw_require:
    case Token_Type::kw_return:
    case Token_Type::kw_satisfies:
    case Token_Type::kw_set:
    case Token_Type::kw_static:
    case Token_Type::kw_string:
    case Token_Type::kw_super:
    case Token_Type::kw_switch:
    case Token_Type::kw_symbol:
    case Token_Type::kw_throw:
    case Token_Type::kw_true:
    case Token_Type::kw_try:
    case Token_Type::kw_type:
    case Token_Type::kw_typeof:
    case Token_Type::kw_undefined:
    case Token_Type::kw_unique:
    case Token_Type::kw_var:
    case Token_Type::kw_void:
    case Token_Type::kw_while:
    case Token_Type::kw_with:
    case Token_Type::kw_yield:
      v.visit_variable_use(this->peek().identifier_name());
      this->skip();
      break;

    // typeof this
    // typeof this.prop
    case Token_Type::kw_this:
      this->skip();
      break;

    // typeof import("modulename")
    case Token_Type::kw_import:
      this->parse_and_visit_typescript_import_type_expression(v);
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }

    while (this->peek().type == Token_Type::dot) {
      this->skip();
      switch (this->peek().type) {
      QLJS_CASE_KEYWORD:
      case Token_Type::identifier:
      case Token_Type::private_identifier:
        this->skip();
        break;
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
    if (!this->peek().has_leading_newline &&
        (this->peek().type == Token_Type::less ||
         this->peek().type == Token_Type::less_less)) {
      // typeof C<T>
      // typeof C< <T>() => RetType>
      if (this->peek().type == Token_Type::less_less) {
        // typeof C<<T>() => RetType>  // Invalid.
        const Char8 *second_less = this->peek().begin + 1;
        this->diag_reporter_->report(
            Diag_TypeScript_Generic_Less_Less_Not_Split{
                .expected_space = Source_Code_Span::unit(second_less),
                .context = Statement_Kind::typeof_type,
            });
      }
      this->parse_and_visit_typescript_generic_arguments_no_scope(v);
    }
    maybe_parse_dots_after_generic_arguments();
    break;

  // keyof Type
  // keyof is Type
  case Token_Type::kw_keyof: {
    Lexer_Transaction transaction = this->lexer_.begin_transaction();
    this->skip();
    if (parse_options.allow_assertion_signature_or_type_predicate &&
        this->peek().type == Token_Type::kw_is) {
      // keyof is Type
      this->lexer_.roll_back_transaction(std::move(transaction));
      goto type_variable_or_namespace_or_type_predicate;
    }
    // keyof Type
    this->lexer_.commit_transaction(std::move(transaction));
    this->parse_and_visit_typescript_type_expression_no_scope(v, parse_options);
    break;
  }

  // import("module").Name
  case Token_Type::kw_import:
    this->parse_and_visit_typescript_import_type_expression(v);
    while (this->peek().type == Token_Type::dot) {
      this->skip();
      switch (this->peek().type) {
      QLJS_CASE_KEYWORD:
      case Token_Type::identifier:
        this->skip();
        break;
      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
      }
    }
    if (!this->peek().has_leading_newline &&
        (this->peek().type == Token_Type::less ||
         this->peek().type == Token_Type::less_less)) {
      this->parse_and_visit_typescript_generic_arguments_no_scope(v);
    }
    maybe_parse_dots_after_generic_arguments();
    break;

  case Token_Type::comma:
  case Token_Type::end_of_file:
  case Token_Type::right_curly:
  case Token_Type::right_paren:
  default:
    this->diag_reporter_->report(Diag_Missing_TypeScript_Type{
        .expected_type = Source_Code_Span::unit(this->peek().begin),
    });
    break;
  }

  //: Type? // invalid
  if (!is_tuple_type && parse_options.parse_question_as_invalid &&
      this->peek().type == Token_Type::question) {
    this->diag_reporter_->report(
        Diag_TypeScript_Question_In_Type_Expression_Should_Be_Void{
            .question = this->peek().span()});
    this->skip();
  }

  while (this->peek().type == Token_Type::left_square &&
         !this->peek().has_leading_newline) {
    // typeexpr[]
    // typeexpr[Key]
    this->skip();
    if (this->peek().type == Token_Type::right_square) {
      is_array_type = true;
      this->skip();
    } else {
      this->parse_and_visit_typescript_type_expression_no_scope(v);
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_square);
      this->skip();
    }
  }
  if (readonly_keyword.has_value() && !(is_array_type || is_tuple_type)) {
    this->diag_reporter_->report(
        Diag_TypeScript_Readonly_In_Type_Needs_Array_Or_Tuple_Type{
            .readonly_keyword = *readonly_keyword,
        });
  }

  if (this->peek().type == Token_Type::ampersand ||
      this->peek().type == Token_Type::pipe) {
    // Type1 | Type2
    leading_binary_operator = this->peek().span();
    this->skip();
    goto again;
  }

  if (parse_options.extends_is_conditional_type &&
      this->peek().type == Token_Type::kw_extends) {
    // T extends T ? T : T
    if (this->peek().has_leading_newline) {
      if (parse_options.stop_parsing_type_at_newline_before_extends) {
        // interface I {
        //   a: T  // ASI.
        //   extends: U;
        // }
        return;
      }
      // type T = U
      // extends B ? A : B;   // Invalid.
      this->diag_reporter_->report(
          Diag_Newline_Not_Allowed_Before_Extends_In_Type{
              .extends_keyword = this->peek().span(),
          });
    }
    this->skip();

    // NOTE[TypeScript-extends-cycle]: TypeScript allows a cycle syntactically
    // if the cycling branch isn't taken. In the following example, there is no
    // cycle because 'number extends string' is always false:
    //
    //   // Equivalent to: type T = Taco;
    //   type T = number extends string ? T : Taco;
    //
    // Be conservative and don't diagnose cycles with 'extends' in the branches.

    Stacked_Buffering_Visitor infer_visitor =
        this->buffering_visitor_stack_.push();

    Buffering_Visitor *old_typescript_infer_declaration_buffer =
        this->typescript_infer_declaration_buffer_;
    this->fatal_parse_error_stack_.try_finally(
        [&]() -> void {
          this->typescript_infer_declaration_buffer_ = &infer_visitor.visitor();
          this->parse_and_visit_typescript_type_expression_no_scope(
              v, TypeScript_Type_Parse_Options{
                     .type_being_declared = parse_options.type_being_declared,
                     .parse_question_as_invalid = false,
                     .extends_is_conditional_type = false,
                 });
        },
        [&]() -> void {
          this->typescript_infer_declaration_buffer_ =
              old_typescript_infer_declaration_buffer;
        });

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::question);
    this->skip();

    v.visit_enter_conditional_type_scope();
    infer_visitor.visitor().move_into(v);
    this->parse_and_visit_typescript_type_expression_no_scope(
        v, TypeScript_Type_Parse_Options{
               // See NOTE[TypeScript-extends-cycle].
               .type_being_declared = std::nullopt,
           });
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::colon);
    v.visit_exit_conditional_type_scope();

    this->skip();
    this->parse_and_visit_typescript_type_expression_no_scope(
        v, TypeScript_Type_Parse_Options{
               // See NOTE[TypeScript-extends-cycle].
               .type_being_declared = std::nullopt,
               // This must be false to handle an edge case involving nested
               // 'extends' with function types:
               //   A extends () => B extends C ? D : E ? F : G
               .parse_question_as_invalid = false,
           });
  }
}

void Parser::parse_and_visit_typescript_arrow_type_expression(
    Parse_Visitor_Base &v) {
  v.visit_enter_function_scope();
  if (this->peek().type == Token_Type::less) {
    this->parse_and_visit_typescript_generic_parameters(v);
  }
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::left_paren);
  this->skip();
  this->parse_and_visit_typescript_arrow_type_expression_after_left_paren_no_scope(
      v);
  v.visit_exit_function_scope();
}

void Parser::parse_and_visit_typescript_arrow_type_expression_after_left_paren(
    Parse_Visitor_Base &v) {
  v.visit_enter_function_scope();
  this->parse_and_visit_typescript_arrow_type_expression_after_left_paren_no_scope(
      v);
  v.visit_exit_function_scope();
}

void Parser::
    parse_and_visit_typescript_arrow_type_expression_after_left_paren_no_scope(
        Parse_Visitor_Base &v) {
  this->parse_and_visit_function_parameters(
      v, Variable_Kind::_function_type_parameter, Parameter_List_Options{});
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_paren);
  this->skip();
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::equal_greater);
  this->parse_and_visit_typescript_arrow_type_arrow_and_return_type_no_scope(v);
}

void Parser::
    parse_and_visit_typescript_arrow_type_arrow_and_return_type_no_scope(
        Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::equal_greater);
  this->skip();
  // visit_enter_type_scope/visit_exit_type_scope is necessary. See
  // NOTE[type-predicate-type-scope].
  this->parse_and_visit_typescript_type_expression(
      v, TypeScript_Type_Parse_Options{
             // TODO(strager): Report
             // Diag_TypeScript_Question_In_Type_Expression_Should_Be_Void (i.e.
             // set parse_question_as_invalid to true) if we are not inside an
             // 'extends' clause.
             .parse_question_as_invalid = false,
             .allow_assertion_signature_or_type_predicate = true,
         });
}

Parser::TypeScript_Type_Arrow_Or_Paren
Parser::parse_and_visit_typescript_arrow_or_paren_type_expression(
    Parse_Visitor_Base &v, const TypeScript_Type_Parse_Options &parse_options) {
  QLJS_ASSERT(this->peek().type == Token_Type::left_paren);
  this->skip();

  // TypeScript's grammar is ambiguous. This could be a parenthesized type (such
  // as '(number)') or an arrow type (such as '(p) => string').
  //
  // Resolve the ambiguity by parsing the parenthesized thing as an arrow type's
  // parameter list. If successful, and if '=>' follows, this is an arrow type.
  // Otherwise, this is a parenthesized type.

  TypeScript_Type_Arrow_Or_Paren result = TypeScript_Type_Arrow_Or_Paren::arrow;
  this->try_parse(
      [&](Parser_Transaction &transaction) {
        Stacked_Buffering_Visitor params_visitor =
            this->buffering_visitor_stack_.push();
        bool parsed_function_parameters =
            this->catch_fatal_parse_errors([&]() -> void {
              this->parse_and_visit_function_parameters(
                  params_visitor.visitor(),
                  Variable_Kind::_function_type_parameter,
                  Parameter_List_Options{});
            });
        if (!parsed_function_parameters) {
          return false;
        }
        if (this->peek().type != Token_Type::right_paren) {
          return false;
        }
        this->skip();
        if (this->peek().type != Token_Type::equal_greater) {
          return false;
        }

        if (transaction.reporter.reported_any_diagnostic_except({
                Diag_Type::
                    Diag_Optional_Parameter_Cannot_Be_Followed_By_Required_Parameter,
                Diag_Type::Diag_This_Parameter_Must_Be_First,
            })) {
          return false;
        }

        v.visit_enter_function_scope();
        params_visitor.visitor().move_into(v);
        // We parse '=>' and the return type, then emit
        // visit_exit_function_scope, outside this try_parse.
        return true;
      },
      [&] {
        result = TypeScript_Type_Arrow_Or_Paren::paren;
        this->parse_and_visit_typescript_type_expression_no_scope(
            v, TypeScript_Type_Parse_Options{
                   .type_being_declared = parse_options.type_being_declared,
               });
        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_paren);
        this->skip();
      });
  if (result == TypeScript_Type_Arrow_Or_Paren::arrow) {
    QLJS_ASSERT(this->peek().type == Token_Type::equal_greater);
    this->parse_and_visit_typescript_arrow_type_arrow_and_return_type_no_scope(
        v);
    v.visit_exit_function_scope();
  }
  return result;
}

void Parser::parse_and_visit_typescript_import_type_expression(
    Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_import);
  this->skip();
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::left_paren);
  this->skip();
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::string);
  this->skip();
  if (this->peek().type == Token_Type::comma) {
    // typeof import("modulename", {assert: {'resolution-mode': 'require'}})
    this->skip();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::left_curly);
    this->parse_and_visit_typescript_object_type_expression(v);
  }
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_paren);
  this->skip();
}

void Parser::parse_and_visit_typescript_object_type_expression(
    Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::left_curly);
  this->skip();

  auto parse_after_property_name =
      [&](const std::optional<Source_Code_Span> &name) -> void {
    switch (this->peek().type) {
    // { prop? }
    case Token_Type::question:
      this->skip();
      break;

    // { [k: T]+? }
    // { [k: T]-? }
    case Token_Type::minus:
    case Token_Type::plus:
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::question);
      this->skip();
      break;

    default:
      break;
    }

    switch (this->peek().type) {
    // { prop: Type }
    case Token_Type::colon:
      this->skip();
      this->parse_and_visit_typescript_type_expression_no_scope(v);
      this->consume_semicolon_or_comma<
          Diag_Missing_Separator_Between_Object_Type_Entries>();
      break;

    // { method() }
    // { method<T>() }
    case Token_Type::left_paren:
    case Token_Type::less:
      v.visit_enter_function_scope();
      this->parse_and_visit_interface_function_parameters_and_body_no_scope(
          v, name, Function_Attributes::normal,
          Parameter_List_Options{
              .is_interface_method = true,
          });
      v.visit_exit_function_scope();
      break;

    end_of_property:
    case Token_Type::right_curly:
      break;

    case Token_Type::comma:
    case Token_Type::semicolon:
      this->skip();
      break;

    default:
      if (this->peek().has_leading_newline) {
        // ASI after property name.
        goto end_of_property;
      }
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  };

  for (;;) {
    switch (this->peek().type) {
    // { readonly prop: Type }
    case Token_Type::kw_readonly:
      this->skip();
      break;

    // { get prop(): Type }
    // { set prop(v: Type) }
    case Token_Type::kw_get:
    case Token_Type::kw_set:
      this->skip();
      break;

    // { -readonly [key: Type]: Type }
    // { +readonly [key: Type]: Type }
    case Token_Type::minus:
    case Token_Type::plus:
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::kw_readonly);
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
    case Token_Type::identifier:
    case Token_Type::number:
    case Token_Type::string: {
      Source_Code_Span name = this->peek().span();
      this->skip();
      parse_after_property_name(name);
      break;
    }

    // { readonly: Type }
    // { get?: Type }
    // { : }  // Invalid.
    // { ? }  // Invalid.
    case Token_Type::colon:
    case Token_Type::question: {
      // TODO(strager): Error if the previous token wasn't a modifier like
      // 'readonly' or 'get'.
      std::optional<Source_Code_Span> modifier_span;  // TODO(strager)
      parse_after_property_name(modifier_span);
      break;
    }

    // { [expr] }
    // { [expr]: Type }
    // { [expr](): Type }
    case Token_Type::left_square: {
      this->skip();

      bool is_index_signature = false;

      switch (this->peek().type) {
      // { [varname]: Type }
      // { [key: Type]: Type }
      // { [Key in Type]: Type }
      // TODO(#765): QLJS_CASE_CONTEXTUAL_KEYWORD overmatches. 'let' and
      // 'static' should error instead.
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case Token_Type::identifier: {
        Token_Type ident_token_type = this->peek().type;
        Identifier ident = this->peek().identifier_name();
        this->skip();
        switch (this->peek().type) {
        // { [key: Type]: Type }
        case Token_Type::colon:
          this->skip();
          is_index_signature = true;
          v.visit_enter_index_signature_scope();
          this->parse_and_visit_typescript_type_expression_no_scope(v);
          v.visit_variable_declaration(
              ident, Variable_Kind::_index_signature_parameter,
              Variable_Declaration_Flags::none);
          // TODO(strager): Report Diag_TypeScript_Index_Signature_Needs_Type if
          // ':' is missing.
          break;

        // { [key in Type]: Type }
        case Token_Type::kw_in:
          this->skip();
          is_index_signature = true;
          v.visit_enter_index_signature_scope();
          this->parse_and_visit_typescript_type_expression_no_scope(v);
          v.visit_variable_declaration(ident, Variable_Kind::_generic_parameter,
                                       Variable_Declaration_Flags::none);
          if (this->peek().type == Token_Type::kw_as) {
            this->skip();
            this->parse_and_visit_typescript_type_expression_no_scope(v);
          }
          break;

        // { [varname]: Type }
        case Token_Type::right_square:
        default: {
          Expression *property_name =
              this->make_expression<Expression::Variable>(ident,
                                                          ident_token_type);
          property_name =
              this->parse_expression_remainder(v, property_name, Precedence{});
          this->visit_expression(property_name, v, Variable_Context::rhs);
          break;
        }
        }
        break;
      }

      // { [(expr)]: Type }
      // { ['literal']: Type }
      default:
        Expression *property_name = this->parse_expression(v);
        this->visit_expression(property_name, v, Variable_Context::rhs);
        break;
      }

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_square);
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
    case Token_Type::left_paren:
    case Token_Type::less:
      v.visit_enter_function_scope();
      this->parse_and_visit_interface_function_parameters_and_body_no_scope(
          v, std::nullopt, Function_Attributes::normal,
          Parameter_List_Options{
              .is_interface_method = true,
          });
      v.visit_exit_function_scope();
      break;

    case Token_Type::right_curly:
      this->skip();
      return;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
  }
}

void Parser::parse_and_visit_typescript_template_type_expression(
    Parse_Visitor_Base &v, const TypeScript_Type_Parse_Options &parse_options) {
  const Char8 *template_begin = this->peek().begin;
  for (;;) {
    QLJS_ASSERT(this->peek().type == Token_Type::incomplete_template);
    // TODO(strager): report_errors_for_escape_sequences_in_template
    this->skip();
    this->parse_and_visit_typescript_type_expression_no_scope(v, parse_options);
    switch (this->peek().type) {
    case Token_Type::right_curly:
      this->lexer_.skip_in_template(template_begin);
      switch (this->peek().type) {
      case Token_Type::complete_template:
        // TODO(strager): report_errors_for_escape_sequences_in_template
        this->skip();
        return;

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

void Parser::parse_and_visit_typescript_tuple_type_expression(
    Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::left_square);
  this->skip();

  const Char8 *first_unnamed_element_begin = nullptr;
  std::optional<Source_Code_Span> first_named_tuple_name_and_colon;
  std::optional<Source_Code_Span> last_optional_question;
  std::optional<Source_Code_Span> first_spread;
  bool is_first = true;
  for (;;) {
    if (!is_first) {
      switch (this->peek().type) {
      case Token_Type::comma:
        this->skip();
        break;

      case Token_Type::right_square:
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }
    }

    std::optional<Source_Code_Span> spread;
    if (this->peek().type == Token_Type::dot_dot_dot) {
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

    std::optional<Source_Code_Span> optional_question;

    // expected_optional_question is where a '?' could syntactically be placed
    // to make the tuple element optional. If nullptr, '?' can be placed after
    // the type. expected_optional_question is only valid if optional_question
    // is nullopt.
    const Char8 *expected_optional_question = nullptr;

    switch (this->peek().type) {
    case Token_Type::right_square:
      this->skip();
      return;

    // [: Type]  // Invalid.
    case Token_Type::colon: {
      Source_Code_Span colon_span = this->peek().span();
      this->diag_reporter_->report(
          Diag_TypeScript_Missing_Name_In_Named_Tuple_Type{
              .colon = colon_span,
          });
      this->skip();
      this->parse_and_visit_typescript_type_expression_no_scope(
          v, TypeScript_Type_Parse_Options{
                 .parse_question_as_invalid = false,
             });
      break;
    }

    // [name: Type]
    // [Type]
    QLJS_CASE_CONTEXTUAL_KEYWORD:
    QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    case Token_Type::identifier:
    case Token_Type::kw_await:
    case Token_Type::kw_false:
    case Token_Type::kw_function:
    case Token_Type::kw_import:
    case Token_Type::kw_new:
    case Token_Type::kw_null:
    case Token_Type::kw_this:
    case Token_Type::kw_true:
    case Token_Type::kw_typeof:
    case Token_Type::kw_void:
    case Token_Type::kw_yield: {
      Lexer_Transaction transaction = this->lexer_.begin_transaction();
      const Char8 *element_begin = this->peek().begin;
      this->skip();

      bool is_named_element;
      if (this->peek().type == Token_Type::question) {
        // [Type?]
        // [name?: Type]
        optional_question = this->peek().span();
        this->skip();
        if (this->peek().type == Token_Type::colon) {
          // [name?: Type]
          is_named_element = true;
        } else {
          // [Type?]
          is_named_element = false;
        }
      } else if (this->peek().type == Token_Type::colon) {
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
        QLJS_ASSERT(this->peek().type == Token_Type::colon);
        const Char8 *colon_end = this->peek().end;
        if (!first_named_tuple_name_and_colon.has_value()) {
          Source_Code_Span name_and_colon(element_begin, colon_end);
          if (first_unnamed_element_begin) {
            // [Type1, name: Type2]  // Invalid.
            this->diag_reporter_->report(
                Diag_TypeScript_Missing_Name_And_Colon_In_Named_Tuple_Type{
                    .expected_name_and_colon =
                        Source_Code_Span::unit(first_unnamed_element_begin),
                    .existing_name = name_and_colon,
                });
          }
          first_named_tuple_name_and_colon = name_and_colon;
        }

        this->skip();
        this->lexer_.commit_transaction(std::move(transaction));

        if (this->peek().type == Token_Type::dot_dot_dot) {
          // [name: ...Type]  // Invalid.
          // [...name: ...Type]  // Invalid.
          if (spread.has_value()) {
            this->diag_reporter_->report(
                Diag_TypeScript_Named_Tuple_Element_Spread_Before_Name_And_Type{
                    .type_spread = this->peek().span(),
                    .name_spread = *spread,
                });
          } else {
            this->diag_reporter_->report(
                Diag_TypeScript_Named_Tuple_Element_Spread_Before_Type{
                    .spread = this->peek().span(),
                    .expected_spread = Source_Code_Span::unit(element_begin),
                });
          }
          this->skip();
        }

        this->parse_and_visit_typescript_type_expression_no_scope(
            v, TypeScript_Type_Parse_Options{
                   .parse_question_as_invalid = false,
               });
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
              Diag_TypeScript_Missing_Name_And_Colon_In_Named_Tuple_Type{
                  .expected_name_and_colon =
                      Source_Code_Span::unit(this->peek().begin),
                  .existing_name = *first_named_tuple_name_and_colon,
              });
        }
        first_unnamed_element_begin = this->peek().begin;

        this->parse_and_visit_typescript_type_expression_no_scope(
            v, TypeScript_Type_Parse_Options{
                   .parse_question_as_invalid = false,
               });
      }
      break;
    }

    // [(Type)]
    default:
      first_unnamed_element_begin = this->peek().begin;
      this->parse_and_visit_typescript_type_expression_no_scope(
          v, TypeScript_Type_Parse_Options{
                 .parse_question_as_invalid = false,
             });
      break;
    }

    if (this->peek().type == Token_Type::question) {
      // [(Type)?]
      // [name: Type?]  // Invalid.
      // [name?: Type?]  // Invalid.
      if (optional_question.has_value()) {
        // [name?: Type?]  // Invalid.
        this->diag_reporter_->report(
            Diag_TypeScript_Named_Tuple_Element_Question_After_Name_And_Type{
                .type_question = this->peek().span(),
                .name_question = *optional_question,
            });
      } else if (expected_optional_question) {
        // [name: Type?]  // Invalid.
        this->diag_reporter_->report(
            Diag_TypeScript_Named_Tuple_Element_Question_After_Type{
                .question = this->peek().span(),
                .expected_question =
                    Source_Code_Span::unit(expected_optional_question),
            });
      }

      optional_question = this->peek().span();
      this->skip();
    }

    if (optional_question.has_value()) {
      if (spread.has_value()) {
        // [...Type?]  // Invalid.
        this->diag_reporter_->report(
            Diag_TypeScript_Spread_Element_Cannot_Be_Optional{
                .optional_question = *optional_question,
                .spread = *spread,
            });
        // Don't set last_optional_question; pretend the '?' wasn't there.
      } else if (first_spread.has_value()) {
        // [...Type1, Type2?]  // Invalid.
        this->diag_reporter_->report(
            Diag_TypeScript_Optional_Tuple_Element_Cannot_Follow_Spread_Element{
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
            Diag_TypeScript_Required_Tuple_Element_After_Optional_Element{
                .expected_question =
                    Source_Code_Span::unit(expected_optional_question),
                .previous_optional_question = *last_optional_question,
            });
      }
    }

    is_first = false;
  }
}

void Parser::parse_and_visit_typescript_generic_arguments(
    Parse_Visitor_Base &v) {
  v.visit_enter_type_scope();
  this->parse_and_visit_typescript_generic_arguments_no_scope(v);
  v.visit_exit_type_scope();
}

void Parser::parse_and_visit_typescript_generic_arguments_no_scope(
    Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::less ||
              this->peek().type == Token_Type::less_less);
  if (this->peek().type == Token_Type::less_less) {
    // <<T>() => void>
    this->lexer_.skip_less_less_as_less();
  } else {
    // <T>
    QLJS_ASSERT(this->peek().type == Token_Type::less);
    this->skip();
  }

  this->parse_and_visit_typescript_type_expression_no_scope(v);
  while (this->peek().type == Token_Type::comma) {
    this->skip();
    this->parse_and_visit_typescript_type_expression_no_scope(v);
  }

  switch (this->peek().type) {
  case Token_Type::greater:
    this->skip();
    break;
  case Token_Type::greater_equal:
  case Token_Type::greater_greater:
  case Token_Type::greater_greater_equal:
  case Token_Type::greater_greater_greater:
  case Token_Type::greater_greater_greater_equal:
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
