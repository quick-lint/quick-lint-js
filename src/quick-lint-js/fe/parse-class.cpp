// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/vector.h>
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
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>
#include <utility>

namespace quick_lint_js {
void Parser::parse_and_visit_class(Parse_Visitor_Base &v,
                                   Parse_Class_Options options) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_class);
  Source_Code_Span class_keyword_span = this->peek().span();

  if (options.is_top_level_typescript_definition_without_declare_or_export) {
    this->diag_reporter_->report(Diag_DTS_Missing_Declare_Or_Export{
        .expected =
            Source_Code_Span::unit(options.abstract_keyword_span.has_value()
                                       ? options.abstract_keyword_span->begin()
                                       : class_keyword_span.begin()),
        .declaring_token = class_keyword_span,
    });
  }

  bool is_abstract_in_javascript =
      options.abstract_keyword_span.has_value() && !this->options_.typescript;
  bool is_declare_in_javascript =
      options.declare_keyword_span.has_value() && !this->options_.typescript;
  if (is_abstract_in_javascript && is_declare_in_javascript) {
    this->diag_reporter_->report(
        Diag_Declare_Abstract_Class_Not_Allowed_In_JavaScript{
            .declare_keyword = *options.declare_keyword_span,
        });
  } else if (is_abstract_in_javascript) {
    this->diag_reporter_->report(
        Diag_TypeScript_Abstract_Class_Not_Allowed_In_JavaScript{
            .abstract_keyword = *options.abstract_keyword_span,
        });
  } else if (is_declare_in_javascript) {
    this->diag_reporter_->report(Diag_Declare_Class_Not_Allowed_In_JavaScript{
        .declare_keyword = *options.declare_keyword_span,
    });
  }

  std::optional<Identifier> class_name = this->parse_class_and_optional_name();

  v.visit_enter_class_scope();
  this->parse_and_visit_class_heading_after_name(v);
  v.visit_enter_class_scope_body(class_name);

  switch (this->peek().type) {
  case Token_Type::left_curly:
    this->parse_and_visit_class_body(
        v, Parse_Class_Body_Options{
               .class_or_interface_keyword_span = class_keyword_span,
               .is_abstract = options.abstract_keyword_span.has_value(),
               .declare_keyword = options.declare_keyword_span,
               .is_interface = false,
           });
    break;

  default:
    this->diag_reporter_->report(Diag_Missing_Body_For_Class{
        .class_keyword_and_name_and_heritage =
            Source_Code_Span::unit(this->lexer_.end_of_previous_token()),
    });
    break;
  }
  v.visit_exit_class_scope();

  this->visit_class_name(v, class_name, class_keyword_span,
                         options.require_name);
}

std::optional<Identifier> Parser::parse_class_and_optional_name() {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_class);
  this->skip();

  switch (this->peek().type) {
  case Token_Type::kw_await:
    if (this->in_async_function_) {
      this->diag_reporter_->report(Diag_Cannot_Declare_Await_In_Async_Function{
          .name = this->peek().span(),
      });
    }
    goto class_name;

  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    [[fallthrough]];
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case Token_Type::identifier:
  case Token_Type::kw_yield:
    // TODO(#707): Disallow classes named 'yield' in generator function.
  class_name : {
    if (this->peek().type == Token_Type::kw_let) {
      this->diag_reporter_->report(Diag_Cannot_Declare_Class_Named_Let{
          .name = this->peek().identifier_name().span()});
    }
    Identifier name = this->peek().identifier_name();
    this->skip();
    return name;
  }

    // class { ... }
  case Token_Type::left_curly:
    return std::nullopt;

    // class extends C { }
  case Token_Type::kw_extends:
    return std::nullopt;

    // { class }  // Invalid.
    // class;     // Invalid.
  default:
    // We'll report errors later.
    return std::nullopt;
  }
}

void Parser::parse_and_visit_class_heading_after_name(Parse_Visitor_Base &v) {
  if (this->peek().type == Token_Type::less) {
    if (!this->options_.typescript) {
      this->diag_reporter_->report(
          Diag_TypeScript_Generics_Not_Allowed_In_JavaScript{
              .opening_less = this->peek().span(),
          });
    }
    this->parse_and_visit_typescript_generic_parameters(v);
  }

  std::optional<Source_Code_Span> extends_keyword;
  std::optional<Source_Code_Span> implements_keyword;

  if (this->peek().type == Token_Type::kw_extends) {
    // class C extends Base {}
    extends_keyword = this->peek().span();
    this->parse_and_visit_class_extends(v);
  }

  if (this->peek().type == Token_Type::kw_implements) {
    // class C implements Iface {}
    implements_keyword = this->peek().span();
    this->parse_and_visit_typescript_class_implements(v);
  }

  if (this->peek().type == Token_Type::kw_extends &&
      !extends_keyword.has_value() && implements_keyword.has_value()) {
    // class C implements Iface extends Base {}  // Invalid.
    extends_keyword = this->peek().span();
    if (this->options_.typescript) {
      this->diag_reporter_->report(
          Diag_TypeScript_Implements_Must_Be_After_Extends{
              .implements_keyword = *implements_keyword,
              .extends_keyword = *extends_keyword,
          });
    } else {
      // We already reported
      // Diag_TypeScript_Class_Implements_Not_Allowed_In_JavaScript. Don't
      // report another diagnostic.
    }
    this->parse_and_visit_class_extends(v);
  }
}

void Parser::parse_and_visit_class_extends(Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_extends);
  this->skip();
  // TODO(strager): Error when extending things like '0' or 'true'.
  this->parse_and_visit_expression(v, Precedence{
                                          .commas = false,
                                          .trailing_curly_is_arrow_body = false,
                                          .in_class_extends_clause = true,
                                      });
}

void Parser::parse_and_visit_typescript_class_implements(
    Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_implements);
  if (!this->options_.typescript) {
    this->diag_reporter_->report(
        Diag_TypeScript_Class_Implements_Not_Allowed_In_JavaScript{
            .implements_keyword = this->peek().span(),
        });
  }
  this->skip();
next_implements:
  this->parse_and_visit_typescript_interface_reference(v);
  if (this->peek().type == Token_Type::comma) {
    // implements IBanana, IOrange
    this->skip();
    goto next_implements;
  }
}

void Parser::visit_class_name(Parse_Visitor_Base &v,
                              std::optional<Identifier> class_name,
                              Source_Code_Span class_keyword_span,
                              Name_Requirement require_name) {
  if (class_name.has_value()) {
    v.visit_variable_declaration(*class_name, Variable_Kind::_class,
                                 Variable_Declaration_Flags::none);
  } else {
    switch (require_name) {
    case Name_Requirement::optional:
      break;
    case Name_Requirement::required_for_export:
      this->diag_reporter_->report(Diag_Missing_Name_Of_Exported_Class{
          .class_keyword = class_keyword_span,
      });
      break;
    case Name_Requirement::required_for_statement:
      this->diag_reporter_->report(Diag_Missing_Name_In_Class_Statement{
          .class_keyword = class_keyword_span,
      });
      break;
    }
  }
}

void Parser::parse_and_visit_class_body(Parse_Visitor_Base &v,
                                        Parse_Class_Body_Options options) {
  QLJS_ASSERT(!options.is_interface);
  Class_Guard g = this->enter_class();

  Source_Code_Span left_curly_span = this->peek().span();
  this->skip();

  while (this->peek().type != Token_Type::right_curly) {
    this->parse_and_visit_class_or_interface_member(v, options);
    if (this->peek().type == Token_Type::end_of_file) {
      this->diag_reporter_->report(Diag_Unclosed_Class_Block{
          .block_open = left_curly_span,
      });
      return;
    }
  }

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_curly);
  this->skip();
}

void Parser::parse_and_visit_class_or_interface_member(
    Parse_Visitor_Base &v, Parse_Class_Body_Options options) {
  struct Class_Parser {
    explicit Class_Parser(Parser *p, Parse_Visitor_Base &v,
                          Source_Code_Span class_or_interface_keyword_span,
                          bool is_interface, bool is_abstract,
                          const Parse_Class_Body_Options &options)
        : p(p),
          v(v),
          class_or_interface_keyword_span(class_or_interface_keyword_span),
          is_interface(is_interface),
          is_abstract(is_abstract),
          declare_keyword(options.declare_keyword) {}

    Parser *p;
    Parse_Visitor_Base &v;
    Source_Code_Span class_or_interface_keyword_span;
    bool is_interface;
    bool is_abstract;
    std::optional<Source_Code_Span> declare_keyword;

    std::optional<Identifier> last_ident;

    // *, !, ?, async, function, get, private, protected, public, readonly, set,
    // static
    struct Modifier {
      Source_Code_Span span;
      Token_Type type;

      bool is_access_specifier() const {
        return this->type == Token_Type::kw_private ||
               this->type == Token_Type::kw_protected ||
               this->type == Token_Type::kw_public;
      }
    };
    Bump_Vector<Modifier, Monotonic_Allocator> modifiers =
        Bump_Vector<Modifier, Monotonic_Allocator>("class member modifiers",
                                                   &p->temporary_memory_);

    // Returns true if an entire property was parsed.
    // Returns false if nothing was parsed or if only modifiers were parsed.
    bool parse_modifiers() {
      for (;;) {
        switch (p->peek().type) {
        // abstract f();
        // accessor myField = null;
        // async f() {}
        // function f() {}  // Invalid.
        // function() {}
        // private f() {}
        // public static field = 42;
        // readonly field: number;
        case Token_Type::kw_abstract:
        case Token_Type::kw_accessor:
        case Token_Type::kw_async:
        case Token_Type::kw_function:
        case Token_Type::kw_private:
        case Token_Type::kw_protected:
        case Token_Type::kw_public:
        case Token_Type::kw_readonly:
          last_ident = p->peek().identifier_name();
          modifiers.push_back(Modifier{
              .span = p->peek().span(),
              .type = p->peek().type,
          });
          p->skip();
          if (p->peek().has_leading_newline) {
            switch (p->peek().type) {
            // 'async' is a field name:
            // class {
            //   async
            //   method() {}
            // }
            QLJS_CASE_KEYWORD:
            case Token_Type::left_square:
            case Token_Type::number:
            case Token_Type::string:
            case Token_Type::identifier:
            case Token_Type::private_identifier:
            case Token_Type::star:
              check_modifiers_for_field_without_type_annotation();
              v.visit_property_declaration(last_ident);
              return true;
            default:
              continue;
            }
          } else {
            continue;
          }
          QLJS_UNREACHABLE();

        // *g() {}
        case Token_Type::star:
          modifiers.push_back(Modifier{
              .span = p->peek().span(),
              .type = p->peek().type,
          });
          p->skip();
          continue;

        // get prop() {}
        case Token_Type::kw_get:
        case Token_Type::kw_set:
          last_ident = p->peek().identifier_name();
          modifiers.push_back(Modifier{
              .span = p->peek().span(),
              .type = p->peek().type,
          });
          p->skip();
          continue;

        // static f() {}
        case Token_Type::kw_static:
          last_ident = p->peek().identifier_name();
          modifiers.push_back(Modifier{
              .span = p->peek().span(),
              .type = p->peek().type,
          });
          p->skip();
          continue;

        default:
          return false;
        }
      }
    }

    void parse_stuff() {
      if (bool done = parse_modifiers(); done) {
        return;
      }

      switch (p->peek().type) {
      // method() {}
      // static() {}
      // #method() {}
      // field;
      // field = initialValue;
      // #field = initialValue;
      case Token_Type::private_identifier:
        if (is_interface) {
          p->diag_reporter_->report(Diag_Interface_Properties_Cannot_Be_Private{
              .property_name_or_private_keyword = p->peek().span(),
          });
        }
        [[fallthrough]];
      QLJS_CASE_RESERVED_KEYWORD_EXCEPT_FUNCTION:
      QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case Token_Type::identifier:
      case Token_Type::reserved_keyword_with_escape_sequence: {
        Identifier property_name = p->peek().identifier_name();
        this->warn_if_typescript_constructor_with_escape_sequence();
        p->skip();
        parse_and_visit_field_or_method(property_name);
        break;
      }

      // "method"() {}
      // 9001() {}
      // "fieldName" = init;
      case Token_Type::number:
      case Token_Type::string: {
        Source_Code_Span name_span = p->peek().span();
        p->skip();
        parse_and_visit_field_or_method_without_name(name_span);
        break;
      }

      // [methodNameExpression]() {}
      // [fieldNameExpression] = initialValue;
      // [key: KeyType]: ValueType;  // TypeScript only
      case Token_Type::left_square: {
        const Char8 *name_begin = p->peek().begin;
        p->skip();

        if (p->options_.typescript || is_interface) {
          bool parsed = this->try_parse_typescript_index_signature(
              /*left_square_begin=*/name_begin);
          if (parsed) {
            break;
          }
        }

        p->parse_and_visit_expression(v);

        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN_WITH_PARSER(
            p, Token_Type::right_square);
        const Char8 *name_end = p->peek().end;
        p->skip();

        parse_and_visit_field_or_method_without_name(
            Source_Code_Span(name_begin, name_end));
        break;
      }

      // async;  // Field named 'async'.
      // ;       // Stray semicolon.
      case Token_Type::semicolon:
        if (last_ident.has_value()) {
          modifiers.pop_back();
          parse_and_visit_field_or_method(*last_ident);
        } else {
          p->skip();
        }
        break;

      // async() {}
      // get() {}
      // () {}       // Invalid.
      // (): void;   // TypeScript call signature.
      case Token_Type::left_paren:
        if (last_ident.has_value()) {
          modifiers.pop_back();
          parse_and_visit_field_or_method(*last_ident);
        } else {
          Source_Code_Span expected_name(p->peek().begin, p->peek().begin);
          if (is_interface) {
            // (): void;
          } else {
            // (params) {}  // Invalid.
            p->diag_reporter_->report(Diag_Missing_Class_Method_Name{
                .expected_name = expected_name,
            });
          }
          parse_and_visit_field_or_method_without_name(expected_name);
        }
        break;

      // async?() {}
      // class C { get }  // Field named 'get'
      // get = init;
      case Token_Type::equal:
      case Token_Type::question:
      case Token_Type::right_curly:
        if (last_ident.has_value()) {
          modifiers.pop_back();
          parse_and_visit_field_or_method(*last_ident);
        } else {
          QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        }
        break;

      // async<T>(): void;     // TypeScript generic method.
      // <T>(param: T): void;  // TypeScript generic interface call signature.
      case Token_Type::less:
        if (last_ident.has_value()) {
          // set<T>(): void;
          modifiers.pop_back();
          parse_and_visit_field_or_method(*last_ident);
        } else {
          // <T>(param: T): void;
          Source_Code_Span property_name_span(p->peek().begin, p->peek().begin);

          v.visit_enter_function_scope();
          {
            Function_Attributes attributes =
                function_attributes_from_modifiers(std::nullopt);
            Parameter_List_Options param_options;
            if (is_interface) {
              p->parse_and_visit_interface_function_parameters_and_body_no_scope(
                  v, property_name_span, attributes, param_options);
            } else {
              p->parse_and_visit_function_parameters_and_body_no_scope(
                  v, property_name_span, attributes, param_options);
              p->diag_reporter_->report(Diag_Missing_Class_Method_Name{
                  .expected_name = property_name_span,
              });
            }
          }
          v.visit_exit_function_scope();
          v.visit_property_declaration(std::nullopt);
        }
        break;

      // class C { {             // Invalid.
      // class C { static { } }  // TypeScript only.
      case Token_Type::left_curly:
        if (modifiers.size() == 1 &&
            modifiers[0].type == Token_Type::kw_static) {
          // class C { static { } }
          v.visit_enter_block_scope();
          Source_Code_Span left_curly_span = p->peek().span();
          p->skip();

          error_if_invalid_static_block(/*static_modifier=*/modifiers[0]);

          p->parse_and_visit_statement_block_after_left_curly(v,
                                                              left_curly_span);
          v.visit_exit_block_scope();
        } else {
          // class C { {  // Invalid.
          p->diag_reporter_->report(Diag_Unexpected_Token{
              .token = p->peek().span(),
          });
          p->skip();
        }
        break;

      case Token_Type::comma:
        p->diag_reporter_->report(Diag_Comma_Not_Allowed_Between_Class_Methods{
            .unexpected_comma = p->peek().span(),
        });
        p->skip();
        break;

      // class C {  // Invalid.
      case Token_Type::end_of_file:
        // An error is reported by parse_and_visit_class_body.
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        break;
      }
    }

    // See NOTE[typescript-constructor-escape].
    void warn_if_typescript_constructor_with_escape_sequence() {
      if (p->options_.typescript && !this->is_interface &&
          p->peek().type == Token_Type::identifier &&
          p->peek().normalized_identifier == u8"constructor"_sv &&
          p->peek().contains_escape_sequence()) {
        bool has_exactly_one_escape_sequence =
            std::count(p->peek().begin, p->peek().end, u8'\\') == 1;
        if (has_exactly_one_escape_sequence) {
          p->diag_reporter_->report(Diag_Keyword_Contains_Escape_Characters{
              .escape_character_in_keyword = p->peek().span()});
        }
      }
    }

    // Returns true if an index signature was parsed.
    // Returns false if nothing was parsed.
    //
    // [key: KeyType]: ValueType;  // TypeScript only
    bool try_parse_typescript_index_signature(const Char8 *left_square_begin) {
      Lexer_Transaction transaction = p->lexer_.begin_transaction();
      // TODO(strager): Allow certain contextual keywords like 'let'?
      if (p->peek().type == Token_Type::identifier) {
        Identifier key_variable = p->peek().identifier_name();
        p->skip();
        if (p->peek().type == Token_Type::colon) {
          // [key: KeyType]: ValueType;
          v.visit_enter_index_signature_scope();
          p->lexer_.commit_transaction(std::move(transaction));
          p->parse_and_visit_typescript_colon_type_expression(v);

          QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN_WITH_PARSER(
              p, Token_Type::right_square);
          const Char8 *name_end = p->peek().end;
          p->skip();

          v.visit_variable_declaration(
              key_variable, Variable_Kind::_index_signature_parameter,
              Variable_Declaration_Flags::none);

          switch (p->peek().type) {
          // [key: KeyType]: ValueType;
          case Token_Type::colon:
            p->parse_and_visit_typescript_colon_type_expression(v);
            p->consume_semicolon<
                Diag_Missing_Semicolon_After_Index_Signature>();
            break;

          // [key: KeyType];  // Invalid.
          case Token_Type::semicolon:
          missing_type_for_index_signature:
            p->diag_reporter_->report(
                Diag_TypeScript_Index_Signature_Needs_Type{
                    .expected_type = Source_Code_Span::unit(
                        p->lexer_.end_of_previous_token()),
                });
            break;

          // [key: KeyType]  // Invalid.
          // ();
          //
          // [key: KeyType]();  // Invalid.
          case Token_Type::left_paren: {
            if (p->peek().has_leading_newline) {
              QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
            } else {
              p->diag_reporter_->report(
                  Diag_TypeScript_Index_Signature_Cannot_Be_Method{
                      .left_paren = p->peek().span(),
                  });
              parse_and_visit_field_or_method_without_name(
                  Source_Code_Span(left_square_begin, name_end));
            }
            break;
          }

          // [key: KeyType]  // Invalid.
          // method();
          //
          // [key: KeyType] method();  // Invalid.
          default:
            if (p->peek().has_leading_newline) {
              goto missing_type_for_index_signature;
            } else {
              QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
            }
            break;
          }
          v.visit_exit_index_signature_scope();
          return true;
        }
      }
      p->lexer_.roll_back_transaction(std::move(transaction));
      return false;
    }

    void parse_and_visit_field_or_method(Identifier property_name) {
      parse_and_visit_field_or_method_impl(property_name, property_name.span());
    }

    void parse_and_visit_field_or_method_without_name(
        Source_Code_Span name_span) {
      parse_and_visit_field_or_method_impl(std::nullopt, name_span);
    }

    void parse_and_visit_field_or_method_impl(
        std::optional<Identifier> property_name,
        Source_Code_Span property_name_span) {
      if (p->peek().type == Token_Type::question) {
        // field?: Type;
        modifiers.push_back(Modifier{
            .span = p->peek().span(),
            .type = p->peek().type,
        });
        p->skip();
      }

      if (p->peek().type == Token_Type::bang) {
        // field!: Type;
        Source_Code_Span bang_span = p->peek().span();
        if (p->peek().has_leading_newline) {
          p->diag_reporter_->report(
              Diag_Newline_Not_Allowed_Before_Assignment_Assertion_Operator{
                  .bang = bang_span,
                  .field_name = property_name_span,
              });
        }
        modifiers.push_back(Modifier{
            .span = bang_span,
            .type = p->peek().type,
        });
        p->skip();
      }

      error_if_function_modifier();
      error_if_async_static();
      error_if_readonly_static();
      error_if_invalid_assignment_assertion();

      switch (p->peek().type) {
        // method() { }
        // method<T>() { }  // TypeScript only.
        // method { }       // Invalid (missing parameter list).
      case Token_Type::left_curly:
      case Token_Type::left_paren:
      case Token_Type::less: {
        if (p->options_.typescript) {
          if (const Modifier *assignment_assertion_modifier =
                  find_modifier(Token_Type::bang)) {
            if (p->peek().has_leading_newline) {
              // field!
              // () {}  // Invalid for classes.

              // Stop parsing the field. We will report
              // Diag_Missing_Class_Method_Name later if needed.
              v.visit_property_declaration(property_name);
              break;
            } else {
              // method!() {}  // Invalid.
              p->diag_reporter_->report(
                  Diag_TypeScript_Assignment_Asserted_Method{
                      .bang = assignment_assertion_modifier->span,
                  });
            }
          }
        }
        check_modifiers_for_method();

        Function_Attributes attributes =
            function_attributes_from_modifiers(property_name);
        Parameter_List_Options param_options = {
            .declare_class_keyword = declare_keyword,
            .is_class_constructor =
                property_name.has_value() &&
                property_name->normalized_name() == u8"constructor"_sv,
        };
        bool is_abstract_method = this->find_modifier(Token_Type::kw_abstract);
        if (declare_keyword.has_value()) {
          p->parse_and_visit_declare_class_method_parameters_and_body(
              v, property_name_span, attributes, param_options);
        } else if (is_abstract_method) {
          v.visit_enter_function_scope();
          p->parse_and_visit_abstract_function_parameters_and_body_no_scope(
              v, property_name_span, attributes, param_options);
          v.visit_exit_function_scope();
        } else if (is_interface) {
          v.visit_enter_function_scope();
          p->parse_and_visit_interface_function_parameters_and_body_no_scope(
              v, property_name_span, attributes, param_options);
          v.visit_exit_function_scope();
        } else {
          p->parse_and_visit_function_parameters_and_body(
              v, /*name=*/property_name_span, attributes, param_options);
        }
        v.visit_property_declaration(property_name);
        break;
      }

      // field;
      // class C { field }
      // class C { field  // Invalid.
      // class C { field, }     // Invalid.
      // interface I { field, }
      case Token_Type::comma:
      case Token_Type::end_of_file:
      case Token_Type::right_curly:
      case Token_Type::semicolon:
        check_modifiers_for_field_without_type_annotation();
        v.visit_property_declaration(property_name);
        this->parse_field_terminator();
        break;

        // field = initialValue;
      case Token_Type::equal:
        check_modifiers_for_field_without_type_annotation();
        this->parse_field_initializer();
        v.visit_property_declaration(property_name);
        this->parse_field_terminator();
        break;

      case Token_Type::identifier:
      case Token_Type::private_identifier:
      case Token_Type::star:
        if (p->peek().has_leading_newline) {
          // class C {
          //   field        // ASI
          //   method() {}
          // }
          check_modifiers_for_field_without_type_annotation();
          v.visit_property_declaration(property_name);
        } else {
          if (u8"const"_sv == property_name_span.string_view()) {
            // class C {
            //   const field
            // }
            p->diag_reporter_->report(Diag_TypeScript_Style_Const_Field{
                .const_token = property_name_span,
            });
          } else if (find_modifier(Token_Type::question)) {
            // class C {
            //   field? method() {}  // Invalid.
            // }
            check_modifiers_for_field_without_type_annotation();
            v.visit_property_declaration(property_name);
            this->parse_field_terminator();
          } else {
            // class C {
            //   asyn method() {}  // Invalid.
            // }
            p->diag_reporter_->report(Diag_Unexpected_Token{
                .token = property_name_span,
            });
          }
        }
        break;

      QLJS_CASE_KEYWORD:
      case Token_Type::left_square:
      case Token_Type::number:
      case Token_Type::string:
        if (p->peek().has_leading_newline) {
          // class C {
          //   field        // ASI
          //   [expr]() {}
          // }
          check_modifiers_for_field_without_type_annotation();
          v.visit_property_declaration(property_name);
        } else {
          QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        }
        break;

      case Token_Type::colon:
        this->check_modifiers_for_field_with_type_annotation();
        if (this->find_modifier(Token_Type::bang)) {
          // If we have a bang modifier, we already reported
          // Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_JavaScript.
          p->skip();
        } else {
          p->parse_typescript_colon_for_type();
        }
        p->parse_and_visit_typescript_type_expression(v);

        if (p->peek().type == Token_Type::equal) {
          this->parse_field_initializer();
        }
        v.visit_property_declaration(property_name);
        this->parse_field_terminator();
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        break;
      }
    }

    void parse_field_initializer() {
      QLJS_ASSERT(p->peek().type == Token_Type::equal);
      const Modifier *bang = this->find_modifier(Token_Type::bang);
      const Modifier *abstract = this->find_modifier(Token_Type::kw_abstract);
      const Modifier *static_modifier =
          this->find_modifier(Token_Type::kw_static);
      if (is_interface && !bang) {
        // Don't report if we found a bang. We already reported
        // Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Interfaces.
        p->diag_reporter_->report(
            Diag_Interface_Fields_Cannot_Have_Initializers{
                .equal = p->peek().span(),
            });
      }
      if (declare_keyword.has_value() && !is_interface && !bang) {
        // Don't report if we found a bang. We already reported
        // Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Declare_Class.
        p->diag_reporter_->report(
            Diag_Declare_Class_Fields_Cannot_Have_Initializers{
                .equal = p->peek().span(),
            });
      }
      if (p->options_.typescript && !is_interface &&
          !declare_keyword.has_value() && bang) {
        p->diag_reporter_->report(
            Diag_TypeScript_Assignment_Asserted_Field_Cannot_Have_Initializer{
                .equal = p->peek().span(),
                .bang = bang->span,
            });
      }
      if (abstract) {
        p->diag_reporter_->report(Diag_Abstract_Field_Cannot_Have_Initializer{
            .equal = p->peek().span(),
            .abstract_keyword = abstract->span,
        });
      }
      p->skip();

      if (!static_modifier) {
        v.visit_enter_class_construct_scope();
      }
      p->parse_and_visit_expression(v, Precedence{.commas = false});
      if (!static_modifier) {
        v.visit_exit_class_construct_scope();
      }
    }

    void parse_field_terminator() {
      if (p->peek().type == Token_Type::comma) {
        // interface I { x: number, }
        if (!this->is_interface) {
          // class C { x: number, }  // Invalid.
          p->diag_reporter_->report(Diag_Unexpected_Comma_After_Class_Field{
              .comma = p->peek().span(),
          });
        }
        p->skip();
      } else {
        // class C { x; }
        // class C { x }
        // class C {
        //   x
        // }
        p->consume_semicolon<Diag_Missing_Semicolon_After_Field>();
      }
    }

    Function_Attributes function_attributes_from_modifiers(
        const std::optional<Identifier> &) const {
      bool has_async = this->find_modifier(Token_Type::kw_async);
      bool has_star = this->find_modifier(Token_Type::star);
      if (has_async && has_star) return Function_Attributes::async_generator;
      if (has_async && !has_star) return Function_Attributes::async;
      if (!has_async && has_star) return Function_Attributes::generator;
      if (!has_async && !has_star) return Function_Attributes::normal;
      QLJS_UNREACHABLE();
    }

    void check_modifiers_for_field_without_type_annotation() {
      this->check_modifiers_for_field();

      if (!this->is_interface && p->options_.typescript) {
        if (const Modifier *bang = this->find_modifier(Token_Type::bang)) {
          p->diag_reporter_->report(
              Diag_TypeScript_Assignment_Asserted_Field_Must_Have_A_Type{
                  .bang = bang->span,
              });
        }
      }
    }

    void check_modifiers_for_field_with_type_annotation() {
      this->check_modifiers_for_field();
    }

    void check_modifiers_for_field() {
      error_if_invalid_access_specifier();
      error_if_getter_setter_field();
      error_if_access_specifier_not_first_in_field();
      error_if_abstract_or_static_after_accessor();
      error_if_conflicting_modifiers();
      error_if_readonly_in_not_typescript();
      error_if_accessor_in_interface();
      error_if_static_in_interface();
      error_if_static_abstract();
      error_if_optional_in_not_typescript();
      error_if_optional_accessor();
      error_if_abstract_not_in_abstract_class();
    }

    void check_modifiers_for_method() {
      error_if_accessor_method();
      error_if_readonly_method();
      error_if_async_or_generator_without_method_body();
      error_if_invalid_access_specifier();
      error_if_access_specifier_not_first_in_method();
      error_if_static_in_interface();
      error_if_static_abstract();
      error_if_optional_in_not_typescript();
      error_if_abstract_not_in_abstract_class();
    }

    void error_if_optional_in_not_typescript() {
      if (!p->options_.typescript && !is_interface) {
        if (const Modifier *optional_modifier =
                find_modifier(Token_Type::question)) {
          p->diag_reporter_->report(
              Diag_TypeScript_Optional_Properties_Not_Allowed_In_JavaScript{
                  .question = optional_modifier->span,
              });
        }
      }
    }

    void error_if_optional_accessor() {
      if (const Modifier *accessor_modifier =
              find_modifier(Token_Type::kw_accessor)) {
        if (const Modifier *optional_modifier =
                find_modifier(Token_Type::question)) {
          p->diag_reporter_->report(Diag_TypeScript_Accessor_Cannot_Be_Optional{
              .optional_question = optional_modifier->span,
              .accessor_keyword = accessor_modifier->span,
          });
        }
      }
    }

    void error_if_invalid_static_block(const Modifier &static_modifier) {
      if (is_interface) {
        p->diag_reporter_->report(
            Diag_TypeScript_Interfaces_Cannot_Contain_Static_Blocks{
                .static_token = static_modifier.span,
            });
      }
      if (declare_keyword.has_value() && !is_interface) {
        if (p->peek().type == Token_Type::right_curly) {
          // static { }
          // An empty static block is legal.
        } else {
          // static { someStatement(); }
          p->diag_reporter_->report(
              Diag_TypeScript_Declare_Class_Cannot_Contain_Static_Block_Statement{
                  .static_token = static_modifier.span,
              });
        }
      }
    }

    void error_if_invalid_assignment_assertion() {
      if (const Modifier *assignment_assertion_modifier =
              find_modifier(Token_Type::bang)) {
        if (!p->options_.typescript) {
          p->diag_reporter_->report(
              Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_JavaScript{
                  .bang = assignment_assertion_modifier->span,
              });
        } else if (is_interface) {
          p->diag_reporter_->report(
              Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Interfaces{
                  .bang = assignment_assertion_modifier->span,
              });
        } else if (declare_keyword.has_value()) {
          p->diag_reporter_->report(
              Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Declare_Class{
                  .bang = assignment_assertion_modifier->span,
              });
        }
      }
    }

    void error_if_function_modifier() {
      if (const Modifier *function_modifier =
              find_modifier(Token_Type::kw_function)) {
        p->diag_reporter_->report(Diag_Methods_Should_Not_Use_Function_Keyword{
            .function_token = function_modifier->span,
        });
      }
    }

    void error_if_async_static() {
      // FIXME(#747): This only checks the first 'readonly' modifier
      // against the first 'static' modifier.
      const Modifier *async_modifier = find_modifier(Token_Type::kw_async);
      const Modifier *static_modifier = find_modifier(Token_Type::kw_static);
      if (async_modifier && static_modifier &&
          async_modifier < static_modifier) {
        if (!is_interface) {
          // FIXME(#747): This produces bad spans if there are tokens between
          // 'async' and 'static'.
          p->diag_reporter_->report(Diag_Async_Static_Method{
              .async_static = Source_Code_Span(async_modifier->span.begin(),
                                               static_modifier->span.end()),
          });
        }
      }
    }

    void error_if_readonly_static() {
      // FIXME(#747): This only checks the first 'readonly' modifier against
      // the first 'static' modifier.
      const Modifier *readonly_modifier =
          find_modifier(Token_Type::kw_readonly);
      const Modifier *static_modifier = find_modifier(Token_Type::kw_static);
      if (readonly_modifier && static_modifier &&
          readonly_modifier < static_modifier) {
        if (!is_interface) {
          // FIXME(#747): This produces bad spans if there are tokens between
          // 'readonly' and 'static'.
          p->diag_reporter_->report(Diag_Readonly_Static_Field{
              .readonly_static = Source_Code_Span(
                  readonly_modifier->span.begin(), static_modifier->span.end()),
          });
        }
      }
    }

    void error_if_accessor_method() {
      if (const Modifier *accessor_modifier =
              find_modifier(Token_Type::kw_accessor)) {
        Source_Code_Span method_start = p->peek().span();
        if (const Modifier *get_modifier = find_modifier(Token_Type::kw_get)) {
          p->diag_reporter_->report(Diag_Class_Accessor_On_Getter_Or_Setter{
              .method_start = method_start,
              .accessor_keyword = accessor_modifier->span,
              .getter_setter_keyword = get_modifier->span,
          });
        } else if (const Modifier *set_modifier =
                       find_modifier(Token_Type::kw_set)) {
          p->diag_reporter_->report(Diag_Class_Accessor_On_Getter_Or_Setter{
              .method_start = method_start,
              .accessor_keyword = accessor_modifier->span,
              .getter_setter_keyword = set_modifier->span,
          });
        } else {
          p->diag_reporter_->report(Diag_Class_Accessor_On_Method{
              .method_start = method_start,
              .accessor_keyword = accessor_modifier->span,
          });
        }
      }
    }

    void error_if_readonly_method() {
      if (const Modifier *readonly_modifier =
              find_modifier(Token_Type::kw_readonly)) {
        p->diag_reporter_->report(Diag_TypeScript_Readonly_Method{
            .readonly_keyword = readonly_modifier->span,
        });
      }
    }

    void error_if_readonly_in_not_typescript() {
      if (!p->options_.typescript) {
        if (const Modifier *readonly_modifier =
                find_modifier(Token_Type::kw_readonly)) {
          p->diag_reporter_->report(
              Diag_TypeScript_Readonly_Fields_Not_Allowed_In_JavaScript{
                  .readonly_keyword = readonly_modifier->span,
              });
        }
      }
    }

    void error_if_invalid_access_specifier() {
      if (const Modifier *access_specifier = find_access_specifier()) {
        if (is_interface) {
          switch (access_specifier->type) {
          case Token_Type::kw_private:
            p->diag_reporter_->report(
                Diag_Interface_Properties_Cannot_Be_Private{
                    .property_name_or_private_keyword = access_specifier->span,
                });
            break;

          case Token_Type::kw_protected:
            p->diag_reporter_->report(
                Diag_Interface_Properties_Cannot_Be_Protected{
                    .protected_keyword = access_specifier->span,
                });
            break;

          case Token_Type::kw_public:
            p->diag_reporter_->report(
                Diag_Interface_Properties_Cannot_Be_Explicitly_Public{
                    .public_keyword = access_specifier->span,
                });
            break;

          default:
            QLJS_UNREACHABLE();
            break;
          }
        } else if (!p->options_.typescript) {
          switch (access_specifier->type) {
          case Token_Type::kw_private:
            p->diag_reporter_->report(
                Diag_TypeScript_Private_Not_Allowed_In_JavaScript{
                    .specifier = access_specifier->span,
                });
            break;

          case Token_Type::kw_protected:
            p->diag_reporter_->report(
                Diag_TypeScript_Protected_Not_Allowed_In_JavaScript{
                    .specifier = access_specifier->span,
                });
            break;

          case Token_Type::kw_public:
            p->diag_reporter_->report(
                Diag_TypeScript_Public_Not_Allowed_In_JavaScript{
                    .specifier = access_specifier->span,
                });
            break;

          default:
            QLJS_UNREACHABLE();
            break;
          }
        }
      }
    }

    void error_if_getter_setter_field() {
      if (const Modifier *get_modifier = find_modifier(Token_Type::kw_get)) {
        p->diag_reporter_->report(Diag_Unexpected_Token{
            .token = get_modifier->span,
        });
      }
      if (const Modifier *set_modifier = find_modifier(Token_Type::kw_set)) {
        p->diag_reporter_->report(Diag_Unexpected_Token{
            .token = set_modifier->span,
        });
      }
    }

    void error_if_access_specifier_not_first_in_field() {
      if (p->options_.typescript && !modifiers.empty()) {
        check_if_access_specifier_precedes_given_modifiers(
            Span<const Token_Type>({
                Token_Type::kw_accessor,
                Token_Type::kw_readonly,
                Token_Type::kw_static,
            }));
      }
    }

    void error_if_access_specifier_not_first_in_method() {
      if (p->options_.typescript && !modifiers.empty()) {
        check_if_access_specifier_precedes_given_modifiers(
            Span<const Token_Type>({
                Token_Type::kw_static,
                Token_Type::kw_async,
            }));
      }
    }

    void error_if_abstract_or_static_after_accessor() {
      if (const Modifier *accessor_modifier =
              this->find_modifier(Token_Type::kw_accessor)) {
        if (const Modifier *abstract_modifier =
                this->find_modifier(Token_Type::kw_abstract)) {
          if (abstract_modifier > accessor_modifier) {
            p->diag_reporter_->report(
                Diag_Class_Modifier_Must_Preceed_Other_Modifier{
                    .expected_first_modifier = abstract_modifier->span,
                    .expected_second_modifier = accessor_modifier->span,
                });
          }
        }

        if (const Modifier *static_modifier =
                this->find_modifier(Token_Type::kw_static)) {
          if (static_modifier > accessor_modifier) {
            p->diag_reporter_->report(
                Diag_Class_Modifier_Must_Preceed_Other_Modifier{
                    .expected_first_modifier = static_modifier->span,
                    .expected_second_modifier = accessor_modifier->span,
                });
          }
        }
      }
    }

    void error_if_conflicting_modifiers() {
      if (const Modifier *accessor_modifier =
              this->find_modifier(Token_Type::kw_accessor)) {
        if (const Modifier *readonly_modifier =
                this->find_modifier(Token_Type::kw_readonly)) {
          const Modifier *modifiers[2] = {accessor_modifier, readonly_modifier};
          sort(modifiers);
          p->diag_reporter_->report(Diag_Class_Conflicting_Modifiers{
              .second_modifier = modifiers[1]->span,
              .first_modifier = modifiers[0]->span,
          });
        }
      }
    }

    void check_if_access_specifier_precedes_given_modifiers(
        Span<const Token_Type> tokens) {
      const Modifier *access_specifier = find_access_specifier();

      for (const Token_Type token : tokens) {
        const Modifier *m = find_modifier(token);

        if (m && m < access_specifier) {
          p->diag_reporter_->report(
              Diag_Access_Specifier_Must_Precede_Other_Modifiers{
                  .second_modifier = access_specifier->span,
                  .first_modifier = m->span,
              });
        }
      }
    }

    void error_if_accessor_in_interface() {
      if (is_interface) {
        if (const Modifier *accessor_modifier =
                find_modifier(Token_Type::kw_accessor)) {
          p->diag_reporter_->report(Diag_Interface_Field_Cannot_Be_Accessor{
              .accessor_keyword = accessor_modifier->span,
          });
        }
      }
    }

    void error_if_static_in_interface() {
      if (is_interface) {
        if (const Modifier *static_modifier =
                find_modifier(Token_Type::kw_static)) {
          p->diag_reporter_->report(Diag_Interface_Properties_Cannot_Be_Static{
              .static_keyword = static_modifier->span,
          });
        }
      }
    }

    void error_if_static_abstract() {
      if (const Modifier *abstract_modifier =
              find_modifier(Token_Type::kw_abstract)) {
        if (const Modifier *static_modifier =
                find_modifier(Token_Type::kw_static)) {
          p->diag_reporter_->report(Diag_TypeScript_Abstract_Static_Property{
              .abstract_keyword = abstract_modifier->span,
              .static_keyword = static_modifier->span,
          });
        }
      }
    }

    void error_if_abstract_not_in_abstract_class() {
      if (const Modifier *abstract_modifier =
              find_modifier(Token_Type::kw_abstract)) {
        if (is_interface) {
          p->diag_reporter_->report(
              Diag_Abstract_Property_Not_Allowed_In_Interface{
                  .abstract_keyword = abstract_modifier->span,
              });
        } else if (!is_abstract) {
          p->diag_reporter_->report(
              Diag_Abstract_Property_Not_Allowed_In_Non_Abstract_Class{
                  .abstract_keyword = abstract_modifier->span,
                  .class_keyword = class_or_interface_keyword_span,
              });
        }
      }
    }

    void error_if_async_or_generator_without_method_body() {
      if (is_interface) {
        if (const Modifier *async_modifier =
                find_modifier(Token_Type::kw_async)) {
          p->diag_reporter_->report(Diag_Interface_Methods_Cannot_Be_Async{
              .async_keyword = async_modifier->span,
          });
        }
        if (const Modifier *star_modifier = find_modifier(Token_Type::star)) {
          p->diag_reporter_->report(Diag_Interface_Methods_Cannot_Be_Generators{
              .star = star_modifier->span,
          });
        }
      }
      if (declare_keyword.has_value()) {
        if (const Modifier *async_modifier =
                find_modifier(Token_Type::kw_async)) {
          p->diag_reporter_->report(Diag_Declare_Class_Methods_Cannot_Be_Async{
              .async_keyword = async_modifier->span,
          });
        }
        if (const Modifier *star_modifier = find_modifier(Token_Type::star)) {
          p->diag_reporter_->report(
              Diag_Declare_Class_Methods_Cannot_Be_Generators{
                  .star = star_modifier->span,
              });
        }
      }

      const Modifier *abstract_modifier =
          find_modifier(Token_Type::kw_abstract);
      if (abstract_modifier) {
        if (const Modifier *async_modifier =
                find_modifier(Token_Type::kw_async)) {
          p->diag_reporter_->report(Diag_Abstract_Methods_Cannot_Be_Async{
              .async_keyword = async_modifier->span,
              .abstract_keyword = abstract_modifier->span,
          });
        }
        if (const Modifier *star_modifier = find_modifier(Token_Type::star)) {
          p->diag_reporter_->report(Diag_Abstract_Methods_Cannot_Be_Generators{
              .star = star_modifier->span,
              .abstract_keyword = abstract_modifier->span,
          });
        }
      }
    }

    const Modifier *find_modifier(Token_Type modifier_type) const {
      for (const Modifier &m : modifiers) {
        if (m.type == modifier_type) {
          return &m;
        }
      }
      return nullptr;
    }

    const Modifier *find_access_specifier() const {
      for (const Modifier &m : modifiers) {
        if (m.is_access_specifier()) {
          return &m;
        }
      }
      return nullptr;
    }
  };
  Class_Parser state(this, v, options.class_or_interface_keyword_span,
                     options.is_interface, options.is_abstract, options);
  state.parse_stuff();
}

void Parser::parse_and_visit_typescript_interface(
    Parse_Visitor_Base &v, Source_Code_Span interface_keyword_span) {
  TypeScript_Only_Construct_Guard ts_guard =
      this->enter_typescript_only_construct();

  if (!this->options_.typescript) {
    this->diag_reporter_->report(
        Diag_TypeScript_Interfaces_Not_Allowed_In_JavaScript{
            .interface_keyword = interface_keyword_span,
        });
  }

  switch (this->peek().type) {
  case Token_Type::kw_await:
    if (this->in_async_function_) {
      this->diag_reporter_->report(Diag_Cannot_Declare_Await_In_Async_Function{
          .name = this->peek().span(),
      });
    }
    goto interface_name;

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
  case Token_Type::kw_is:
  case Token_Type::kw_keyof:
  case Token_Type::kw_module:
  case Token_Type::kw_namespace:
  case Token_Type::kw_of:
  case Token_Type::kw_out:
  case Token_Type::kw_override:
  case Token_Type::kw_readonly:
  case Token_Type::kw_require:
  case Token_Type::kw_set:
  case Token_Type::kw_satisfies:
  case Token_Type::kw_type:
  case Token_Type::kw_unique:
  interface_name:
    v.visit_variable_declaration(this->peek().identifier_name(),
                                 Variable_Kind::_interface,
                                 Variable_Declaration_Flags::none);
    this->skip();
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  v.visit_enter_interface_scope();
  if (this->peek().type == Token_Type::less) {
    this->parse_and_visit_typescript_generic_parameters(v);
  }
  if (this->peek().type == Token_Type::kw_extends) {
    this->parse_and_visit_typescript_interface_extends(v);
  }
  if (this->peek().type == Token_Type::left_curly) {
    this->parse_and_visit_typescript_interface_body(
        v,
        /*interface_keyword_span=*/interface_keyword_span);
  } else {
    this->diag_reporter_->report(Diag_Missing_Body_For_TypeScript_Interface{
        .interface_keyword_and_name_and_heritage =
            Source_Code_Span(interface_keyword_span.begin(),
                             this->lexer_.end_of_previous_token()),
    });
  }
  v.visit_exit_interface_scope();
}

void Parser::parse_and_visit_typescript_interface_extends(
    Parse_Visitor_Base &v) {
  QLJS_ASSERT(this->peek().type == Token_Type::kw_extends);

  this->skip();
next_extends:
  this->parse_and_visit_typescript_interface_reference(v);
  if (this->peek().type == Token_Type::comma) {
    // extends IBanana, IOrange
    this->skip();
    goto next_extends;
  }
}

void Parser::parse_and_visit_typescript_interface_reference(
    Parse_Visitor_Base &v) {
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::identifier);
  Identifier ident = this->peek().identifier_name();
  this->skip();

  if (this->peek().type == Token_Type::dot) {
    // extends mynamespace.MyInterface
    // extends ns.subns.I
    while (this->peek().type == Token_Type::dot) {
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::identifier);
      this->skip();
    }
    v.visit_variable_namespace_use(ident);
  } else {
    // extends MyInterface
    v.visit_variable_type_use(ident);
  }

  if (this->peek().type == Token_Type::less) {
    // extends SomeType<Arg>
    this->parse_and_visit_typescript_generic_arguments(v);
  }
}

void Parser::parse_and_visit_typescript_interface_body(
    Parse_Visitor_Base &v, Source_Code_Span interface_keyword_span) {
  QLJS_ASSERT(this->peek().type == Token_Type::left_curly);
  Source_Code_Span left_curly_span = this->peek().span();
  this->skip();

  while (this->peek().type != Token_Type::right_curly) {
    this->parse_and_visit_class_or_interface_member(
        v, Parse_Class_Body_Options{
               .class_or_interface_keyword_span = interface_keyword_span,
               .is_abstract = false,
               .declare_keyword = std::nullopt,
               .is_interface = true,
           });
    if (this->peek().type == Token_Type::end_of_file) {
      this->diag_reporter_->report(Diag_Unclosed_Interface_Block{
          .block_open = left_curly_span,
      });
      return;
    }
  }

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(Token_Type::right_curly);
  this->skip();
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
