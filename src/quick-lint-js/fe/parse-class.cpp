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

    // const C = class implements I {};  // TypeScript only.
  case Token_Type::kw_implements:
    return std::nullopt;

  case Token_Type::kw_interface:
  case Token_Type::kw_package:
  case Token_Type::kw_private:
  case Token_Type::kw_protected:
  case Token_Type::kw_public:
    // TODO(#73): Disallow 'protected', etc. in strict mode.
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
  this->parse_and_visit_typescript_interface_reference(
      v, Statement_Kind::class_implements_clause);
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
    // The location of where this member starts. It could be the start of the
    // first modifier or the beginning of the member's name.
    const Char8 *current_member_begin;

    // *, !, ?, accessor, async, declare, function, get, private, protected,
    // public, readonly, set, static, @ (decorator)
    struct Modifier {
      Source_Code_Span span;
      Token_Type type;

      bool is_access_specifier() const {
        return this->type == Token_Type::kw_private ||
               this->type == Token_Type::kw_protected ||
               this->type == Token_Type::kw_public;
      }
    };
    Vector<Modifier> modifiers =
        Vector<Modifier>("class member modifiers", &p->temporary_memory_);

    // Example:
    //
    // class C {
    //   f(x: number); // TypeScript_Overload_Signature
    //   f(x: string); // TypeScript_Overload_Signature
    //   f(x: number|string) {
    //   }
    // }
    struct TypeScript_Overload_Signature {
      Span<Modifier> modifiers;

      // If empty, this overload signature ended with a newline.
      Span<Source_Code_Span> semicolons;

      // Where a function body would appear if this was not an overload
      // signature.
      const Char8 *expected_body;

      // If nullopt, the property is a computed property.
      //
      // See TODO[TypeScript-overload-signature-with-computed-property].
      std::optional<Identifier> name;

      Source_Code_Span name_span;
    };
    Vector<TypeScript_Overload_Signature> overload_signatures{
        "class overload signatures", &p->temporary_memory_};

    void reset_state_except_overload_signatures() {
      this->current_member_begin = p->peek().begin;
      this->last_ident = std::nullopt;
      this->modifiers.clear();
    }

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
        // declare field;
        case Token_Type::kw_abstract:
        case Token_Type::kw_accessor:
        case Token_Type::kw_async:
        case Token_Type::kw_declare:
        case Token_Type::kw_function:
        case Token_Type::kw_override:
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
            if (this->overload_signatures.empty()) {
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
                this->modifiers.pop_back();
                check_modifiers_for_field_without_type_annotation(last_ident);
                v.visit_property_declaration(last_ident);
                return true;
              default:
                continue;
              }
            } else {
              // class C {
              //   f();     // TypeScript only.
              //   async    // Invalid.
              //   f() {}
              // }
              p->diag_reporter_->report(
                  Diag_Newline_Not_Allowed_Between_Modifier_And_Method_Name{
                      .modifier = last_ident->span(),
                  });
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

        // @decorator f() {}
        case Token_Type::at:
          if (this->is_interface) {
            p->diag_reporter_->report(Diag_Decorator_In_TypeScript_Interface{
                .decorator_at = p->peek().span(),
            });
          }
          modifiers.push_back(Modifier{
              .span = p->peek().span(),
              .type = p->peek().type,
          });
          p->parse_and_visit_decorator(v);
          continue;

        default:
          return false;
        }
      }
    }

    void parse_class_member() {
      // Reset state in case we come here after parsing an overload signature.
      this->reset_state_except_overload_signatures();

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
        } else if (!this->modifiers.empty() &&
                   this->modifiers.back().type == Token_Type::at) {
          // @decorator;  // Invalid.
          const Char8 *end_of_decorator = p->lexer_.end_of_previous_token();
          Source_Code_Span semicolon_span = p->peek().span();
          p->skip();
          if (this->is_interface) {
            // We already reported Diag_Decorator_In_TypeScript_Interface.
          } else if (p->peek().type == Token_Type::right_curly) {
            // class C { @decorator; }  // Invalid.
            p->diag_reporter_->report(Diag_Missing_Class_Member_After_Decorator{
                .expected_member = Source_Code_Span::unit(end_of_decorator),
                .decorator_at = this->modifiers.back().span,
            });
          } else {
            // class C { @decorator; foo() {} }  // Invalid.
            p->diag_reporter_->report(Diag_Unexpected_Semicolon_After_Decorator{
                .semicolon = semicolon_span,
                .decorator_at = this->modifiers.back().span,
            });
          }
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
      // set: any
      case Token_Type::equal:
      case Token_Type::question:
      case Token_Type::right_curly:
      case Token_Type::colon:
        if (!this->overload_signatures.empty()) {
          // class C { method?(); }
          // class C { method(); }   // Invalid.
          this->error_on_overload_signatures();
        } else if (last_ident.has_value()) {
          modifiers.pop_back();
          parse_and_visit_field_or_method(*last_ident);
        } else if (!this->modifiers.empty() &&
                   this->modifiers.back().type == Token_Type::at) {
          if (this->is_interface) {
            // We already reported Diag_Decorator_In_TypeScript_Interface.
          } else {
            p->diag_reporter_->report(Diag_Missing_Class_Member_After_Decorator{
                .expected_member =
                    Source_Code_Span::unit(p->lexer_.end_of_previous_token()),
                .decorator_at = this->modifiers.back().span,
            });
          }
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
            Parameter_List_Options param_options = {
                // FIXME(strager): Shouldn't we possibly set
                // declare_class_keyword?
                .abstract_method_keyword = this->maybe_abstract_keyword_span(),
                .is_class_method = !is_interface,
                .is_interface_method = is_interface,
            };
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
        if (!modifiers.empty() &&
            modifiers.back().type == Token_Type::kw_static) {
          // class C { static { } }
          v.visit_enter_block_scope();
          Source_Code_Span left_curly_span = p->peek().span();
          p->skip();

          error_if_invalid_static_block(/*static_modifier=*/modifiers.back());

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
      switch (p->peek().type) {
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case Token_Type::identifier: {
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
            p->consume_semicolon_or_comma<
                Diag_Missing_Semicolon_After_Index_Signature>();
            break;

          // [key: KeyType];  // Invalid.
          // [key: KeyType],  // Invalid.
          case Token_Type::comma:
          case Token_Type::semicolon:
            p->diag_reporter_->report(
                Diag_TypeScript_Index_Signature_Needs_Type{
                    .expected_type = Source_Code_Span::unit(
                        p->lexer_.end_of_previous_token()),
                });
            p->skip();
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
              p->diag_reporter_->report(
                  Diag_TypeScript_Index_Signature_Needs_Type{
                      .expected_type = Source_Code_Span::unit(
                          p->lexer_.end_of_previous_token()),
                  });
            } else {
              QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
            }
            break;
          }
          v.visit_exit_index_signature_scope();
          return true;
        }
        break;
      }

      default:
        break;
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
            .abstract_method_keyword = this->maybe_abstract_keyword_span(),
            .is_class_constructor =
                property_name.has_value() &&
                property_name->normalized_name() == u8"constructor"_sv,
            .is_class_method = !is_interface,
            .is_interface_method = is_interface,
        };
        bool is_abstract_method = this->find_modifier(Token_Type::kw_abstract);
        if (declare_keyword.has_value() ||
            p->options_.typescript_definition_file) {
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
          // class C { myMethod() {} }
          bool is_possibly_typescript_overload = false;

          v.visit_enter_function_scope();

          Function_Guard guard = p->enter_function(attributes);
          Function_Parameter_Parse_Result result =
              p->parse_and_visit_function_parameter_list(v, property_name_span,
                                                         param_options);
          switch (result) {
          case Function_Parameter_Parse_Result::parsed_parameters:
          case Function_Parameter_Parse_Result::missing_parameters:
            v.visit_enter_function_scope_body();
            p->parse_and_visit_statement_block_no_scope(v);
            property_name = this->check_overload_signatures(property_name,
                                                            property_name_span);
            break;

          case Function_Parameter_Parse_Result::missing_parameters_ignore_body:
            break;

          case Function_Parameter_Parse_Result::parsed_parameters_missing_body:
            if (p->options_.typescript) {
              // class C { myMethod(x: number); myMethod(x: any) {} }
              //
              // This looks like an overload signature. We remember it in
              // this->overload_signatures then parse the next member.
              is_possibly_typescript_overload = true;
              const Char8 *expected_body = p->lexer_.end_of_previous_token();
              Vector<Source_Code_Span> semicolons("semicolons",
                                                  &p->temporary_memory_);
              while (p->peek().type == Token_Type::semicolon) {
                semicolons.push_back(p->peek().span());
                p->skip();
              }
              if (semicolons.empty()) {
                // class C { f() f() {} }  // Invalid.
                p->consume_semicolon<
                    Diag_Missing_Semicolon_After_TypeScript_Method_Overload_Signature>();
              }
              this->overload_signatures.push_back(TypeScript_Overload_Signature{
                  .modifiers = this->modifiers.release_to_span(),
                  .semicolons = semicolons.release_to_span(),
                  .expected_body = expected_body,
                  .name = property_name,
                  .name_span = property_name_span,
              });
            } else if (this->find_modifier(Token_Type::question)) {
              // class C { myMethod?(); }  // Invalid JavaScript.
              QLJS_ASSERT(!p->options_.typescript);
              // Do nothing.
            } else {
              // class C { myMethod(); }  // Invalid.
              p->diag_reporter_->report(Diag_Missing_Function_Body{
                  .expected_body = Source_Code_Span::unit(
                      p->lexer_.end_of_previous_token())});
            }
            break;
          }

          v.visit_exit_function_scope();

          if (is_possibly_typescript_overload) {
            this->parse_class_member();
            break;  // Skip v.visit_property_declaration(property_name).
          }
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
        check_modifiers_for_field_without_type_annotation(property_name);
        v.visit_property_declaration(property_name);
        this->parse_field_terminator();
        break;

        // field = initialValue;
      case Token_Type::equal:
        check_modifiers_for_field_without_type_annotation(property_name);
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
          check_modifiers_for_field_without_type_annotation(property_name);
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
            check_modifiers_for_field_without_type_annotation(property_name);
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
      case Token_Type::at:
      case Token_Type::left_square:
      case Token_Type::number:
      case Token_Type::string:
        if (p->peek().has_leading_newline) {
          // class C {
          //   field        // ASI
          //   [expr]() {}
          // }
          check_modifiers_for_field_without_type_annotation(property_name);
          v.visit_property_declaration(property_name);
        } else {
          QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        }
        break;

      case Token_Type::colon:
        this->check_modifiers_for_field_with_type_annotation(property_name);
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
      if (this->is_interface) {
        // interface I { x; }
        // interface I { x }
        // interface I {
        //   x
        // }
        p->consume_semicolon_or_comma<Diag_Missing_Semicolon_After_Field>();
      } else {
        if (p->peek().type == Token_Type::comma) {
          // class C { x: number, }  // Invalid.
          p->diag_reporter_->report(Diag_Unexpected_Comma_After_Class_Field{
              .comma = p->peek().span(),
          });
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

    std::optional<Source_Code_Span> maybe_abstract_keyword_span() const {
      const Modifier *abstract = this->find_modifier(Token_Type::kw_abstract);
      if (abstract == nullptr) {
        return std::nullopt;
      }
      return abstract->span;
    }

    void check_modifiers_for_field_without_type_annotation(
        const std::optional<Identifier> &field_name) {
      this->check_modifiers_for_field(field_name);

      if (!this->is_interface && p->options_.typescript) {
        if (const Modifier *bang = this->find_modifier(Token_Type::bang)) {
          p->diag_reporter_->report(
              Diag_TypeScript_Assignment_Asserted_Field_Must_Have_A_Type{
                  .bang = bang->span,
              });
        }
      }
    }

    void check_modifiers_for_field_with_type_annotation(
        const std::optional<Identifier> &field_name) {
      this->check_modifiers_for_field(field_name);
    }

    void check_modifiers_for_field(
        const std::optional<Identifier> &field_name) {
      error_if_invalid_access_specifier();
      error_if_getter_setter_field();
      error_if_access_specifier_not_first_in_field();
      error_if_decorator_not_first();
      error_if_abstract_with_decorator();
      error_if_abstract_or_static_after_accessor();
      error_if_override_in_wrong_order();
      error_if_conflicting_modifiers();
      error_if_readonly_in_not_typescript();
      error_if_declare_in_not_typescript();
      error_if_declare_in_interface();
      error_if_declare_of_private_identifier(field_name);
      error_if_declare_field_with_assignment_assertion();
      error_if_override_in_interface();
      error_if_accessor_in_interface();
      error_if_static_in_interface();
      error_if_static_abstract();
      error_if_optional_in_not_typescript();
      error_if_optional_accessor();
      error_if_abstract_not_in_abstract_class();
      error_on_overload_signatures();
    }

    void check_modifiers_for_method() {
      error_if_accessor_method();
      error_if_declare_method();
      error_if_generator_getter_or_setter();
      error_if_async_getter_or_setter();
      error_if_readonly_method();
      error_if_async_or_generator_without_method_body();
      error_if_invalid_access_specifier();
      error_if_access_specifier_not_first_in_method();
      error_if_decorator_not_first();
      error_if_abstract_with_decorator();
      error_if_override_in_wrong_order();
      error_if_override_in_interface();
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
      if (const Modifier *decorator_modifier =
              this->find_modifier(Token_Type::at)) {
        p->diag_reporter_->report(
            Diag_Decorator_Not_Allowed_On_Class_Static_Block{
                .decorator_at = decorator_modifier->span,
                .static_keyword = static_modifier.span,
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

    void error_if_declare_method() {
      if (const Modifier *declare_modifier =
              this->find_modifier(Token_Type::kw_declare)) {
        p->diag_reporter_->report(Diag_TypeScript_Declare_Method{
            .declare_keyword = declare_modifier->span,
        });
      }
    }

    void error_if_generator_getter_or_setter() {
      if (const Modifier *star_modifier = find_modifier(Token_Type::star)) {
        if (const Modifier *get_modifier = find_modifier(Token_Type::kw_get)) {
          p->diag_reporter_->report(Diag_Class_Generator_On_Getter_Or_Setter{
              .star_token = star_modifier->span,
              .getter_setter_keyword = get_modifier->span,
          });
        } else if (const Modifier *set_modifier =
                       find_modifier(Token_Type::kw_set)) {
          p->diag_reporter_->report(Diag_Class_Generator_On_Getter_Or_Setter{
              .star_token = star_modifier->span,
              .getter_setter_keyword = set_modifier->span,
          });
        }
      }
    }

    void error_if_async_getter_or_setter() {
      if (const Modifier *async_modifier =
              find_modifier(Token_Type::kw_async)) {
        if (const Modifier *get_modifier = find_modifier(Token_Type::kw_get)) {
          p->diag_reporter_->report(Diag_Class_Async_On_Getter_Or_Setter{
              .async_keyword = async_modifier->span,
              .getter_setter_keyword = get_modifier->span,
          });
        } else if (const Modifier *set_modifier =
                       find_modifier(Token_Type::kw_set)) {
          p->diag_reporter_->report(Diag_Class_Async_On_Getter_Or_Setter{
              .async_keyword = async_modifier->span,
              .getter_setter_keyword = set_modifier->span,
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

    void error_if_declare_in_not_typescript() {
      if (!p->options_.typescript) {
        if (const Modifier *declare_modifier =
                this->find_modifier(Token_Type::kw_declare)) {
          p->diag_reporter_->report(
              Diag_TypeScript_Declare_Field_Not_Allowed_In_JavaScript{
                  .declare_keyword = declare_modifier->span,
              });
        }
      }
    }

    void error_if_declare_in_interface() {
      if (this->is_interface) {
        if (const Modifier *declare_modifier =
                this->find_modifier(Token_Type::kw_declare)) {
          p->diag_reporter_->report(Diag_Interface_Field_Cannot_Be_Declare{
              .declare_keyword = declare_modifier->span,
          });
        }
      }
    }

    void error_if_declare_of_private_identifier(
        const std::optional<Identifier> &field_name) {
      if (field_name.has_value() && field_name->is_private_identifier()) {
        if (const Modifier *declare_modifier =
                this->find_modifier(Token_Type::kw_declare)) {
          const Char8 *field_name_begin = field_name->span().begin();
          p->diag_reporter_->report(
              Diag_TypeScript_Declare_Field_Cannot_Use_Private_Identifier{
                  .private_identifier_hash =
                      Source_Code_Span(field_name_begin, field_name_begin + 1),
                  .declare_keyword = declare_modifier->span,
              });
        }
      }
    }

    void error_if_declare_field_with_assignment_assertion() {
      if (const Modifier *declare_modifier =
              this->find_modifier(Token_Type::kw_declare)) {
        if (const Modifier *assignment_assertion_modifier =
                this->find_modifier(Token_Type::bang)) {
          p->diag_reporter_->report(
              Diag_TypeScript_Declare_Field_Cannot_Be_Assignment_Asserted{
                  .bang = assignment_assertion_modifier->span,
                  .declare_keyword = declare_modifier->span,
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
                Token_Type::kw_override,
                Token_Type::kw_readonly,
                Token_Type::kw_static,
            }));
      }
    }

    void error_if_access_specifier_not_first_in_method() {
      if (p->options_.typescript && !modifiers.empty()) {
        check_if_access_specifier_precedes_given_modifiers(
            Span<const Token_Type>({
                Token_Type::kw_async,
                Token_Type::kw_override,
                Token_Type::kw_static,
            }));
      }
    }

    void error_if_decorator_not_first() {
      if (this->is_interface) {
        // We already reported Diag_Decorator_In_TypeScript_Interface.
        return;
      }
      Span_Size i = 0;
      for (; i < this->modifiers.size() &&
             this->modifiers[i].type == Token_Type::at;
           ++i) {
        // Skip decorators.
      }
      if (i < this->modifiers.size()) {
        // There is at least one non-decorator modifier.
        const Modifier *non_decorator_modifier = &this->modifiers[i];
        if (non_decorator_modifier->type == Token_Type::kw_abstract) {
          // abstract @decorator myMethod() {}  // Invalid.
          // Diag_Decorator_On_Abstract_Class_Member is reported elsewhere.
          return;
        }
        for (; i < this->modifiers.size(); ++i) {
          if (this->modifiers[i].type == Token_Type::at) {
            p->diag_reporter_->report(
                Diag_Decorator_After_Class_Member_Modifiers{
                    .decorator_at = this->modifiers[i].span,
                    .modifier = non_decorator_modifier->span,
                });
          }
        }
      }
    }

    void error_if_abstract_with_decorator() {
      if (const Modifier *abstract_modifier =
              this->find_modifier(Token_Type::kw_abstract)) {
        for (const Modifier &other_modifier : this->modifiers) {
          if (other_modifier.type == Token_Type::at) {
            // @decorator abstract myMethod() {}  // Invalid.
            p->diag_reporter_->report(Diag_Decorator_On_Abstract_Class_Member{
                .decorator_at = other_modifier.span,
                .abstract_keyword = abstract_modifier->span,
            });
          }
        }
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

        if (const Modifier *override_modifier =
                this->find_modifier(Token_Type::kw_override)) {
          if (override_modifier > accessor_modifier) {
            p->diag_reporter_->report(
                Diag_Class_Modifier_Must_Preceed_Other_Modifier{
                    .expected_first_modifier = override_modifier->span,
                    .expected_second_modifier = accessor_modifier->span,
                });
          }
        }
      }
    }

    void error_if_override_in_wrong_order() {
      if (const Modifier *override_modifier =
              this->find_modifier(Token_Type::kw_override)) {
        if (const Modifier *static_modifier =
                this->find_modifier(Token_Type::kw_static)) {
          if (static_modifier > override_modifier) {
            p->diag_reporter_->report(
                Diag_Class_Modifier_Must_Preceed_Other_Modifier{
                    .expected_first_modifier = static_modifier->span,
                    .expected_second_modifier = override_modifier->span,
                });
          }
        }

        if (const Modifier *abstract_modifier =
                this->find_modifier(Token_Type::kw_abstract)) {
          if (abstract_modifier > override_modifier) {
            p->diag_reporter_->report(
                Diag_Class_Modifier_Must_Preceed_Other_Modifier{
                    .expected_first_modifier = abstract_modifier->span,
                    .expected_second_modifier = override_modifier->span,
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

    void error_if_override_in_interface() {
      if (is_interface) {
        if (const Modifier *override_modifier =
                find_modifier(Token_Type::kw_override)) {
          p->diag_reporter_->report(
              Diag_Override_Property_Not_Allowed_In_Interface{
                  .override_keyword = override_modifier->span,
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

    // If a better property name is discovered, the better name is returned.
    // Otherwise, property_name is returned.
    [[nodiscard]] std::optional<Identifier> check_overload_signatures(
        std::optional<Identifier> property_name,
        Source_Code_Span property_name_span) {
      if (this->overload_signatures.empty()) {
        return property_name;
      }

      // Given the following example, set property_name to 'a'.
      //
      //   class C { a(); "b"() {} }
      //
      // This shouldn't be necessary. See
      // TODO[TypeScript-overload-signature-with-computed-property].
      if (!property_name.has_value()) {
        for (TypeScript_Overload_Signature &signature :
             this->overload_signatures) {
          if (signature.name.has_value()) {
            property_name = signature.name;
            break;
          }
        }
      }

      const Modifier *method_get_or_set_modifier = nullptr;
      if (method_get_or_set_modifier == nullptr) {
        method_get_or_set_modifier = this->find_modifier(Token_Type::kw_get);
      }
      if (method_get_or_set_modifier == nullptr) {
        method_get_or_set_modifier = this->find_modifier(Token_Type::kw_set);
      }

      const Modifier *method_question_modifier =
          this->find_modifier(Token_Type::question);
      const Modifier *method_static_modifier =
          this->find_modifier(Token_Type::kw_static);
      const Modifier *method_access_specifier = this->find_access_specifier();

      bool have_likely_overload_signature = false;
      for (TypeScript_Overload_Signature &signature :
           this->overload_signatures) {
        if (const Modifier *star_modifier =
                this->find_modifier(Token_Type::star, signature.modifiers)) {
          p->diag_reporter_->report(
              Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star{
                  .generator_star = star_modifier->span,
              });
        }
        if (const Modifier *decorator_modifier =
                this->find_modifier(Token_Type::at, signature.modifiers)) {
          p->diag_reporter_->report(Diag_Decorator_On_Overload_Signature{
              .decorator_at = decorator_modifier->span,
              .expected_location =
                  Source_Code_Span::unit(this->current_member_begin),
          });
        }

        const Modifier *signature_get_or_set_modifier = nullptr;
        if (signature_get_or_set_modifier == nullptr) {
          signature_get_or_set_modifier =
              this->find_modifier(Token_Type::kw_get, signature.modifiers);
        }
        if (signature_get_or_set_modifier == nullptr) {
          signature_get_or_set_modifier =
              this->find_modifier(Token_Type::kw_set, signature.modifiers);
        }

        if (signature.name.has_value() && property_name.has_value() &&
            signature.name->normalized_name() !=
                property_name->normalized_name()) {
          if (this->find_modifier(Token_Type::question, signature.modifiers) !=
              nullptr) {
            // class C {
            //   f?();  // Valid; no body.
            //   g() {}
            // }
            v.visit_property_declaration(*signature.name);
          } else if (signature.semicolons.empty() ||
                     signature_get_or_set_modifier != nullptr) {
            // class C {
            //   f()      // Invalid; missing body.
            //   g() {}
            // }
            //
            // class C {
            //   get f();  // Invalid; missing body
            //   get g() {}
            // }
            p->diag_reporter_->report(Diag_Missing_Function_Body{
                .expected_body =
                    Source_Code_Span::unit(signature.expected_body),
            });
            v.visit_property_declaration(*signature.name);
          } else {
            // class C {
            //   f();     // Invalid; mismatched names.
            //   g() {}
            // }
            have_likely_overload_signature = true;
            p->diag_reporter_->report(
                Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name{
                    .overload_name = signature.name->span(),
                    .function_name = property_name->span(),
                });
          }
        } else {
          have_likely_overload_signature = true;

          if (signature_get_or_set_modifier != nullptr &&
              method_get_or_set_modifier == nullptr) {
            // class C { get m(); m() {} }  // Invalid.
            p->diag_reporter_->report(
                Diag_Class_Modifier_Not_Allowed_On_TypeScript_Overload_Signature{
                    .modifier = signature_get_or_set_modifier->span,
                });
          }

          const Modifier *signature_question_modifier =
              this->find_modifier(Token_Type::question, signature.modifiers);
          this->error_if_signature_and_method_have_mismatched_modifier_presence(
              signature_question_modifier, signature.name_span.end(),
              method_question_modifier, property_name_span.end());

          const Modifier *signature_static_modifier =
              this->find_modifier(Token_Type::kw_static, signature.modifiers);
          this->error_if_signature_and_method_have_mismatched_modifier_presence(
              signature_static_modifier, signature.name_span.begin(),
              method_static_modifier, property_name_span.begin());

          const Modifier *signature_access_specifier =
              this->find_access_specifier(signature.modifiers);
          if (is_explicitly_or_implicitly_public(method_access_specifier) &&
              is_explicitly_or_implicitly_public(signature_access_specifier)) {
            // f(); public f() {}  // OK.
            // public f(); f() {}  // OK.
            // Emit no diagnostic.
          } else {
            this->error_if_signature_and_method_have_mismatched_modifier_presence(
                signature_access_specifier, signature.name_span.begin(),
                method_access_specifier, property_name_span.begin());
          }

          if (method_access_specifier != nullptr &&
              signature_access_specifier != nullptr) {
            if (method_access_specifier->type !=
                signature_access_specifier->type) {
              // class C { public f(); private f() {} }  // Invalid.
              p->diag_reporter_->report(
                  Diag_TypeScript_Overload_Signature_Access_Specifier_Mismatch{
                      .method_access_specifier = method_access_specifier->span,
                      .signature_access_specifier =
                          signature_access_specifier->span,
                  });
            }
          }

          for (Span_Size i = 1; i < signature.semicolons.size(); ++i) {
            // class C { m();; m() {} }  // Invalid.
            p->diag_reporter_->report(
                Diag_Unexpected_Semicolon_After_Overload_Signature{
                    .extra_semicolon = signature.semicolons[i],
                    .original_semicolon = signature.semicolons[0],
                });
          }
        }
      }

      if (have_likely_overload_signature) {
        if (method_get_or_set_modifier != nullptr) {
          // class C { m(); get m() {} }  // Invalid.
          p->diag_reporter_->report(
              Diag_Getter_Or_Setter_Cannot_Have_TypeScript_Overload_Signature{
                  .get_or_set_token = method_get_or_set_modifier->span,
              });
        }
      }

      return property_name;
    }

    void error_if_signature_and_method_have_mismatched_modifier_presence(
        const Modifier *signature_modifier,
        const Char8 *expected_signature_modifier_location,
        const Modifier *method_modifier,
        const Char8 *expected_method_modifier_location) {
      if ((signature_modifier != nullptr) && (method_modifier == nullptr)) {
        // class C { m?(); m() {} }  // Invalid.
        p->diag_reporter_->report(
            Diag_Class_Modifier_Missing_On_Method_With_TypeScript_Overload_Signature{
                .signature_modifier = signature_modifier->span,
                .missing_method_modifier =
                    Source_Code_Span::unit(expected_method_modifier_location),
            });
      }
      if ((signature_modifier == nullptr) && (method_modifier != nullptr)) {
        // class C { m(); m?() {} }  // Invalid.
        p->diag_reporter_->report(
            Diag_Class_Modifier_Missing_On_TypeScript_Overload_Signature{
                .missing_signature_modifier = Source_Code_Span::unit(
                    expected_signature_modifier_location),
                .method_modifier = method_modifier->span,
            });
      }
    }

    static bool is_explicitly_or_implicitly_public(
        const Modifier *access_specifier) {
      return access_specifier == nullptr ||
             access_specifier->type == Token_Type::kw_public;
    }

    void error_on_overload_signatures() {
      for (TypeScript_Overload_Signature &signature :
           this->overload_signatures) {
        if (signature.name.has_value()) {
          v.visit_property_declaration(*signature.name);
        }
        if (this->find_modifier(Token_Type::question, signature.modifiers) !=
            nullptr) {
          // class C { method?(); }
        } else {
          // class C { method(); }  // Invalid.
          p->diag_reporter_->report(Diag_Missing_Function_Body{
              .expected_body = Source_Code_Span::unit(signature.expected_body),
          });
        }
      }
    }

    const Modifier *find_modifier(Token_Type modifier_type) const {
      return this->find_modifier(modifier_type,
                                 Span<const Modifier>(this->modifiers));
    }

    static const Modifier *find_modifier(Token_Type modifier_type,
                                         Span<const Modifier> modifiers) {
      for (const Modifier &m : modifiers) {
        if (m.type == modifier_type) {
          return &m;
        }
      }
      return nullptr;
    }

    const Modifier *find_access_specifier() const {
      return this->find_access_specifier(Span<const Modifier>(this->modifiers));
    }

    static const Modifier *find_access_specifier(
        Span<const Modifier> modifiers) {
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
  state.parse_class_member();
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
  this->parse_and_visit_typescript_interface_reference(
      v, Statement_Kind::interface_extends_clause);
  if (this->peek().type == Token_Type::comma) {
    // extends IBanana, IOrange
    this->skip();
    goto next_extends;
  }
}

void Parser::parse_and_visit_typescript_interface_reference(
    Parse_Visitor_Base &v, Statement_Kind context) {
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

  if (this->peek().type == Token_Type::less ||
      this->peek().type == Token_Type::less_less) {
    // extends SomeType<Arg>
    // extends SomeType< <T>() => ReturnType>
    if (this->peek().type == Token_Type::less_less) {
      // extends SomeType<<T>() => ReturnType>  // Invalid.
      const Char8 *second_less = this->peek().begin + 1;
      this->diag_reporter_->report(Diag_TypeScript_Generic_Less_Less_Not_Split{
          .expected_space = Source_Code_Span::unit(second_less),
          .context = context,
      });
    }
    this->parse_and_visit_typescript_generic_arguments(v, /*in_jsx=*/false);
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
