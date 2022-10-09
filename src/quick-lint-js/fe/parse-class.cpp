// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdlib>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/fe/buffering-visitor.h>
#include <quick-lint-js/fe/diag-reporter.h>
#include <quick-lint-js/fe/diagnostic-types.h>
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
#include <utility>

namespace quick_lint_js {
void parser::parse_and_visit_class(
    parse_visitor_base &v, parser::name_requirement require_name,
    std::optional<source_code_span> abstract_keyword_span) {
  QLJS_ASSERT(this->peek().type == token_type::kw_class);
  source_code_span class_keyword_span = this->peek().span();

  if (abstract_keyword_span.has_value() && !this->options_.typescript) {
    this->diag_reporter_->report(
        diag_typescript_abstract_class_not_allowed_in_javascript{
            .abstract_keyword = *abstract_keyword_span,
        });
  }

  std::optional<identifier> class_name = this->parse_class_and_optional_name();

  v.visit_enter_class_scope();
  this->parse_and_visit_class_heading_after_name(v);
  v.visit_enter_class_scope_body(class_name);

  switch (this->peek().type) {
  case token_type::left_curly:
    this->parse_and_visit_class_body(
        v,
        /*class_keyword_span=*/class_keyword_span,
        /*is_abstract=*/abstract_keyword_span.has_value());
    break;

  default:
    this->diag_reporter_->report(diag_missing_body_for_class{
        .class_keyword_and_name_and_heritage =
            source_code_span::unit(this->lexer_.end_of_previous_token()),
    });
    break;
  }
  v.visit_exit_class_scope();

  this->visit_class_name(v, class_name, class_keyword_span, require_name);
}

std::optional<identifier> parser::parse_class_and_optional_name() {
  QLJS_ASSERT(this->peek().type == token_type::kw_class);
  this->skip();

  switch (this->peek().type) {
  case token_type::kw_await:
    if (this->in_async_function_) {
      this->diag_reporter_->report(diag_cannot_declare_await_in_async_function{
          .name = this->peek().identifier_name(),
      });
    }
    goto class_name;

  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    [[fallthrough]];
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier:
  case token_type::kw_yield:
    // TODO(#707): Disallow classes named 'yield' in generator function.
  class_name : {
    if (this->peek().type == token_type::kw_let) {
      this->diag_reporter_->report(diag_cannot_declare_class_named_let{
          .name = this->peek().identifier_name().span()});
    }
    identifier name = this->peek().identifier_name();
    this->skip();
    return name;
  }

    // class { ... }
  case token_type::left_curly:
    return std::nullopt;

    // class extends C { }
  case token_type::kw_extends:
    return std::nullopt;

    // { class }  // Invalid.
    // class;     // Invalid.
  default:
    // We'll report errors later.
    return std::nullopt;
  }
}

void parser::parse_and_visit_class_heading_after_name(parse_visitor_base &v) {
  if (this->peek().type == token_type::less) {
    if (!this->options_.typescript) {
      this->diag_reporter_->report(
          diag_typescript_generics_not_allowed_in_javascript{
              .opening_less = this->peek().span(),
          });
    }
    this->parse_and_visit_typescript_generic_parameters(v);
  }

  std::optional<source_code_span> extends_keyword;
  std::optional<source_code_span> implements_keyword;

  if (this->peek().type == token_type::kw_extends) {
    // class C extends Base {}
    extends_keyword = this->peek().span();
    this->parse_and_visit_class_extends(v);
  }

  if (this->peek().type == token_type::kw_implements) {
    // class C implements Iface {}
    implements_keyword = this->peek().span();
    this->parse_and_visit_typescript_class_implements(v);
  }

  if (this->peek().type == token_type::kw_extends &&
      !extends_keyword.has_value() && implements_keyword.has_value()) {
    // class C implements Iface extends Base {}  // Invalid.
    extends_keyword = this->peek().span();
    if (this->options_.typescript) {
      this->diag_reporter_->report(
          diag_typescript_implements_must_be_after_extends{
              .implements_keyword = *implements_keyword,
              .extends_keyword = *extends_keyword,
          });
    } else {
      // We already reported
      // diag_typescript_class_implements_not_allowed_in_javascript. Don't
      // report another diagnostic.
    }
    this->parse_and_visit_class_extends(v);
  }
}

void parser::parse_and_visit_class_extends(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_extends);
  this->skip();
  // TODO(strager): Error when extending things like '0' or 'true'.
  this->parse_and_visit_expression(v, precedence{
                                          .commas = false,
                                          .trailing_curly_is_arrow_body = false,
                                      });
}

void parser::parse_and_visit_typescript_class_implements(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_implements);
  if (!this->options_.typescript) {
    this->diag_reporter_->report(
        diag_typescript_class_implements_not_allowed_in_javascript{
            .implements_keyword = this->peek().span(),
        });
  }
  this->skip();
next_implements:
  this->parse_and_visit_typescript_interface_reference(v);
  if (this->peek().type == token_type::comma) {
    // implements IBanana, IOrange
    this->skip();
    goto next_implements;
  }
}

void parser::visit_class_name(parse_visitor_base &v,
                              std::optional<identifier> class_name,
                              source_code_span class_keyword_span,
                              name_requirement require_name) {
  if (class_name.has_value()) {
    v.visit_variable_declaration(*class_name, variable_kind::_class,
                                 variable_init_kind::normal);
  } else {
    switch (require_name) {
    case name_requirement::optional:
      break;
    case name_requirement::required_for_export:
      this->diag_reporter_->report(diag_missing_name_of_exported_class{
          .class_keyword = class_keyword_span,
      });
      break;
    case name_requirement::required_for_statement:
      this->diag_reporter_->report(diag_missing_name_in_class_statement{
          .class_keyword = class_keyword_span,
      });
      break;
    }
  }
}

void parser::parse_and_visit_class_body(parse_visitor_base &v,
                                        source_code_span class_keyword_span,
                                        bool is_abstract) {
  class_guard g = this->enter_class();

  source_code_span left_curly_span = this->peek().span();
  this->skip();

  while (this->peek().type != token_type::right_curly) {
    this->parse_and_visit_class_or_interface_member(
        v,
        /*class_or_interface_keyword_span=*/class_keyword_span,
        /*is_interface=*/false,
        /*is_abstract=*/is_abstract);
    if (this->peek().type == token_type::end_of_file) {
      this->diag_reporter_->report(diag_unclosed_class_block{
          .block_open = left_curly_span,
      });
      return;
    }
  }

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
  this->skip();
}

void parser::parse_and_visit_class_or_interface_member(
    parse_visitor_base &v, source_code_span class_or_interface_keyword_span,
    bool is_interface, bool is_abstract) {
  struct class_parser {
    explicit class_parser(parser *p, parse_visitor_base &v,
                          source_code_span class_or_interface_keyword_span,
                          bool is_interface, bool is_abstract)
        : p(p),
          v(v),
          class_or_interface_keyword_span(class_or_interface_keyword_span),
          is_interface(is_interface),
          is_abstract(is_abstract) {}

    parser *p;
    parse_visitor_base &v;
    source_code_span class_or_interface_keyword_span;
    bool is_interface;
    bool is_abstract;

    std::optional<identifier> last_ident;

    // *, !, ?, async, function, get, private, protected, public, readonly, set,
    // static
    struct modifier {
      source_code_span span;
      token_type type;

      bool is_access_specifier() const noexcept {
        return this->type == token_type::kw_private ||
               this->type == token_type::kw_protected ||
               this->type == token_type::kw_public;
      }
    };
    bump_vector<modifier, monotonic_allocator> modifiers =
        bump_vector<modifier, monotonic_allocator>("class member modifiers",
                                                   &p->temporary_memory_);

    // Returns true if an entire property was parsed.
    // Returns false if nothing was parsed or if only modifiers were parser.
    bool parse_modifiers() {
      for (;;) {
        switch (p->peek().type) {
        // async f() {}
        // abstract f();
        case token_type::kw_abstract:
        case token_type::kw_async:
          last_ident = p->peek().identifier_name();
          modifiers.push_back(modifier{
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
            case token_type::left_square:
            case token_type::number:
            case token_type::string:
            case token_type::identifier:
            case token_type::private_identifier:
            case token_type::star:
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
        case token_type::star:
          modifiers.push_back(modifier{
              .span = p->peek().span(),
              .type = p->peek().type,
          });
          p->skip();
          continue;

        // get prop() {}
        case token_type::kw_get:
        case token_type::kw_set:
          last_ident = p->peek().identifier_name();
          modifiers.push_back(modifier{
              .span = p->peek().span(),
              .type = p->peek().type,
          });
          p->skip();
          continue;

        // private f() {}
        // public static field = 42;
        case token_type::kw_private:
        case token_type::kw_protected:
        case token_type::kw_public:
          last_ident = p->peek().identifier_name();
          modifiers.push_back(modifier{
              .span = p->peek().span(),
              .type = p->peek().type,
          });
          p->skip();
          continue;

        // static f() {}
        case token_type::kw_static:
          last_ident = p->peek().identifier_name();
          modifiers.push_back(modifier{
              .span = p->peek().span(),
              .type = p->peek().type,
          });
          p->skip();
          continue;

        // readonly field: number;
        case token_type::kw_readonly:
          last_ident = p->peek().identifier_name();
          modifiers.push_back(modifier{
              .span = p->peek().span(),
              .type = p->peek().type,
          });
          p->skip();
          continue;

        // function() {}
        // function f() {}  // Invalid.
        case token_type::kw_function:
          last_ident = p->peek().identifier_name();
          modifiers.push_back(modifier{
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
      case token_type::private_identifier:
        if (is_interface) {
          p->diag_reporter_->report(diag_interface_properties_cannot_be_private{
              .property_name_or_private_keyword = p->peek().span(),
          });
        }
        [[fallthrough]];
      QLJS_CASE_RESERVED_KEYWORD_EXCEPT_FUNCTION:
      QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case token_type::identifier:
      case token_type::reserved_keyword_with_escape_sequence: {
        identifier property_name = p->peek().identifier_name();
        p->skip();
        parse_and_visit_field_or_method(property_name);
        break;
      }

      // "method"() {}
      // 9001() {}
      // "fieldName" = init;
      case token_type::number:
      case token_type::string: {
        source_code_span name_span = p->peek().span();
        p->skip();
        parse_and_visit_field_or_method_without_name(name_span);
        break;
      }

      // [methodNameExpression]() {}
      // [fieldNameExpression] = initialValue;
      // [key: KeyType]: ValueType;  // TypeScript only
      case token_type::left_square: {
        const char8 *name_begin = p->peek().begin;
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
            p, token_type::right_square);
        const char8 *name_end = p->peek().end;
        p->skip();

        parse_and_visit_field_or_method_without_name(
            source_code_span(name_begin, name_end));
        break;
      }

      // async;  // Field named 'async'.
      // ;       // Stray semicolon.
      case token_type::semicolon:
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
      case token_type::left_paren:
        if (last_ident.has_value()) {
          modifiers.pop_back();
          parse_and_visit_field_or_method(*last_ident);
        } else {
          source_code_span expected_name(p->peek().begin, p->peek().begin);
          if (is_interface) {
            // (): void;
          } else {
            // (params) {}  // Invalid.
            p->diag_reporter_->report(diag_missing_class_method_name{
                .expected_name = expected_name,
            });
          }
          parse_and_visit_field_or_method_without_name(expected_name);
        }
        break;

      // async?() {}
      // class C { get }  // Field named 'get'
      // get = init;
      case token_type::equal:
      case token_type::question:
      case token_type::right_curly:
        if (last_ident.has_value()) {
          modifiers.pop_back();
          parse_and_visit_field_or_method(*last_ident);
        } else {
          QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        }
        break;

      // async<T>(): void;     // TypeScript generic method.
      // <T>(param: T): void;  // TypeScript generic interface call signature.
      case token_type::less:
        if (last_ident.has_value()) {
          // set<T>(): void;
          modifiers.pop_back();
          parse_and_visit_field_or_method(*last_ident);
        } else {
          // <T>(param: T): void;
          source_code_span property_name_span(p->peek().begin, p->peek().begin);

          v.visit_property_declaration(std::nullopt);
          v.visit_enter_function_scope();
          {
            function_attributes attributes =
                function_attributes_from_modifiers(std::nullopt);
            if (is_interface) {
              p->parse_and_visit_interface_function_parameters_and_body_no_scope(
                  v, property_name_span, attributes);
            } else {
              p->parse_and_visit_function_parameters_and_body_no_scope(
                  v, property_name_span, attributes);
              p->diag_reporter_->report(diag_missing_class_method_name{
                  .expected_name = property_name_span,
              });
            }
          }
          v.visit_exit_function_scope();
        }
        break;

      // class C { {             // Invalid.
      // class C { static { } }  // TypeScript only.
      case token_type::left_curly:
        if (modifiers.size() == 1 &&
            modifiers[0].type == token_type::kw_static) {
          // class C { static { } }
          error_if_invalid_static_block(/*static_modifier=*/modifiers[0]);
          v.visit_enter_block_scope();
          p->parse_and_visit_statement_block_no_scope(v);
          v.visit_exit_block_scope();
        } else {
          // class C { {  // Invalid.
          p->diag_reporter_->report(diag_unexpected_token{
              .token = p->peek().span(),
          });
          p->skip();
        }
        break;

      case token_type::comma:
        p->diag_reporter_->report(diag_comma_not_allowed_between_class_methods{
            .unexpected_comma = p->peek().span(),
        });
        p->skip();
        break;

      // class C {  // Invalid.
      case token_type::end_of_file:
        // An error is reported by parse_and_visit_class_body.
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        break;
      }
    }

    // Returns true if an index signature was parsed.
    // Returns false if nothing was parsed.
    //
    // [key: KeyType]: ValueType;  // TypeScript only
    bool try_parse_typescript_index_signature(const char8 *left_square_begin) {
      lexer_transaction transaction = p->lexer_.begin_transaction();
      // TODO(strager): Allow certain contextual keywords like 'let'?
      if (p->peek().type == token_type::identifier) {
        identifier key_variable = p->peek().identifier_name();
        p->skip();
        if (p->peek().type == token_type::colon) {
          // [key: KeyType]: ValueType;
          v.visit_enter_index_signature_scope();
          p->lexer_.commit_transaction(std::move(transaction));
          p->parse_and_visit_typescript_colon_type_expression(v);

          QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN_WITH_PARSER(
              p, token_type::right_square);
          const char8 *name_end = p->peek().end;
          p->skip();

          v.visit_variable_declaration(
              key_variable, variable_kind::_index_signature_parameter,
              variable_init_kind::normal);

          switch (p->peek().type) {
          // [key: KeyType]: ValueType;
          case token_type::colon:
            p->parse_and_visit_typescript_colon_type_expression(v);
            p->consume_semicolon<
                diag_missing_semicolon_after_index_signature>();
            break;

          // [key: KeyType];  // Invalid.
          case token_type::semicolon:
          missing_type_for_index_signature:
            p->diag_reporter_->report(
                diag_typescript_index_signature_needs_type{
                    .expected_type = source_code_span::unit(
                        p->lexer_.end_of_previous_token()),
                });
            break;

          // [key: KeyType]  // Invalid.
          // ();
          //
          // [key: KeyType]();  // Invalid.
          case token_type::left_paren: {
            if (p->peek().has_leading_newline) {
              QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
            } else {
              p->diag_reporter_->report(
                  diag_typescript_index_signature_cannot_be_method{
                      .left_paren = p->peek().span(),
                  });
              parse_and_visit_field_or_method_without_name(
                  source_code_span(left_square_begin, name_end));
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

    void parse_and_visit_field_or_method(identifier property_name) {
      parse_and_visit_field_or_method_impl(property_name, property_name.span());
    }

    void parse_and_visit_field_or_method_without_name(
        source_code_span name_span) {
      parse_and_visit_field_or_method_impl(std::nullopt, name_span);
    }

    void parse_and_visit_field_or_method_impl(
        std::optional<identifier> property_name,
        source_code_span property_name_span) {
      if (p->peek().type == token_type::question) {
        // field?: Type;
        modifiers.push_back(modifier{
            .span = p->peek().span(),
            .type = p->peek().type,
        });
        p->skip();
      }

      if (p->peek().type == token_type::bang) {
        // field!: Type;
        source_code_span bang_span = p->peek().span();
        if (p->peek().has_leading_newline) {
          p->diag_reporter_->report(
              diag_newline_not_allowed_before_assignment_assertion_operator{
                  .bang = bang_span,
                  .field_name = property_name_span,
              });
        }
        modifiers.push_back(modifier{
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
      case token_type::left_curly:
      case token_type::left_paren:
      case token_type::less: {
        v.visit_property_declaration(property_name);

        if (p->options_.typescript) {
          if (const modifier *assignment_assertion_modifier =
                  find_modifier(token_type::bang)) {
            if (p->peek().has_leading_newline) {
              // field!
              // () {}  // Invalid for classes.

              // Stop parsing the field. We will report
              // diag_missing_class_method_name later if needed.
              break;
            } else {
              // method!() {}  // Invalid.
              p->diag_reporter_->report(
                  diag_typescript_assignment_asserted_method{
                      .bang = assignment_assertion_modifier->span,
                  });
            }
          }
        }
        check_modifiers_for_method();

        function_attributes attributes =
            function_attributes_from_modifiers(property_name);
        bool is_abstract_method = this->find_modifier(token_type::kw_abstract);
        if (is_abstract_method) {
          v.visit_enter_function_scope();
          p->parse_and_visit_abstract_function_parameters_and_body_no_scope(
              v, property_name_span, attributes);
          v.visit_exit_function_scope();
        } else if (is_interface) {
          v.visit_enter_function_scope();
          p->parse_and_visit_interface_function_parameters_and_body_no_scope(
              v, property_name_span, attributes);
          v.visit_exit_function_scope();
        } else {
          p->parse_and_visit_function_parameters_and_body(
              v, /*name=*/property_name_span, attributes);
        }
        break;
      }

        // field;
        // class C { field }
        // class C { field  // Invalid.
      case token_type::end_of_file:
      case token_type::right_curly:
      case token_type::semicolon:
        check_modifiers_for_field_without_type_annotation();
        v.visit_property_declaration(property_name);
        p->consume_semicolon<diag_missing_semicolon_after_field>();
        break;

        // field = initialValue;
      case token_type::equal:
        check_modifiers_for_field_without_type_annotation();
        this->parse_field_initializer();
        v.visit_property_declaration(property_name);
        p->consume_semicolon<diag_missing_semicolon_after_field>();
        break;

      case token_type::identifier:
      case token_type::private_identifier:
      case token_type::star:
        if (p->peek().has_leading_newline) {
          // class C {
          //   field        // ASI
          //   method() {}
          // }
          check_modifiers_for_field_without_type_annotation();
          v.visit_property_declaration(property_name);
        } else {
          if (u8"const" == property_name_span.string_view()) {
            // class C {
            //   const field
            // }
            p->diag_reporter_->report(diag_typescript_style_const_field{
                .const_token = property_name_span,
            });
          } else if (find_modifier(token_type::question)) {
            // class C {
            //   field? method() {}  // Invalid.
            // }
            check_modifiers_for_field_without_type_annotation();
            v.visit_property_declaration(property_name);
            p->consume_semicolon<diag_missing_semicolon_after_field>();
          } else {
            // class C {
            //   asyn method() {}  // Invalid.
            // }
            p->diag_reporter_->report(diag_unexpected_token{
                .token = property_name_span,
            });
          }
        }
        break;

      QLJS_CASE_KEYWORD:
      case token_type::left_square:
      case token_type::number:
      case token_type::string:
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

      case token_type::colon:
        this->check_modifiers_for_field_with_type_annotation();
        if (this->find_modifier(token_type::bang)) {
          // If we have a bang modifier, we already reported
          // diag_typescript_assignment_asserted_fields_not_allowed_in_javascript.
          p->skip();
        } else {
          p->parse_typescript_colon_for_type();
        }
        p->parse_and_visit_typescript_type_expression(v);

        if (p->peek().type == token_type::equal) {
          this->parse_field_initializer();
        }
        v.visit_property_declaration(property_name);
        p->consume_semicolon<diag_missing_semicolon_after_field>();
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        break;
      }
    }

    void parse_field_initializer() {
      QLJS_ASSERT(p->peek().type == token_type::equal);
      const modifier *bang = this->find_modifier(token_type::bang);
      const modifier *abstract = this->find_modifier(token_type::kw_abstract);
      if (is_interface && !bang) {
        // Don't report if we found a bang. We already reported
        // diag_typescript_assignment_asserted_fields_not_allowed_in_interfaces.
        p->diag_reporter_->report(
            diag_interface_fields_cannot_have_initializers{
                .equal = p->peek().span(),
            });
      }
      if (p->options_.typescript && !is_interface && bang) {
        p->diag_reporter_->report(
            diag_typescript_assignment_asserted_field_cannot_have_initializer{
                .equal = p->peek().span(),
                .bang = bang->span,
            });
      }
      if (abstract) {
        p->diag_reporter_->report(diag_abstract_field_cannot_have_initializer{
            .equal = p->peek().span(),
            .abstract_keyword = abstract->span,
        });
      }
      p->skip();
      p->parse_and_visit_expression(v);
    }

    function_attributes function_attributes_from_modifiers(
        const std::optional<identifier> &) const {
      bool has_async = this->find_modifier(token_type::kw_async);
      bool has_star = this->find_modifier(token_type::star);
      if (has_async && has_star) return function_attributes::async_generator;
      if (has_async && !has_star) return function_attributes::async;
      if (!has_async && has_star) return function_attributes::generator;
      if (!has_async && !has_star) return function_attributes::normal;
      QLJS_UNREACHABLE();
    }

    void check_modifiers_for_field_without_type_annotation() {
      this->check_modifiers_for_field();

      if (!this->is_interface && p->options_.typescript) {
        if (const modifier *bang = this->find_modifier(token_type::bang)) {
          p->diag_reporter_->report(
              diag_typescript_assignment_asserted_field_must_have_a_type{
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
      error_if_readonly_in_not_typescript();
      error_if_static_in_interface();
      error_if_optional_in_not_typescript();
      error_if_abstract_not_in_abstract_class();
    }

    void check_modifiers_for_method() {
      error_if_readonly_method();
      error_if_async_or_generator_without_method_body();
      error_if_invalid_access_specifier();
      error_if_static_in_interface();
      error_if_optional_in_not_typescript();
      error_if_abstract_not_in_abstract_class();
    }

    void error_if_optional_in_not_typescript() {
      if (!p->options_.typescript && !is_interface) {
        if (const modifier *optional_modifier =
                find_modifier(token_type::question)) {
          p->diag_reporter_->report(
              diag_typescript_optional_properties_not_allowed_in_javascript{
                  .question = optional_modifier->span,
              });
        }
      }
    }

    void error_if_invalid_static_block(const modifier &static_modifier) {
      if (is_interface) {
        p->diag_reporter_->report(
            diag_typescript_interfaces_cannot_contain_static_blocks{
                .static_token = static_modifier.span,
            });
      }
    }

    void error_if_invalid_assignment_assertion() {
      if (const modifier *assignment_assertion_modifier =
              find_modifier(token_type::bang)) {
        if (!p->options_.typescript) {
          p->diag_reporter_->report(
              diag_typescript_assignment_asserted_fields_not_allowed_in_javascript{
                  .bang = assignment_assertion_modifier->span,
              });
        } else if (is_interface) {
          p->diag_reporter_->report(
              diag_typescript_assignment_asserted_fields_not_allowed_in_interfaces{
                  .bang = assignment_assertion_modifier->span,
              });
        }
      }
    }

    void error_if_function_modifier() {
      if (const modifier *function_modifier =
              find_modifier(token_type::kw_function)) {
        p->diag_reporter_->report(diag_methods_should_not_use_function_keyword{
            .function_token = function_modifier->span,
        });
      }
    }

    void error_if_async_static() {
      // FIXME(#747): This only checks the first 'readonly' modifier
      // against the first 'static' modifier.
      const modifier *async_modifier = find_modifier(token_type::kw_async);
      const modifier *static_modifier = find_modifier(token_type::kw_static);
      if (async_modifier && static_modifier &&
          async_modifier < static_modifier) {
        if (!is_interface) {
          // FIXME(#747): This produces bad spans if there are tokens between
          // 'async' and 'static'.
          p->diag_reporter_->report(diag_async_static_method{
              .async_static = source_code_span(async_modifier->span.begin(),
                                               static_modifier->span.end()),
          });
        }
      }
    }

    void error_if_readonly_static() {
      // FIXME(#747): This only checks the first 'readonly' modifier against
      // the first 'static' modifier.
      const modifier *readonly_modifier =
          find_modifier(token_type::kw_readonly);
      const modifier *static_modifier = find_modifier(token_type::kw_static);
      if (readonly_modifier && static_modifier &&
          readonly_modifier < static_modifier) {
        if (!is_interface) {
          // FIXME(#747): This produces bad spans if there are tokens between
          // 'readonly' and 'static'.
          p->diag_reporter_->report(diag_readonly_static_field{
              .readonly_static = source_code_span(
                  readonly_modifier->span.begin(), static_modifier->span.end()),
          });
        }
      }
    }

    void error_if_readonly_method() {
      if (const modifier *readonly_modifier =
              find_modifier(token_type::kw_readonly)) {
        p->diag_reporter_->report(diag_typescript_readonly_method{
            .readonly_keyword = readonly_modifier->span,
        });
      }
    }

    void error_if_readonly_in_not_typescript() {
      if (!p->options_.typescript) {
        if (const modifier *readonly_modifier =
                find_modifier(token_type::kw_readonly)) {
          p->diag_reporter_->report(
              diag_typescript_readonly_fields_not_allowed_in_javascript{
                  .readonly_keyword = readonly_modifier->span,
              });
        }
      }
    }

    void error_if_invalid_access_specifier() {
      if (const modifier *access_specifier = find_access_specifier()) {
        if (is_interface) {
          switch (access_specifier->type) {
          case token_type::kw_private:
            p->diag_reporter_->report(
                diag_interface_properties_cannot_be_private{
                    .property_name_or_private_keyword = access_specifier->span,
                });
            break;

          case token_type::kw_protected:
            p->diag_reporter_->report(
                diag_interface_properties_cannot_be_protected{
                    .protected_keyword = access_specifier->span,
                });
            break;

          case token_type::kw_public:
            p->diag_reporter_->report(
                diag_interface_properties_cannot_be_explicitly_public{
                    .public_keyword = access_specifier->span,
                });
            break;

          default:
            QLJS_UNREACHABLE();
            break;
          }
        } else if (!p->options_.typescript) {
          switch (access_specifier->type) {
          case token_type::kw_private:
            p->diag_reporter_->report(
                diag_typescript_private_not_allowed_in_javascript{
                    .specifier = access_specifier->span,
                });
            break;

          case token_type::kw_protected:
            p->diag_reporter_->report(
                diag_typescript_protected_not_allowed_in_javascript{
                    .specifier = access_specifier->span,
                });
            break;

          case token_type::kw_public:
            p->diag_reporter_->report(
                diag_typescript_public_not_allowed_in_javascript{
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

    void error_if_static_in_interface() {
      if (is_interface) {
        if (const modifier *static_modifier =
                find_modifier(token_type::kw_static)) {
          p->diag_reporter_->report(diag_interface_properties_cannot_be_static{
              .static_keyword = static_modifier->span,
          });
        }
      }
    }

    void error_if_abstract_not_in_abstract_class() {
      if (const modifier *abstract_modifier =
              find_modifier(token_type::kw_abstract)) {
        if (is_interface) {
          p->diag_reporter_->report(
              diag_abstract_property_not_allowed_in_interface{
                  .abstract_keyword = abstract_modifier->span,
              });
        } else if (!is_abstract) {
          p->diag_reporter_->report(
              diag_abstract_property_not_allowed_in_non_abstract_class{
                  .abstract_keyword = abstract_modifier->span,
                  .class_keyword = class_or_interface_keyword_span,
              });
        }
      }
    }

    void error_if_async_or_generator_without_method_body() {
      if (is_interface) {
        if (const modifier *async_modifier =
                find_modifier(token_type::kw_async)) {
          p->diag_reporter_->report(diag_interface_methods_cannot_be_async{
              .async_keyword = async_modifier->span,
          });
        }
        if (const modifier *star_modifier = find_modifier(token_type::star)) {
          p->diag_reporter_->report(diag_interface_methods_cannot_be_generators{
              .star = star_modifier->span,
          });
        }
      }

      const modifier *abstract_modifier =
          find_modifier(token_type::kw_abstract);
      if (abstract_modifier) {
        if (const modifier *async_modifier =
                find_modifier(token_type::kw_async)) {
          p->diag_reporter_->report(diag_abstract_methods_cannot_be_async{
              .async_keyword = async_modifier->span,
              .abstract_keyword = abstract_modifier->span,
          });
        }
        if (const modifier *star_modifier = find_modifier(token_type::star)) {
          p->diag_reporter_->report(diag_abstract_methods_cannot_be_generators{
              .star = star_modifier->span,
              .abstract_keyword = abstract_modifier->span,
          });
        }
      }
    }

    const modifier *find_modifier(token_type modifier_type) const {
      for (const modifier &m : modifiers) {
        if (m.type == modifier_type) {
          return &m;
        }
      }
      return nullptr;
    }

    const modifier *find_access_specifier() const {
      for (const modifier &m : modifiers) {
        if (m.is_access_specifier()) {
          return &m;
        }
      }
      return nullptr;
    }
  };
  class_parser state(this, v, class_or_interface_keyword_span, is_interface,
                     is_abstract);
  state.parse_stuff();
}

void parser::parse_and_visit_typescript_interface(
    parse_visitor_base &v, source_code_span interface_keyword_span) {
  typescript_only_construct_guard ts_guard =
      this->enter_typescript_only_construct();

  if (!this->options_.typescript) {
    this->diag_reporter_->report(
        diag_typescript_interfaces_not_allowed_in_javascript{
            .interface_keyword = interface_keyword_span,
        });
  }

  switch (this->peek().type) {
  case token_type::kw_await:
    if (this->in_async_function_) {
      this->diag_reporter_->report(diag_cannot_declare_await_in_async_function{
          .name = this->peek().identifier_name(),
      });
    }
    goto interface_name;

  case token_type::identifier:
  case token_type::kw_abstract:
  case token_type::kw_as:
  case token_type::kw_assert:
  case token_type::kw_asserts:
  case token_type::kw_async:
  case token_type::kw_constructor:
  case token_type::kw_declare:
  case token_type::kw_from:
  case token_type::kw_get:
  case token_type::kw_global:
  case token_type::kw_infer:
  case token_type::kw_intrinsic:
  case token_type::kw_is:
  case token_type::kw_keyof:
  case token_type::kw_module:
  case token_type::kw_namespace:
  case token_type::kw_of:
  case token_type::kw_out:
  case token_type::kw_override:
  case token_type::kw_readonly:
  case token_type::kw_require:
  case token_type::kw_set:
  case token_type::kw_type:
  case token_type::kw_unique:
  interface_name:
    v.visit_variable_declaration(this->peek().identifier_name(),
                                 variable_kind::_interface,
                                 variable_init_kind::normal);
    this->skip();
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }

  v.visit_enter_interface_scope();
  if (this->peek().type == token_type::less) {
    this->parse_and_visit_typescript_generic_parameters(v);
  }
  if (this->peek().type == token_type::kw_extends) {
    this->parse_and_visit_typescript_interface_extends(v);
  }
  if (this->peek().type == token_type::left_curly) {
    this->parse_and_visit_typescript_interface_body(
        v,
        /*interface_keyword_span=*/interface_keyword_span);
  } else {
    this->diag_reporter_->report(diag_missing_body_for_typescript_interface{
        .interface_keyword_and_name_and_heritage =
            source_code_span(interface_keyword_span.begin(),
                             this->lexer_.end_of_previous_token()),
    });
  }
  v.visit_exit_interface_scope();
}

void parser::parse_and_visit_typescript_interface_extends(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_extends);

  this->skip();
next_extends:
  this->parse_and_visit_typescript_interface_reference(v);
  if (this->peek().type == token_type::comma) {
    // extends IBanana, IOrange
    this->skip();
    goto next_extends;
  }
}

void parser::parse_and_visit_typescript_interface_reference(
    parse_visitor_base &v) {
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
  identifier ident = this->peek().identifier_name();
  this->skip();

  if (this->peek().type == token_type::dot) {
    // extends mynamespace.MyInterface
    this->skip();
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
    this->skip();
    v.visit_variable_namespace_use(ident);
  } else {
    // extends MyInterface
    v.visit_variable_type_use(ident);
  }
}

void parser::parse_and_visit_typescript_interface_body(
    parse_visitor_base &v, source_code_span interface_keyword_span) {
  QLJS_ASSERT(this->peek().type == token_type::left_curly);
  source_code_span left_curly_span = this->peek().span();
  this->skip();

  while (this->peek().type != token_type::right_curly) {
    this->parse_and_visit_class_or_interface_member(
        v,
        /*class_or_interface_keyword_span=*/interface_keyword_span,
        /*is_interface=*/true,
        /*is_abstract=*/false);
    if (this->peek().type == token_type::end_of_file) {
      this->diag_reporter_->report(diag_unclosed_interface_block{
          .block_open = left_curly_span,
      });
      return;
    }
  }

  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
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
