// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

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
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/warning.h>
#include <utility>

namespace quick_lint_js {
void parser::parse_and_visit_class(parse_visitor_base &v,
                                   parser::name_requirement require_name) {
  QLJS_ASSERT(this->peek().type == token_type::kw_class);

  this->parse_and_visit_class_heading(v, /*require_name=*/require_name);

  switch (this->peek().type) {
  case token_type::left_curly:
    v.visit_enter_class_scope();
    this->parse_and_visit_class_body(v);
    v.visit_exit_class_scope();
    break;

  default: {
    const char8 *here = this->lexer_.end_of_previous_token();
    this->diag_reporter_->report(diag_missing_body_for_class{
        .class_keyword_and_name_and_heritage = source_code_span(here, here),
    });
    break;
  }
  }
}

void parser::parse_and_visit_class_heading(
    parse_visitor_base &v, parser::name_requirement require_name) {
  QLJS_ASSERT(this->peek().type == token_type::kw_class);
  source_code_span class_keyword_span = this->peek().span();
  this->skip();

  std::optional<identifier> optional_class_name;
  switch (this->peek().type) {
  case token_type::kw_await:
    if (this->in_async_function_) {
      this->diag_reporter_->report(
          diag_cannot_declare_class_named_await_in_async_function{
              .name = this->peek().identifier_name().span()});
    }
    goto class_name;

  QLJS_CASE_STRICT_ONLY_RESERVED_KEYWORD:
    // TODO(#73): Disallow 'protected', 'implements', etc. in strict mode.
    [[fallthrough]];
  QLJS_CASE_CONTEXTUAL_KEYWORD:
  case token_type::identifier:
  case token_type::kw_yield:
    // TODO(#707): Disallow classes named 'yield' in generator function.
  class_name:
    if (this->peek().type == token_type::kw_let) {
      this->diag_reporter_->report(diag_cannot_declare_class_named_let{
          .name = this->peek().identifier_name().span()});
    }
    optional_class_name = this->peek().identifier_name();
    this->skip();
    break;

    // class { ... }
  case token_type::left_curly:
    break;

    // class extends C { }
  case token_type::kw_extends:
    break;

    // { class }  // Invalid.
    // class;     // Invalid.
  default:
    // We'll report errors later.
    break;
  }

  switch (this->peek().type) {
  case token_type::kw_extends:
    this->skip();
    // TODO(strager): Error when extending things like '0' or 'true'.
    this->parse_and_visit_expression(v,
                                     precedence{
                                         .commas = false,
                                         .trailing_curly_is_arrow_body = false,
                                     });
    break;

  case token_type::left_curly:
    break;

    // class C;     // Invalid.
    // { class C }  // Invalid.
  default:
    // parse_and_visit_class or parse_class_expression will report an error.
    break;
  }

  if (optional_class_name.has_value()) {
    v.visit_variable_declaration(*optional_class_name, variable_kind::_class,
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

void parser::parse_and_visit_class_body(parse_visitor_base &v) {
  class_guard g(this, std::exchange(this->in_class_, true));

  source_code_span left_curly_span = this->peek().span();
  this->skip();

  while (this->peek().type != token_type::right_curly) {
    this->parse_and_visit_class_or_interface_member(v, /*is_interface=*/false);
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

void parser::parse_and_visit_class_or_interface_member(parse_visitor_base &v,
                                                       bool is_interface) {
  struct class_parser {
    explicit class_parser(parser *p, parse_visitor_base &v, bool is_interface)
        : p(p), v(v), is_interface(is_interface) {}

    parser *p;
    parse_visitor_base &v;
    bool is_interface;

    std::optional<identifier> last_ident;

    // *, async, function, get, private, protected, public, readonly, set,
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
              check_modifiers_for_field();
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
              .property_name = p->peek().identifier_name(),
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

        if (p->options_.typescript) {
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
              p->diag_reporter_->report(
                  diag_typescript_call_signatures_not_allowed_in_classes{
                      .expected_method_name = property_name_span,
                  });
            }
          }
          v.visit_exit_function_scope();
        }
        break;

      case token_type::left_curly:
        p->diag_reporter_->report(diag_unexpected_token{
            .token = p->peek().span(),
        });
        p->skip();
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
      parser_transaction transaction = p->begin_transaction();
      // TODO(strager): Allow certain contextual keywords like 'let'?
      if (p->peek().type == token_type::identifier) {
        identifier key_variable = p->peek().identifier_name();
        p->skip();
        if (p->peek().type == token_type::colon) {
          // [key: KeyType]: ValueType;
          v.visit_enter_index_signature_scope();
          p->commit_transaction(std::move(transaction));
          p->parse_and_visit_typescript_colon_type_expression(v);

          QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN_WITH_PARSER(
              p, token_type::right_square);
          const char8 *name_end = p->peek().end;
          p->skip();

          // TODO(strager): We probably should create a new kind of variable
          // instead of overloading 'parameter'.
          v.visit_variable_declaration(key_variable, variable_kind::_parameter,
                                       variable_init_kind::normal);

          switch (p->peek().type) {
          // [key: KeyType]: ValueType;
          case token_type::colon:
            p->parse_and_visit_typescript_colon_type_expression(v);
            p->consume_semicolon<
                diag_missing_semicolon_after_index_signature>();
            break;

          // [key: KeyType];  // Invalid.
          case token_type::semicolon: {
          missing_type_for_index_signature:
            const char8 *expected_type = p->lexer_.end_of_previous_token();
            p->diag_reporter_->report(
                diag_typescript_index_signature_needs_type{
                    .expected_type =
                        source_code_span(expected_type, expected_type),
                });
            break;
          }

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
      p->roll_back_transaction(std::move(transaction));
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
      if (const modifier *function_modifier =
              find_modifier(token_type::kw_function)) {
        p->diag_reporter_->report(diag_methods_should_not_use_function_keyword{
            .function_token = function_modifier->span,
        });
      }

      const modifier *static_modifier = find_modifier(token_type::kw_static);

      // FIXME(strager): This only checks the first 'readonly' modifier
      // against the first 'static' modifier.
      const modifier *async_modifier = find_modifier(token_type::kw_async);
      if (async_modifier && static_modifier &&
          async_modifier < static_modifier) {
        if (!is_interface) {
          p->diag_reporter_->report(diag_async_static_method{
              .async_static = source_code_span(async_modifier->span.begin(),
                                               static_modifier->span.end()),
          });
        }
      }

      // FIXME(strager): This only checks the first 'readonly' modifier against
      // the first 'static' modifier.
      const modifier *readonly_modifier =
          find_modifier(token_type::kw_readonly);
      if (readonly_modifier && static_modifier &&
          readonly_modifier < static_modifier) {
        if (!is_interface) {
          // FIXME(strager): This produces bad spans if there are tokens between
          // 'readonly' and 'static'.
          p->diag_reporter_->report(diag_readonly_static_field{
              .readonly_static = source_code_span(
                  readonly_modifier->span.begin(), static_modifier->span.end()),
          });
        }
      }

      std::optional<source_code_span> optional_span;
      if (p->peek().type == token_type::question) {
        // field?: Type;
        optional_span = p->peek().span();
        if (!p->options_.typescript) {
          p->diag_reporter_->report(
              diag_typescript_optional_properties_not_allowed_in_javascript{
                  .question = *optional_span,
              });
        }
        p->skip();
      }

      std::optional<source_code_span> assignment_assertion_span;
      if (p->peek().type == token_type::bang) {
        // field!: Type;
        assignment_assertion_span = p->peek().span();
        if (!p->options_.typescript) {
          p->diag_reporter_->report(
              diag_typescript_assignment_asserted_fields_not_allowed_in_javascript{
                  .bang = *assignment_assertion_span,
              });
        } else if (is_interface) {
          p->diag_reporter_->report(
              diag_typescript_assignment_asserted_fields_not_allowed_in_interfaces{
                  .bang = *assignment_assertion_span,
              });
        }
        p->skip();
      }

      switch (p->peek().type) {
        // method() { }
        // method<T>() { }  // TypeScript only.
        // method { }       // Invalid (missing parameter list).
      case token_type::left_curly:
      case token_type::left_paren:
      case token_type::less: {
        v.visit_property_declaration(property_name);
        if (optional_span.has_value() && !is_interface &&
            p->options_.typescript) {
          // method?() {}  // Invalid.
          p->diag_reporter_->report(
              diag_typescript_optional_properties_not_allowed_on_methods{
                  .question = *optional_span,
              });
        }
        if (assignment_assertion_span.has_value() && !is_interface &&
            p->options_.typescript) {
          // method!() {}  // Invalid.
          p->diag_reporter_->report(
              diag_typescript_assignment_asserted_fields_not_allowed_on_methods{
                  .bang = *assignment_assertion_span,
              });
        }
        check_modifiers_for_method();
        function_attributes attributes =
            function_attributes_from_modifiers(property_name);
        if (is_interface) {
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
        check_modifiers_for_field();
        v.visit_property_declaration(property_name);
        p->consume_semicolon<diag_missing_semicolon_after_field>();
        break;

        // field = initialValue;
      case token_type::equal:
        check_modifiers_for_field();
        if (is_interface) {
          p->diag_reporter_->report(
              diag_interface_fields_cannot_have_initializers{
                  .equal = p->peek().span(),
              });
        }
        p->skip();
        p->parse_and_visit_expression(v);
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
          check_modifiers_for_field();
          v.visit_property_declaration(property_name);
        } else {
          if (u8"const" == property_name_span.string_view()) {
            // class C {
            //   const field
            // }
            p->diag_reporter_->report(diag_typescript_style_const_field{
                .const_token = property_name_span,
            });
          } else if (optional_span.has_value()) {
            // class C {
            //   field? method() {}  // Invalid.
            // }
            check_modifiers_for_field();
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
          check_modifiers_for_field();
          v.visit_property_declaration(property_name);
        } else {
          QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        }
        break;

      case token_type::colon:
        p->parse_and_visit_typescript_colon_type_expression(v);
        v.visit_property_declaration(property_name);
        p->consume_semicolon<diag_missing_semicolon_after_field>();
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        break;
      }
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

    void check_modifiers_for_field() {
      error_if_invalid_access_specifier();
      error_if_readonly_in_not_typescript();
      error_if_static_in_interface();
    }

    void check_modifiers_for_method() {
      error_if_readonly_method();
      error_if_async_in_interface();
      error_if_generator_star_in_interface();
      error_if_invalid_access_specifier();
      error_if_static_in_interface();
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
          p->diag_reporter_->report(
              diag_typescript_interfaces_cannot_contain_access_specifiers{
                  .specifier = access_specifier->span,
              });
        } else if (!p->options_.typescript) {
          p->diag_reporter_->report(
              diag_typescript_access_specifiers_not_allowed_in_javascript{
                  .specifier = access_specifier->span,
              });
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

    void error_if_async_in_interface() {
      if (is_interface) {
        if (const modifier *async_modifier =
                find_modifier(token_type::kw_async)) {
          p->diag_reporter_->report(diag_interface_methods_cannot_be_async{
              .async_keyword = async_modifier->span,
          });
        }
      }
    }

    void error_if_generator_star_in_interface() {
      if (is_interface) {
        if (const modifier *star_modifier = find_modifier(token_type::star)) {
          p->diag_reporter_->report(diag_interface_methods_cannot_be_generators{
              .star = star_modifier->span,
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
  class_parser state(this, v, is_interface);
  state.parse_stuff();
}

void parser::parse_and_visit_typescript_interface(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_interface);
  this->skip();

  switch (this->peek().type) {
  case token_type::kw_await:
    if (this->in_async_function_) {
      this->diag_reporter_->report(
          diag_cannot_declare_interface_named_await_in_async_function{
              .name = this->peek().identifier_name().span()});
    }
    goto interface_name;

  // TODO(strager): Allow contextual keywords.
  case token_type::identifier:
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
  this->parse_and_visit_typescript_interface_body(v);
  v.visit_exit_interface_scope();
}

void parser::parse_and_visit_typescript_interface_extends(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::kw_extends);

  this->skip();
next_extends:
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

  if (this->peek().type == token_type::comma) {
    // extends IBanana, IOrange
    this->skip();
    goto next_extends;
  }
}

void parser::parse_and_visit_typescript_interface_body(parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::left_curly);
  source_code_span left_curly_span = this->peek().span();
  this->skip();

  while (this->peek().type != token_type::right_curly) {
    this->parse_and_visit_class_or_interface_member(v, /*is_interface=*/true);
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
