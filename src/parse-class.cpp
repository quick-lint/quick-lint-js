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
    function_attributes method_attributes = function_attributes::normal;
    bool async_static = false;
    bool readonly_static = false;
    std::optional<source_code_span> readonly_keyword;
    std::optional<source_code_span> static_keyword;
    std::optional<source_code_span> star_token;
    std::optional<source_code_span> async_keyword;

    void parse_leading_static() {
      switch (p->peek().type) {
      // static f() {}
      case token_type::kw_static:
        last_ident = p->peek().identifier_name();
        static_keyword = p->peek().span();
        p->skip();
        break;

      default:
        break;
      }
    }

    void parse_leading_readonly() {
      switch (p->peek().type) {
      // readonly field: number;
      case token_type::kw_readonly:
        last_ident = p->peek().identifier_name();
        readonly_keyword = p->peek().span();
        p->skip();

        if (p->peek().type == token_type::kw_static) {
          // readonly static field;  // Invalid
          // readonly static;
          readonly_static = true;
          // TODO(#736): What about 'static readonly static'?
          static_keyword = p->peek().span();
        }
        break;

      default:
        break;
      }
    }

    void parse_stuff() {
    next:
      switch (p->peek().type) {
      // async f() {}
      case token_type::kw_async:
        last_ident = p->peek().identifier_name();
        async_keyword = p->peek().span();
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
            error_if_readonly_in_not_typescript(last_ident);
            error_if_static_in_interface(last_ident);
            v.visit_property_declaration(last_ident);
            return;
          default:
            break;
          }
        } else {
          if (p->peek().type != token_type::left_paren) {
            method_attributes = function_attributes::async;
          }
          if (p->peek().type == token_type::star) {
            // async *g() {}
            method_attributes = function_attributes::async_generator;
            star_token = p->peek().span();
            p->skip();
          }
          if (p->peek().type == token_type::kw_static) {
            // async static method() {}  // Invalid
            // async static() {}
            async_static = true;
            // TODO(strager): What about 'static async static'?
            static_keyword = p->peek().span();
          }
        }
        break;

      // *g() {}
      case token_type::star:
        method_attributes = function_attributes::generator;
        star_token = p->peek().span();
        p->skip();
        break;

      // get prop() {}
      case token_type::kw_get:
      case token_type::kw_set:
        last_ident = p->peek().identifier_name();
        p->skip();
        break;

      default:
        break;
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
      QLJS_CASE_CONTEXTUAL_KEYWORD:
      case token_type::identifier:
      case token_type::reserved_keyword_with_escape_sequence: {
        identifier property_name = p->peek().identifier_name();
        p->skip();
        parse_and_visit_field_or_method(property_name, method_attributes);
        break;
      }

      // "method"() {}
      // 9001() {}
      // "fieldName" = init;
      case token_type::number:
      case token_type::string: {
        source_code_span name_span = p->peek().span();
        p->skip();
        parse_and_visit_field_or_method_without_name(name_span,
                                                     method_attributes);
        break;
      }

      // [methodNameExpression]() {}
      // [fieldNameExpression] = initialValue;
      // [key: KeyType]: ValueType;  // TypeScript only
      case token_type::left_square: {
        const char8 *name_begin = p->peek().begin;
        p->skip();

        if (p->options_.typescript) {
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
              v.visit_variable_declaration(key_variable,
                                           variable_kind::_parameter,
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
                      source_code_span(name_begin, name_end),
                      method_attributes);
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
              break;
            }
          }
          p->roll_back_transaction(std::move(transaction));
        }

        p->parse_and_visit_expression(v);

        QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN_WITH_PARSER(
            p, token_type::right_square);
        const char8 *name_end = p->peek().end;
        p->skip();

        parse_and_visit_field_or_method_without_name(
            source_code_span(name_begin, name_end), method_attributes);
        break;
      }

      // function() {}
      // function f() {}  // Invalid.
      case token_type::kw_function: {
        token function_token = p->peek();
        p->skip();
        switch (p->peek().type) {
        // function() {}
        // function?() {}
        // class C { function }   // Field named 'function'.
        // class C { function; }  // Field named 'function'.
        // function = init;       // Field named 'function'.
        case token_type::equal:
        case token_type::left_paren:
        case token_type::question:
        case token_type::right_curly:
        case token_type::semicolon:
          parse_and_visit_field_or_method(function_token.identifier_name(),
                                          method_attributes);
          break;

        default:
          // function f() {}  // Invalid.
          p->diag_reporter_->report(
              diag_methods_should_not_use_function_keyword{
                  .function_token = function_token.span(),
              });
          goto next;
        }
        break;
      }

      // async;  // Field named 'async'.
      // ;       // Stray semicolon.
      case token_type::semicolon:
        if (last_ident.has_value()) {
          parse_and_visit_field_or_method(*last_ident, method_attributes);
        } else {
          p->skip();
        }
        break;

      // async() {}
      // get() {}
      // () {}       // Invalid.
      case token_type::left_paren:
        if (last_ident.has_value()) {
          parse_and_visit_field_or_method(*last_ident, method_attributes);
        } else {
          source_code_span expected_name(p->peek().begin, p->peek().begin);
          if (!is_interface) {
            p->diag_reporter_->report(diag_missing_class_method_name{
                .expected_name = expected_name,
            });
          }
          parse_and_visit_field_or_method_without_name(expected_name,
                                                       method_attributes);
        }
        break;

      // async?() {}
      // class C { get }  // Field named 'get'
      // get = init;
      case token_type::equal:
      case token_type::question:
      case token_type::right_curly:
        if (last_ident.has_value()) {
          parse_and_visit_field_or_method(*last_ident, method_attributes);
        } else {
          QLJS_PARSER_UNIMPLEMENTED_WITH_PARSER(p);
        }
        break;

      // <T>(param: T): void;  // TypeScript generic interface call signature.
      case token_type::less: {
        source_code_span property_name_span(p->peek().begin, p->peek().begin);

        v.visit_property_declaration(std::nullopt);
        v.visit_enter_function_scope();
        {
          function_guard guard = p->enter_function(method_attributes);
          p->parse_and_visit_typescript_generic_parameters(v);
          p->parse_and_visit_interface_function_parameters_and_body_no_scope(
              v, property_name_span);
        }
        v.visit_exit_function_scope();
        break;
      }

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

    void parse_and_visit_field_or_method(
        identifier property_name, function_attributes method_attributes) {
      parse_and_visit_field_or_method_impl(property_name, property_name.span(),
                                           method_attributes);
    }

    void parse_and_visit_field_or_method_without_name(
        source_code_span name_span, function_attributes method_attributes) {
      parse_and_visit_field_or_method_impl(std::nullopt, name_span,
                                           method_attributes);
    }

    void parse_and_visit_field_or_method_impl(
        std::optional<identifier> property_name,
        source_code_span property_name_span,
        function_attributes method_attributes) {
      if (async_static) {
        if (p->peek().type == token_type::star) {
          // async static *m() {}  // Invalid.
          method_attributes = function_attributes::async_generator;
          star_token = p->peek().span();
          p->skip();
        }
        switch (p->peek().type) {
        case token_type::private_identifier:
          if (is_interface) {
            p->diag_reporter_->report(
                diag_interface_properties_cannot_be_private{
                    .property_name = p->peek().identifier_name(),
                });
          }
          [[fallthrough]];
        QLJS_CASE_RESERVED_KEYWORD_EXCEPT_FUNCTION:
        QLJS_CASE_CONTEXTUAL_KEYWORD:
        case token_type::identifier:
        case token_type::reserved_keyword_with_escape_sequence: {
          // async static method() {}             // Invalid
          // async static *myAsyncGenerator() {}  // Invalid
          identifier new_property_name = p->peek().identifier_name();
          if (is_interface) {
            // diag_interface_methods_cannot_be_async and
            // diag_interface_properties_cannot_be_static (and maybe
            // diag_interface_methods_cannot_be_generators) are reported later.
            QLJS_ASSERT(async_keyword.has_value());
            QLJS_ASSERT(static_keyword.has_value());
          } else {
            p->diag_reporter_->report(diag_async_static_method{
                .async_static = source_code_span(async_keyword->begin(),
                                                 property_name_span.end()),
            });
          }
          property_name = new_property_name;
          property_name_span = property_name->span();
          p->skip();
          break;
        }
          // async static() {}
        default:
          break;
        }
      }

      if (readonly_static) {
        // TODO(strager): Do we need to handle '*' (generator)?
        switch (p->peek().type) {
        case token_type::private_identifier:
          if (is_interface) {
            p->diag_reporter_->report(
                diag_interface_properties_cannot_be_private{
                    .property_name = p->peek().identifier_name(),
                });
          }
          [[fallthrough]];
        QLJS_CASE_CONTEXTUAL_KEYWORD:
        QLJS_CASE_RESERVED_KEYWORD_EXCEPT_FUNCTION:
        case token_type::identifier:
        case token_type::reserved_keyword_with_escape_sequence: {
          // readonly static field = init;  // Invalid
          identifier new_property_name = p->peek().identifier_name();
          if (is_interface) {
            // diag_interface_properties_cannot_be_static is reported later.
            QLJS_ASSERT(static_keyword.has_value());
          } else {
            p->diag_reporter_->report(diag_readonly_static_field{
                .readonly_static = source_code_span(readonly_keyword->begin(),
                                                    property_name_span.end()),
            });
          }
          property_name = new_property_name;
          property_name_span = property_name->span();
          p->skip();
          break;
        }
        // readonly static;
        default:
          break;
        }
      }

      bool is_optional = p->peek().type == token_type::question;
      if (is_optional) {
        if (!p->options_.typescript) {
          p->diag_reporter_->report(
              diag_typescript_optional_properties_not_allowed_in_javascript{
                  .question = p->peek().span(),
              });
        }
        p->skip();
      }

      switch (p->peek().type) {
        // method() { }
        // method { }    // Invalid (missing parameter list).
      case token_type::left_curly:
      case token_type::left_paren:
        v.visit_property_declaration(property_name);
        if (is_keyword(readonly_keyword, property_name)) {
          // readonly method() {}  // Invalid.
          p->diag_reporter_->report(diag_typescript_readonly_method{
              .readonly_keyword = *readonly_keyword,
          });
        }
        if (is_interface) {
          error_if_async_in_interface(property_name);
          error_if_generator_star_in_interface(property_name);
          error_if_static_in_interface(property_name);
          v.visit_enter_function_scope();
          function_guard guard = p->enter_function(method_attributes);
          p->parse_and_visit_interface_function_parameters_and_body_no_scope(
              v, property_name_span);
          v.visit_exit_function_scope();
        } else {
          p->parse_and_visit_function_parameters_and_body(
              v, /*name=*/property_name_span, method_attributes);
        }
        break;

        // field;
        // class C { field }
        // class C { field  // Invalid.
      case token_type::end_of_file:
      case token_type::right_curly:
      case token_type::semicolon:
        error_if_readonly_in_not_typescript(property_name);
        error_if_static_in_interface(property_name);
        v.visit_property_declaration(property_name);
        p->consume_semicolon<diag_missing_semicolon_after_field>();
        break;

        // field = initialValue;
      case token_type::equal:
        error_if_readonly_in_not_typescript(property_name);
        if (is_interface) {
          error_if_static_in_interface(property_name);
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
          error_if_static_in_interface(property_name);
          error_if_readonly_in_not_typescript(property_name);
          v.visit_property_declaration(property_name);
        } else {
          if (u8"const" == property_name_span.string_view()) {
            // class C {
            //   const field
            // }
            p->diag_reporter_->report(diag_typescript_style_const_field{
                .const_token = property_name_span,
            });
          } else if (is_optional) {
            // class C {
            //   field? method() {}  // Invalid.
            // }
            error_if_readonly_in_not_typescript(property_name);
            error_if_static_in_interface(property_name);
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
          error_if_readonly_in_not_typescript(property_name);
          error_if_static_in_interface(property_name);
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

    void error_if_readonly_in_not_typescript(
        const std::optional<identifier> &property_name) {
      if (!p->options_.typescript &&
          is_keyword(readonly_keyword, property_name)) {
        p->diag_reporter_->report(
            diag_typescript_readonly_fields_not_allowed_in_javascript{
                .readonly_keyword = *readonly_keyword,
            });
      }
    }

    void error_if_static_in_interface(
        const std::optional<identifier> &property_name) {
      if (is_interface && is_keyword(static_keyword, property_name)) {
        p->diag_reporter_->report(diag_interface_properties_cannot_be_static{
            .static_keyword = *static_keyword,
        });
      }
    }

    void error_if_async_in_interface(
        const std::optional<identifier> &property_name) {
      if (is_interface && is_keyword(async_keyword, property_name)) {
        p->diag_reporter_->report(diag_interface_methods_cannot_be_async{
            .async_keyword = *async_keyword,
        });
      }
    }

    void error_if_generator_star_in_interface(
        const std::optional<identifier> &property_name) {
      if (is_interface && is_keyword(star_token, property_name)) {
        p->diag_reporter_->report(diag_interface_methods_cannot_be_generators{
            .star = *star_token,
        });
      }
    }

    static bool is_keyword(const std::optional<source_code_span> &keyword,
                           const std::optional<identifier> &property_name) {
      return keyword.has_value() &&
             (!property_name.has_value() ||
              property_name->span().begin() != keyword->begin());
    }
  };
  class_parser state(this, v, is_interface);
  state.parse_leading_static();
  state.parse_leading_readonly();
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

  if (this->peek().type == token_type::kw_extends) {
    this->parse_and_visit_typescript_interface_extends(v);
  }

  v.visit_enter_interface_scope();
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
