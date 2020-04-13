// quicklint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef QUICKLINT_JS_PARSE_H
#define QUICKLINT_JS_PARSE_H

#include <cstdlib>
#include <optional>
#include <quicklint-js/error.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/location.h>
#include <vector>

#define QLJS_PARSER_UNIMPLEMENTED()                                   \
  do {                                                                \
    this->crash_on_unimplemented_token(__FILE__, __LINE__, __func__); \
  } while (false)

namespace quicklint_js {
enum class variable_kind {
  _class,
  _const,
  _function,
  _import,
  _let,
  _parameter,
  _var,
};

struct expression_options {
  bool parse_commas;
};

class parser {
 public:
  explicit parser(const char *input, error_reporter *error_reporter)
      : lexer_(input, error_reporter),
        locator_(input),
        error_reporter_(error_reporter) {}

  quicklint_js::locator &locator() noexcept { return this->locator_; }

  template <class Visitor>
  void parse_module(Visitor &v) {
    while (this->peek().type != token_type::end_of_file) {
      this->parse_statement(v);
    }
    v.visit_end_of_module();
  }

  template <class Visitor>
  void parse_statement(Visitor &v) {
    switch (this->peek().type) {
      case token_type::_export:
        this->lexer_.skip();
        this->parse_declaration(v);
        break;

      case token_type::semicolon:
        this->lexer_.skip();
        break;

      case token_type::_async:
      case token_type::_function:
        this->parse_declaration(v);
        break;

      case token_type::_import:
        this->parse_import(v);
        break;

      case token_type::_const:
        this->parse_let_bindings(v, variable_kind::_const);
        break;

      case token_type::_let:
        this->parse_let_bindings(v, variable_kind::_let);
        break;

      case token_type::_var:
        this->parse_let_bindings(v, variable_kind::_var);
        break;

      case token_type::identifier:
        this->parse_expression(v, expression_options{.parse_commas = true});

        if (this->peek().type != token_type::semicolon) {
          QLJS_PARSER_UNIMPLEMENTED();
        }
        this->lexer_.skip();
        break;

      case token_type::_class:
        this->parse_class(v);
        break;

      case token_type::_return:
        this->lexer_.skip();
        this->parse_expression(v, expression_options{.parse_commas = true});
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
    }
  }

  template <class Visitor>
  void parse_expression(Visitor &v, expression_options options) {
    bool allow_binary_operator = false;
    bool allow_identifier = true;

    std::optional<source_code_span> last_operator;

    for (;;) {
      switch (this->peek().type) {
        case token_type::left_paren: {
          source_code_span left_paren_span = this->peek().span();
          this->lexer_.skip();
          this->parse_expression(v, expression_options{.parse_commas = true});
          if (this->peek().type == token_type::right_paren) {
            this->lexer_.skip();
          } else {
            this->error_reporter_->report_error_unmatched_parenthesis(
                left_paren_span);
          }
          last_operator = std::nullopt;
          allow_identifier = false;
          break;
        }

        case token_type::identifier:
          if (!allow_identifier) {
            this->error_reporter_->report_error_unexpected_identifier(
                this->peek().span());
          }
          v.visit_variable_use(this->peek().identifier_name());
          this->lexer_.skip();
          last_operator = std::nullopt;
          allow_binary_operator = true;
          allow_identifier = false;
          break;

        case token_type::_new:
          this->lexer_.skip();
          break;

        case token_type::number:
        case token_type::string:
          this->lexer_.skip();
          last_operator = std::nullopt;
          allow_binary_operator = true;
          break;

        case token_type::plus:
          last_operator = this->peek().span();
          this->lexer_.skip();
          allow_binary_operator = false;
          allow_identifier = true;
          break;

        case token_type::dot:
          this->lexer_.skip();
          this->lexer_.skip();
          break;

        case token_type::comma:
          if (options.parse_commas) {
            goto parse_binary_operator;
          } else {
            goto done;
          }

        case token_type::ampersand:
        case token_type::circumflex:
        case token_type::star:
        parse_binary_operator:
          if (!allow_binary_operator) {
            const source_code_span &bad_token_span = last_operator.has_value()
                                                         ? *last_operator
                                                         : this->peek().span();
            this->error_reporter_->report_error_missing_oprand_for_operator(
                bad_token_span);
          }
          last_operator = this->peek().span();
          this->lexer_.skip();
          allow_binary_operator = false;
          allow_identifier = true;
          break;

        case token_type::incomplete_template:
          this->parse_template(v);
          break;

        case token_type::right_paren:
        default:
        done:
          if (last_operator.has_value()) {
            this->error_reporter_->report_error_missing_oprand_for_operator(
                *last_operator);
          }
          return;
      }
    }
  }

 private:
  template <class Visitor>
  void parse_declaration(Visitor &v) {
    switch (this->peek().type) {
      case token_type::_async:
        this->lexer_.skip();
        switch (this->peek().type) {
          case token_type::_function:
            this->parse_function_declaration(v);
            break;

          default:
            QLJS_PARSER_UNIMPLEMENTED();
            break;
        }
        break;

      case token_type::_function:
        this->parse_function_declaration(v);
        break;

      case token_type::_class:
        this->parse_class(v);
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
    }
  }

  template <class Visitor>
  void parse_function_declaration(Visitor &v) {
    assert(this->peek().type == token_type::_function);
    this->lexer_.skip();

    if (this->peek().type != token_type::identifier) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    v.visit_variable_declaration(this->peek().identifier_name(),
                                 variable_kind::_function);
    this->lexer_.skip();

    this->parse_function_parameters_and_body(v);
  }

  template <class Visitor>
  void parse_function_parameters_and_body(Visitor &v) {
    if (this->peek().type != token_type::left_paren) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->lexer_.skip();
    v.visit_enter_function_scope();

    bool first_parameter = true;
    for (;;) {
      std::optional<source_code_span> comma_span = std::nullopt;
      if (!first_parameter) {
        if (this->peek().type != token_type::comma) {
          break;
        }
        comma_span = this->peek().span();
        this->lexer_.skip();
      }

      switch (this->peek().type) {
        case token_type::identifier:
        case token_type::left_curly:
          this->parse_binding_element(v, variable_kind::_parameter);
          break;
        case token_type::right_paren:
          goto done;
        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
      }
      first_parameter = false;
    }
  done:

    if (this->peek().type != token_type::right_paren) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->lexer_.skip();

    if (this->peek().type != token_type::left_curly) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->lexer_.skip();

    while (this->peek().type != token_type::right_curly) {
      this->parse_statement(v);
    }

    if (this->peek().type != token_type::right_curly) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->lexer_.skip();

    v.visit_exit_function_scope();
  }

  template <class Visitor>
  void parse_class(Visitor &v) {
    assert(this->peek().type == token_type::_class);
    this->lexer_.skip();

    identifier class_name = this->peek().identifier_name();
    this->lexer_.skip();

    switch (this->peek().type) {
      case token_type::_extends:
        this->lexer_.skip();
        switch (this->peek().type) {
          case token_type::identifier:
            // TODO(strager): Don't allow extending any ol' expression.
            this->parse_expression(v,
                                   expression_options{.parse_commas = false});
            break;
          default:
            QLJS_PARSER_UNIMPLEMENTED();
            break;
        }
        break;

      case token_type::left_curly:
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
    }

    v.visit_variable_declaration(class_name, variable_kind::_class);

    v.visit_enter_class_scope();

    switch (this->peek().type) {
      case token_type::left_curly:
        this->lexer_.skip();
        this->parse_class_body(v);
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
    }

    v.visit_exit_class_scope();
  }

  template <class Visitor>
  void parse_class_body(Visitor &v) {
    for (;;) {
      switch (this->peek().type) {
        case token_type::_static:
          this->lexer_.skip();
          switch (this->peek().type) {
            case token_type::identifier:
              v.visit_property_declaration(this->peek().identifier_name());
              this->lexer_.skip();
              this->parse_function_parameters_and_body(v);
              break;

            default:
              QLJS_PARSER_UNIMPLEMENTED();
              break;
          }
          break;

        case token_type::identifier:
          v.visit_property_declaration(this->peek().identifier_name());
          this->lexer_.skip();
          this->parse_function_parameters_and_body(v);
          break;

        case token_type::right_curly:
          return;

        default:
          QLJS_PARSER_UNIMPLEMENTED();
          break;
      }
    }
  }

  template <class Visitor>
  void parse_import(Visitor &v) {
    assert(this->peek().type == token_type::_import);
    this->lexer_.skip();

    switch (this->peek().type) {
      case token_type::identifier:
      case token_type::left_curly:
        this->parse_binding_element(v, variable_kind::_import);
        break;

      case token_type::star:
        this->lexer_.skip();

        if (this->peek().type != token_type::_as) {
          QLJS_PARSER_UNIMPLEMENTED();
        }
        this->lexer_.skip();

        v.visit_variable_declaration(this->peek().identifier_name(),
                                     variable_kind::_import);
        this->lexer_.skip();
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
    }

    if (this->peek().type != token_type::_from) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->lexer_.skip();

    if (this->peek().type != token_type::string) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->lexer_.skip();

    if (this->peek().type == token_type::semicolon) {
      this->lexer_.skip();
    }
  }

  template <class Visitor>
  void parse_let_bindings(Visitor &v, variable_kind declaration_kind) {
    source_code_span let_span = this->peek().span();
    this->lexer_.skip();
    bool first_binding = true;
    for (;;) {
      std::optional<source_code_span> comma_span = std::nullopt;
      if (!first_binding) {
        if (this->peek().type != token_type::comma) {
          break;
        }
        comma_span = this->peek().span();
        this->lexer_.skip();
      }

      switch (this->peek().type) {
        case token_type::identifier:
        case token_type::left_curly:
          this->parse_binding_element(v, declaration_kind);
          break;
        case token_type::_if:
        case token_type::number:
          this->error_reporter_->report_error_invalid_binding_in_let_statement(
              this->peek().span());
          break;
        default:
          if (first_binding) {
            this->error_reporter_->report_error_let_with_no_bindings(let_span);
          } else {
            assert(comma_span.has_value());
            this->error_reporter_->report_error_stray_comma_in_let_statement(
                *comma_span);
          }
          break;
      }
      first_binding = false;
    }

    if (this->peek().type == token_type::semicolon) {
      this->lexer_.skip();
    }
  }

  template <class Visitor>
  void parse_binding_element(Visitor &v, variable_kind declaration_kind) {
    buffering_visitor lhs;

    switch (this->peek().type) {
      case token_type::identifier: {
        identifier name = this->peek().identifier_name();
        this->lexer_.skip();
        lhs.visit_variable_declaration(name, declaration_kind);
        break;
      }

      case token_type::left_curly:
        this->lexer_.skip();
        switch (this->peek().type) {
          case token_type::right_curly:
            break;
          default:
            this->parse_binding_element(v, declaration_kind);
            break;
        }

        while (this->peek().type == token_type::comma) {
          this->lexer_.skip();
          this->parse_binding_element(v, declaration_kind);
        }

        switch (this->peek().type) {
          case token_type::right_curly:
            this->lexer_.skip();
            break;
          default:
            QLJS_PARSER_UNIMPLEMENTED();
            break;
        }
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
    }

    if (this->peek().type == token_type::equal) {
      this->lexer_.skip();
      this->parse_expression(v, expression_options{.parse_commas = false});
    }
    lhs.move_into(v);
  }

  template <class Visitor>
  void parse_template(Visitor &v) {
    const char *template_begin = this->peek().begin;
    for (;;) {
      assert(this->peek().type == token_type::incomplete_template);
      this->lexer_.skip();
      this->parse_expression(v, expression_options{.parse_commas = true});
      switch (this->peek().type) {
        case token_type::right_curly:
          this->lexer_.skip_in_template(template_begin);
          switch (this->peek().type) {
            case token_type::complete_template:
              this->lexer_.skip();
              return;

            case token_type::incomplete_template:
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

  const token &peek() const noexcept { return this->lexer_.peek(); }

  class buffering_visitor {
   public:
    template <class Visitor>
    void move_into(Visitor &target) {
      for (const visited_variable_declaration &visit :
           this->visited_variable_declarations_) {
        target.visit_variable_declaration(visit.name, visit.kind);
      }
    }

    void visit_variable_declaration(identifier name, variable_kind kind) {
      this->visited_variable_declarations_.emplace_back(
          visited_variable_declaration{name, kind});
    }

   private:
    struct visited_variable_declaration {
      identifier name;
      variable_kind kind;
    };
    std::vector<visited_variable_declaration> visited_variable_declarations_;
  };

  void crash_on_unimplemented_token(const char *qljs_file_name, int qljs_line,
                                    const char *qljs_function_name);

  lexer lexer_;
  quicklint_js::locator locator_;
  error_reporter *error_reporter_;
};
}  // namespace quicklint_js

#undef QLJS_PARSER_UNIMPLEMENTED

#endif
