// quick-lint-js finds bugs in JavaScript programs.
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

#ifndef QUICK_LINT_JS_PARSE_H
#define QUICK_LINT_JS_PARSE_H

#include <cstdlib>
#include <optional>
#include <quick-lint-js/error.h>
#include <quick-lint-js/expression.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <vector>

#define QLJS_PARSER_UNIMPLEMENTED()                                   \
  do {                                                                \
    this->crash_on_unimplemented_token(__FILE__, __LINE__, __func__); \
  } while (false)

#define QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(expected_token_type) \
  do {                                                              \
    if (this->peek().type != (expected_token_type)) {               \
      QLJS_PARSER_UNIMPLEMENTED();                                  \
    }                                                               \
  } while (false)

namespace quick_lint_js {
class parser {
 public:
  explicit parser(const char *input, error_reporter *error_reporter)
      : lexer_(input, error_reporter),
        locator_(input),
        error_reporter_(error_reporter) {}

  quick_lint_js::locator &locator() noexcept { return this->locator_; }

  template <class Visitor>
  void parse_and_visit_module(Visitor &v) {
    while (this->peek().type != token_type::end_of_file) {
      this->parse_and_visit_statement(v);
    }
    v.visit_end_of_module();
  }

  template <class Visitor>
  void parse_and_visit_statement(Visitor &v) {
    switch (this->peek().type) {
      case token_type::_export:
        this->lexer_.skip();
        this->parse_and_visit_declaration(v);
        break;

      case token_type::semicolon:
        this->lexer_.skip();
        break;

      case token_type::_async:
      case token_type::_function:
        this->parse_and_visit_declaration(v);
        break;

      case token_type::_import:
        this->parse_and_visit_import(v);
        break;

      case token_type::_const:
        this->parse_and_visit_let_bindings(v, variable_kind::_const);
        break;

      case token_type::_let:
        this->parse_and_visit_let_bindings(v, variable_kind::_let);
        break;

      case token_type::_var:
        this->parse_and_visit_let_bindings(v, variable_kind::_var);
        break;

      case token_type::identifier:
        this->parse_and_visit_expression(
            v, precedence{.binary_operators = true, .commas = true});

        if (this->peek().type != token_type::semicolon) {
          QLJS_PARSER_UNIMPLEMENTED();
        }
        this->lexer_.skip();
        break;

      case token_type::_class:
        this->parse_and_visit_class(v);
        break;

      case token_type::_return:
        this->lexer_.skip();
        this->parse_and_visit_expression(
            v, precedence{.binary_operators = true, .commas = true});
        break;

      case token_type::_try:
        this->parse_and_visit_try(v);
        break;

      case token_type::right_curly:
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
    }
  }

  template <class Visitor>
  void parse_and_visit_expression(Visitor &v) {
    this->parse_and_visit_expression(
        v, precedence{.binary_operators = true, .commas = true});
  }

  expression_ptr parse_expression() {
    return this->parse_expression(
        precedence{.binary_operators = true, .commas = true});
  }

 private:
  enum class variable_context {
    lhs,
    rhs,
  };

  template <class Visitor>
  void visit_expression(expression_ptr ast, Visitor &v,
                        variable_context context) {
    auto visit_children = [&] {
      for (int i = 0; i < ast->child_count(); ++i) {
        this->visit_expression(ast->child(i), v, context);
      }
    };
    switch (ast->kind()) {
      case expression_kind::_invalid:
      case expression_kind::literal:
        break;
      case expression_kind::_new:
      case expression_kind::_template:
      case expression_kind::binary_operator:
      case expression_kind::call:
        visit_children();
        break;
      case expression_kind::assignment: {
        expression_ptr lhs = ast->child_0();
        expression_ptr rhs = ast->child_1();
        this->visit_expression(lhs, v, variable_context::lhs);
        this->visit_expression(rhs, v, variable_context::rhs);
        switch (lhs->kind()) {
          case expression_kind::variable:
            v.visit_variable_assignment(lhs->variable_identifier());
            break;
          default:
            break;
        }
        break;
      }
      case expression_kind::await:
      case expression_kind::unary_operator:
        this->visit_expression(ast->child_0(), v, context);
        break;
      case expression_kind::dot:
        this->visit_expression(ast->child_0(), v, variable_context::rhs);
        break;
      case expression_kind::variable:
        switch (context) {
          case variable_context::lhs:
            break;
          case variable_context::rhs:
            v.visit_variable_use(ast->variable_identifier());
            break;
        }
        break;
    }
  }

  template <class Visitor>
  void parse_and_visit_declaration(Visitor &v) {
    switch (this->peek().type) {
      case token_type::_async:
        this->lexer_.skip();
        switch (this->peek().type) {
          case token_type::_function:
            this->parse_and_visit_function_declaration(v);
            break;

          default:
            QLJS_PARSER_UNIMPLEMENTED();
            break;
        }
        break;

      case token_type::_function:
        this->parse_and_visit_function_declaration(v);
        break;

      case token_type::_class:
        this->parse_and_visit_class(v);
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
    }
  }

  template <class Visitor>
  void parse_and_visit_function_declaration(Visitor &v) {
    assert(this->peek().type == token_type::_function);
    this->lexer_.skip();

    if (this->peek().type != token_type::identifier) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    v.visit_variable_declaration(this->peek().identifier_name(),
                                 variable_kind::_function);
    this->lexer_.skip();

    this->parse_and_visit_function_parameters_and_body(v);
  }

  template <class Visitor>
  void parse_and_visit_function_parameters_and_body(Visitor &v) {
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
          this->parse_and_visit_binding_element(v, variable_kind::_parameter);
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
      this->parse_and_visit_statement(v);
    }

    if (this->peek().type != token_type::right_curly) {
      QLJS_PARSER_UNIMPLEMENTED();
    }
    this->lexer_.skip();

    v.visit_exit_function_scope();
  }

  template <class Visitor>
  void parse_and_visit_class(Visitor &v) {
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
            this->parse_and_visit_expression(
                v, precedence{.binary_operators = true, .commas = false});
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
        this->parse_and_visit_class_body(v);
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
        break;
    }

    v.visit_exit_class_scope();
  }

  template <class Visitor>
  void parse_and_visit_class_body(Visitor &v) {
    for (;;) {
      switch (this->peek().type) {
        case token_type::_static:
          this->lexer_.skip();
          switch (this->peek().type) {
            case token_type::identifier:
              v.visit_property_declaration(this->peek().identifier_name());
              this->lexer_.skip();
              this->parse_and_visit_function_parameters_and_body(v);
              break;

            default:
              QLJS_PARSER_UNIMPLEMENTED();
              break;
          }
          break;

        case token_type::identifier:
          v.visit_property_declaration(this->peek().identifier_name());
          this->lexer_.skip();
          this->parse_and_visit_function_parameters_and_body(v);
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
  void parse_and_visit_try(Visitor &v) {
    assert(this->peek().type == token_type::_try);
    this->lexer_.skip();

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_curly);
    this->lexer_.skip();
    v.visit_enter_block_scope();

    this->parse_and_visit_statement(v);

    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
    this->lexer_.skip();
    v.visit_exit_block_scope();

    if (this->peek().type == token_type::_catch) {
      this->lexer_.skip();

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_paren);
      this->lexer_.skip();
      v.visit_enter_block_scope();

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
      v.visit_variable_declaration(this->peek().identifier_name(),
                                   variable_kind::_catch);
      this->lexer_.skip();

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
      this->lexer_.skip();

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_curly);
      this->lexer_.skip();

      this->parse_and_visit_statement(v);

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
      this->lexer_.skip();
      v.visit_exit_block_scope();
    }
    if (this->peek().type == token_type::_finally) {
      this->lexer_.skip();

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::left_curly);
      this->lexer_.skip();
      v.visit_enter_block_scope();

      this->parse_and_visit_statement(v);

      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_curly);
      this->lexer_.skip();
      v.visit_exit_block_scope();
    }
  }

  template <class Visitor>
  void parse_and_visit_import(Visitor &v) {
    assert(this->peek().type == token_type::_import);
    this->lexer_.skip();

    switch (this->peek().type) {
      case token_type::identifier:
      case token_type::left_curly:
        this->parse_and_visit_binding_element(v, variable_kind::_import);
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
  void parse_and_visit_let_bindings(Visitor &v,
                                    variable_kind declaration_kind) {
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
          this->parse_and_visit_binding_element(v, declaration_kind);
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
  void parse_and_visit_binding_element(Visitor &v,
                                       variable_kind declaration_kind) {
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
            this->parse_and_visit_binding_element(v, declaration_kind);
            break;
        }

        while (this->peek().type == token_type::comma) {
          this->lexer_.skip();
          this->parse_and_visit_binding_element(v, declaration_kind);
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
      this->parse_and_visit_expression(
          v, precedence{.binary_operators = true, .commas = false});
    }
    lhs.move_into(v);
  }

  struct precedence {
    bool binary_operators;
    bool commas;
  };

  template <class Visitor>
  void parse_and_visit_expression(Visitor &v, precedence prec) {
    expression_ptr ast = this->parse_expression(prec);
    this->visit_expression(ast, v, variable_context::rhs);
  }

  expression_ptr parse_expression(precedence);

  expression_ptr parse_expression_remainder(expression_ptr, precedence);

  expression_ptr parse_template();

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

  [[noreturn]] void crash_on_unimplemented_token(
      const char *qljs_file_name, int qljs_line,
      const char *qljs_function_name);

  template <expression_kind Kind, class... Args>
  expression_ptr make_expression(Args &&... args) {
    return this->expressions_.make_expression<Kind>(
        std::forward<Args>(args)...);
  }

  lexer lexer_;
  quick_lint_js::locator locator_;
  error_reporter *error_reporter_;
  expression_arena expressions_;
};
}  // namespace quick_lint_js

#undef QLJS_PARSER_UNIMPLEMENTED

#endif
