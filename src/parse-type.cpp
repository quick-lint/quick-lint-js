// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/container/pmr/memory_resource.hpp>
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
#include <quick-lint-js/null-visitor.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <utility>

namespace quick_lint_js {
void parser::parse_and_visit_typescript_colon_type_expression(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::colon);
  if (!this->options_.typescript && !this->in_typescript_only_construct_) {
    this->diag_reporter_->report(
        diag_typescript_type_annotations_not_allowed_in_javascript{
            .type_colon = this->peek().span(),
        });
  }
  this->skip();
  this->parse_and_visit_typescript_type_expression(v);
}

void parser::parse_and_visit_typescript_type_expression(parse_visitor_base &v) {
  depth_guard guard(this);
  switch (this->peek().type) {
  case token_type::complete_template:
  case token_type::kw_any:
  case token_type::kw_bigint:
  case token_type::kw_boolean:
  case token_type::kw_never:
  case token_type::kw_null:
  case token_type::kw_number:
  case token_type::kw_object:
  case token_type::kw_string:
  case token_type::kw_symbol:
  case token_type::kw_this:
  case token_type::kw_undefined:
  case token_type::kw_unknown:
  case token_type::kw_void:
  case token_type::number:
  case token_type::string:
    break;

  // `template ${sometype}`
  case token_type::incomplete_template:
    this->parse_and_visit_typescript_template_type_expression(v);
    break;

  case token_type::identifier: {
    identifier name = this->peek().identifier_name();
    this->skip();
    if (this->peek().type == token_type::dot) {
      this->skip();
      QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);
      this->skip();
      v.visit_variable_namespace_use(name);
    } else {
      v.visit_variable_type_use(name);
    }
    break;
  }

  // [A, B, C]
  case token_type::left_square:
    this->parse_and_visit_typescript_tuple_type_expression(v);
    break;

  // (typeexpr)
  case token_type::left_paren:
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
    QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::right_paren);
    this->skip();
    break;

  // { key: value }
  case token_type::left_curly:
    this->parse_and_visit_typescript_object_type_expression(v);
    break;

  default:
    QLJS_PARSER_UNIMPLEMENTED();
    break;
  }
}

void parser::parse_and_visit_typescript_object_type_expression(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::left_curly);
  this->skip();

  bool is_first = true;
  for (;;) {
    if (!is_first) {
      switch (this->peek().type) {
      case token_type::comma:
        this->skip();
        break;

      case token_type::right_curly:
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }
    }

    if (this->peek().type == token_type::kw_readonly) {
      // { readonly prop: Type }
      this->skip();
    }

    switch (this->peek().type) {
    // { prop }
    // { prop: Type }
    // { prop?: Type }
    case token_type::identifier:
      this->skip();
      if (this->peek().type == token_type::question) {
        // { prop? }
        this->skip();
      }
      if (this->peek().type == token_type::colon) {
        // { prop: Type }
        this->parse_and_visit_typescript_colon_type_expression(v);
      }
      break;

    case token_type::right_curly:
      this->skip();
      return;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    is_first = false;
  }
}

void parser::parse_and_visit_typescript_template_type_expression(
    parse_visitor_base &v) {
  const char8 *template_begin = this->peek().begin;
  for (;;) {
    QLJS_ASSERT(this->peek().type == token_type::incomplete_template);
    // TODO(strager): report_errors_for_escape_sequences_in_template
    this->skip();
    this->parse_and_visit_typescript_type_expression(v);
    switch (this->peek().type) {
    case token_type::right_curly:
      this->lexer_.skip_in_template(template_begin);
      switch (this->peek().type) {
      case token_type::complete_template:
        // TODO(strager): report_errors_for_escape_sequences_in_template
        this->skip();
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

void parser::parse_and_visit_typescript_tuple_type_expression(
    parse_visitor_base &v) {
  QLJS_ASSERT(this->peek().type == token_type::left_square);
  this->skip();
  bool is_first = true;
  for (;;) {
    if (!is_first) {
      switch (this->peek().type) {
      case token_type::comma:
        this->skip();
        break;

      case token_type::right_square:
        break;

      default:
        QLJS_PARSER_UNIMPLEMENTED();
      }
    }
    switch (this->peek().type) {
    case token_type::right_square:
      this->skip();
      return;

    case token_type::identifier:
      this->parse_and_visit_typescript_type_expression(v);
      break;

    default:
      QLJS_PARSER_UNIMPLEMENTED();
      break;
    }
    is_first = false;
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
