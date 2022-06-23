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
  QLJS_PARSER_UNIMPLEMENTED_IF_NOT_TOKEN(token_type::identifier);

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
