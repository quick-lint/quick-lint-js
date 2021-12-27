// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/document.h>
#include <quick-lint-js/parse.h>

namespace quick_lint_js {
template <class Locator, template <class, class> class ErrorReporter,
          class Diagnostic>
class document_base {
 public:
  const auto* lint() {
    this->error_reporter_.reset();
    this->error_reporter_.set_input(this->document_.string(),
                                    &this->document_.locator());
    parser p(this->document_.string(), &this->error_reporter_);
    linter l(&this->error_reporter_, &this->config_.globals());
    // TODO(strager): Use parse_and_visit_module_catching_fatal_parse_errors
    // instead of parse_and_visit_module to avoid crashing on
    // QLJS_PARSER_UNIMPLEMENTED.
    p.parse_and_visit_module(l);

    return this->error_reporter_.get_diagnostics();
  }

  const auto* lint_as_config_file() {
    this->error_reporter_.reset();
    this->error_reporter_.set_input(this->document_.string(),
                                    &this->document_.locator());
    configuration().load_from_json(this->document_.string(),
                                   &this->error_reporter_);
    return this->error_reporter_.get_diagnostics();
  }

  quick_lint_js::document<Locator> document_;
  ErrorReporter<Diagnostic, Locator> error_reporter_;
  configuration config_;
};
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
