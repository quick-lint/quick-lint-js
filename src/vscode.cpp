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

#include <cstddef>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-document.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/vscode-error-reporter.h>
#include <quick-lint-js/vscode.h>

struct qljs_vscode_parser {
 public:
  const qljs_vscode_diagnostic* lint() {
    return this->error_reporter_.get_diagnostics();
  }

  void replace_text(int start_line, int start_character, int end_line,
                    int end_character,
                    quick_lint_js::string8_view replacement) {
    using namespace quick_lint_js;

    this->document_.replace_text(
        lsp_range{
            .start = {.line = start_line, .character = start_character},
            .end = {.line = end_line, .character = end_character},
        },
        replacement);

    this->error_reporter_.reset();
    this->error_reporter_.set_input(this->document_.string(),
                                    &this->document_.locator());
    parser p(this->document_.string(), &this->error_reporter_);
    linter l(&this->error_reporter_);
    p.parse_and_visit_module(l);
  }

 private:
  quick_lint_js::lsp_document document_;
  quick_lint_js::vscode_error_reporter error_reporter_;
};

qljs_vscode_parser* qljs_vscode_create_parser(void) {
  qljs_vscode_parser* p = new qljs_vscode_parser();
  return p;
}

void qljs_vscode_destroy_parser(qljs_vscode_parser* p) { delete p; }

void qljs_vscode_replace_text(qljs_vscode_parser* p, int start_line,
                              int start_character, int end_line,
                              int end_character,
                              const void* replacement_text_utf_8,
                              size_t replacement_text_byte_count) {
  p->replace_text(
      start_line, start_character, end_line, end_character,
      quick_lint_js::string8_view(
          reinterpret_cast<const quick_lint_js::char8*>(replacement_text_utf_8),
          replacement_text_byte_count));
}

const qljs_vscode_diagnostic* qljs_vscode_lint(qljs_vscode_parser* p) {
  return p->lint();
}
