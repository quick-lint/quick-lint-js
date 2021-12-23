// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/c-api-error-reporter.h>
#include <quick-lint-js/c-api.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/document-base.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/web-demo-location.h>
#include <vector>

struct qljs_web_demo_document final
    : public quick_lint_js::qljs_document_base<
          quick_lint_js::web_demo_locator,
          quick_lint_js::c_api_error_reporter<
              qljs_web_demo_diagnostic, quick_lint_js::web_demo_locator>> {
 public:
  void set_text(quick_lint_js::string8_view replacement) {
    this->document_.set_text(replacement);
  }

  void set_config_text(quick_lint_js::string8_view text) {
    quick_lint_js::padded_string padded_text(text);
    this->config_.reset();
    this->config_.load_from_json(&padded_text,
                                 &quick_lint_js::null_error_reporter::instance);
  }
};

qljs_web_demo_document* qljs_web_demo_create_document(void) {
  qljs_web_demo_document* p = new qljs_web_demo_document();
  return p;
}

void qljs_web_demo_destroy_document(qljs_web_demo_document* p) { delete p; }

void qljs_web_demo_set_text(qljs_web_demo_document* p, const void* text_utf_8,
                            size_t text_byte_count) {
  p->set_text(quick_lint_js::string8_view(
      reinterpret_cast<const quick_lint_js::char8*>(text_utf_8),
      text_byte_count));
}

void qljs_web_demo_set_config_text(qljs_web_demo_document* p,
                                   const void* text_utf_8,
                                   size_t text_byte_count) {
  p->set_config_text(quick_lint_js::string8_view(
      reinterpret_cast<const quick_lint_js::char8*>(text_utf_8),
      text_byte_count));
}

const qljs_web_demo_diagnostic* qljs_web_demo_lint(qljs_web_demo_document* p) {
  return p->lint();
}

const qljs_web_demo_diagnostic* qljs_web_demo_lint_as_config_file(
    qljs_web_demo_document* p) {
  return p->lint_as_config_file();
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
