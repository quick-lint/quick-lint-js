// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/c-api-diag-reporter.h>
#include <quick-lint-js/c-api.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/c-string-list.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/web-demo-location.h>
#include <vector>

using namespace quick_lint_js;

struct qljs_web_demo_document final {
  padded_string text_;
  c_api_diag_reporter<qljs_web_demo_diagnostic, web_demo_locator>
      diag_reporter_;
  configuration config_;
  linter_options linter_options_;
  bool is_config_json_ = false;
  qljs_web_demo_document* config_document_ = nullptr;
  bool need_update_config_ = true;
};

qljs_web_demo_document* qljs_web_demo_create_document(void) {
  qljs_web_demo_document* p = new qljs_web_demo_document();
  return p;
}

void qljs_web_demo_destroy_document(qljs_web_demo_document* p) { delete p; }

void qljs_web_demo_set_text(qljs_web_demo_document* p, const void* text_utf_8,
                            size_t text_byte_count) {
  p->text_ = padded_string(string8_view(
      reinterpret_cast<const char8*>(text_utf_8), text_byte_count));
}

void qljs_web_demo_set_config(qljs_web_demo_document* js_document,
                              qljs_web_demo_document* config_document) {
  js_document->need_update_config_ = true;
  js_document->config_document_ = config_document;
}

void qljs_web_demo_set_language_options(qljs_web_demo_document* p,
                                        qljs_language_options options) {
  p->linter_options_.jsx = options & qljs_language_options_jsx_bit;
  p->linter_options_.typescript =
      options & qljs_language_options_typescript_bit;
  p->is_config_json_ = options & qljs_language_options_config_json_bit;
}

void qljs_web_demo_set_locale(qljs_web_demo_document* p, const char* locale) {
  translator t;
  t.use_messages_from_locale(locale);
  p->diag_reporter_.set_translator(t);
}

const qljs_web_demo_diagnostic* qljs_web_demo_lint(qljs_web_demo_document* p) {
  if (p->need_update_config_) {
    p->config_.reset();
    if (p->config_document_) {
      p->config_.load_from_json(&p->config_document_->text_,
                                &null_diag_reporter::instance);
    }
  }

  p->diag_reporter_.reset();
  p->diag_reporter_.set_input(&p->text_);
  if (p->is_config_json_) {
    configuration().load_from_json(&p->text_, &p->diag_reporter_);
  } else {
    parse_and_lint(&p->text_, p->diag_reporter_, p->config_.globals(),
                   p->linter_options_);
  }
  return p->diag_reporter_.get_diagnostics();
}

const char* const* qljs_list_locales() {
  static const char* const* locale_list = [] {
    std::size_t locale_count = translation_table_locale_count + 1;
    const char** locales = new const char*[locale_count + 1];

    std::size_t i = 0;
    c_string_list_view locale_table(translation_data.locale_table);
    for (c_string_list_iterator it = locale_table.begin();
         it != locale_table.end(); ++it, ++i) {
      locales[i] = it.c_str();
    }
    locales[i] = "";  // Default locale.
    ++i;
    QLJS_ASSERT(i == locale_count);
    locales[i] = nullptr;  // Terminator.

    return locales;
  }();
  return locale_list;
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
