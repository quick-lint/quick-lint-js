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
#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/lsp/lsp-document-text.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/web-demo-location.h>
#include <vector>

using namespace quick_lint_js;

struct QLJS_Web_Demo_Document final {
  Padded_String text_;
  C_API_Diag_Reporter<QLJS_Web_Demo_Diagnostic, Web_Demo_Locator>
      diag_reporter_;
  Configuration config_;
  Linter_Options linter_options_;
  bool is_config_json_ = false;
  QLJS_Web_Demo_Document* config_document_ = nullptr;
  bool need_update_config_ = true;
};

QLJS_Web_Demo_Document* qljs_web_demo_create_document(void) {
  QLJS_Web_Demo_Document* p = new QLJS_Web_Demo_Document();
  return p;
}

void qljs_web_demo_destroy_document(QLJS_Web_Demo_Document* p) { delete p; }

void qljs_web_demo_set_text(QLJS_Web_Demo_Document* p, const void* text_utf_8,
                            size_t text_byte_count) {
  p->text_ = Padded_String(String8_View(
      reinterpret_cast<const Char8*>(text_utf_8), text_byte_count));
}

void qljs_web_demo_set_config(QLJS_Web_Demo_Document* js_document,
                              QLJS_Web_Demo_Document* config_document) {
  js_document->need_update_config_ = true;
  js_document->config_document_ = config_document;
}

void qljs_web_demo_set_language_options(QLJS_Web_Demo_Document* p,
                                        QLJS_Language_Options options) {
  p->linter_options_.jsx = options & qljs_language_options_jsx_bit;
  p->linter_options_.typescript =
      options & qljs_language_options_typescript_bit;
  p->is_config_json_ = options & qljs_language_options_config_json_bit;
}

void qljs_web_demo_set_locale(QLJS_Web_Demo_Document* p, const char* locale) {
  Translator t;
  t.use_messages_from_locale(locale);
  p->diag_reporter_.set_translator(t);
}

const QLJS_Web_Demo_Diagnostic* qljs_web_demo_lint(QLJS_Web_Demo_Document* p) {
  Monotonic_Allocator temp_memory("qljs_web_demo_lint");

  if (p->need_update_config_) {
    p->config_.reset();
    if (p->config_document_) {
      Diag_List diags(&temp_memory);
      p->config_.load_from_json(&p->config_document_->text_, &diags);
    }
  }

  p->diag_reporter_.reset();
  p->diag_reporter_.set_input(&p->text_);
  if (p->is_config_json_) {
    Diag_List diags(&temp_memory);
    Configuration().load_from_json(&p->text_, &diags);
    p->diag_reporter_.report(diags);
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
    C_String_List_View locale_table(translation_data.locale_table);
    for (C_String_List_Iterator it = locale_table.begin();
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
