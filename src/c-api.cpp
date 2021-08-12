// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <quick-lint-js/c-api-error-reporter.h>
#include <quick-lint-js/c-api.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/web-demo-location.h>

#if QLJS_SUBLIME_TEXT_PLUGIN
#include <quick-lint-js/program-error.h>
#include <quick-lint-js/sublime-text-3-location.h>
#include <quick-lint-js/sublime-text.h>
#endif

namespace quick_lint_js {
namespace {
template <class Locator, class ErrorReporter>
class qljs_parser_base {
 public:
  const auto* lint() {
    this->error_reporter_.reset();
    this->error_reporter_.set_input(this->document_.string(),
                                    &this->document_.locator());
    parser p(this->document_.string(), &this->error_reporter_);
    linter l(&this->error_reporter_, &this->config_.globals());
    // TODO(strager): Use parse_and_visit_module_catching_unimplemented instead
    // of parse_and_visit_module to avoid crashing on QLJS_PARSER_UNIMPLEMENTED.
    p.parse_and_visit_module(l);

    return this->error_reporter_.get_diagnostics();
  }

  quick_lint_js::document<Locator> document_;
  ErrorReporter error_reporter_;
  configuration config_;
};
}
}

struct qljs_vscode_parser final
    : public quick_lint_js::qljs_parser_base<
          quick_lint_js::lsp_locator,
          quick_lint_js::c_api_error_reporter<qljs_vscode_diagnostic,
                                              quick_lint_js::lsp_locator>> {
 public:
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
  }
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

struct qljs_web_demo_parser final
    : public quick_lint_js::qljs_parser_base<
          quick_lint_js::web_demo_locator,
          quick_lint_js::c_api_error_reporter<
              qljs_web_demo_diagnostic, quick_lint_js::web_demo_locator>> {
 public:
  void set_text(quick_lint_js::string8_view replacement) {
    this->document_.set_text(replacement);
  }
};

qljs_web_demo_parser* qljs_web_demo_create_parser(void) {
  qljs_web_demo_parser* p = new qljs_web_demo_parser();
  return p;
}

void qljs_web_demo_destroy_parser(qljs_web_demo_parser* p) { delete p; }

void qljs_web_demo_set_text(qljs_web_demo_parser* p, const void* text_utf_8,
                            size_t text_byte_count) {
  p->set_text(quick_lint_js::string8_view(
      reinterpret_cast<const quick_lint_js::char8*>(text_utf_8),
      text_byte_count));
}

const qljs_web_demo_diagnostic* qljs_web_demo_lint(qljs_web_demo_parser* p) {
  return p->lint();
}

#if QLJS_SUBLIME_TEXT_PLUGIN
struct qljs_sublime_text_3_parser final
    : public quick_lint_js::qljs_parser_base<
          quick_lint_js::sublime_text_3_locator,
          quick_lint_js::c_api_error_reporter<
              qljs_sublime_text_3_diagnostic,
              quick_lint_js::sublime_text_3_locator>> {
 public:
  void set_text(quick_lint_js::string8_view replacement) {
    this->document_.set_text(replacement);
  }
};

qljs_sublime_text_3_parser* qljs_sublime_text_3_create_parser(void) {
#if QLJS_SUBLIME_TEXT_PLUGIN_TEST
  QLJS_SUBLIME_INITILIZE_TEST_CRASH();
#endif

  QLJS_SUBLIME_TEXT_DEFINE_SIGNAL_HANDLER();
  qljs_sublime_text_3_parser* p = new qljs_sublime_text_3_parser();
  return p;
}

void qljs_sublime_text_3_destroy_parser(qljs_sublime_text_3_parser* p) {
  delete p;
}

qljs_sublime_text_3_error qljs_sublime_text_3_set_text(
    qljs_sublime_text_3_parser* p, const void* text_utf_8,
    size_t text_byte_count) {
  QLJS_SUBLIME_TEXT_TRY() {
    p->set_text(quick_lint_js::string8_view(
        reinterpret_cast<const quick_lint_js::char8*>(text_utf_8),
        text_byte_count));
    return qljs_sublime_text_3_error{nullptr};
  }
  QLJS_SUBLIME_TEXT_CATCH() {
    qljs_sublime_text_3_error error =
        qljs_sublime_text_3_error{qljs_sublime_text_program_error_reports};
    QLJS_CLEAR_PROGRAM_ERROR();
    return error;
  }
}

const qljs_sublime_text_3_result* qljs_sublime_text_3_lint(
    qljs_sublime_text_3_parser* p) {
  QLJS_SUBLIME_TEXT_TRY() {
    return new qljs_sublime_text_3_result{.value = {.diagnostics = p->lint()},
                                          .is_diagnostics = true};
  }
  QLJS_SUBLIME_TEXT_CATCH() {
    qljs_sublime_text_3_error error =
        qljs_sublime_text_3_error{qljs_sublime_text_program_error_reports};
    QLJS_CLEAR_PROGRAM_ERROR();
    return new qljs_sublime_text_3_result{.value = {.error = error},
                                          .is_diagnostics = false};
  }
}

struct qljs_sublime_text_4_parser final
    : public quick_lint_js::qljs_parser_base<
          quick_lint_js::lsp_locator,
          quick_lint_js::c_api_error_reporter<qljs_sublime_text_4_diagnostic,
                                              quick_lint_js::lsp_locator>> {
 public:
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
  }
};

qljs_sublime_text_4_parser* qljs_sublime_text_4_create_parser(void) {
#if QLJS_SUBLIME_TEXT_PLUGIN_TEST
  QLJS_SUBLIME_INITILIZE_TEST_CRASH();
#endif

  QLJS_SUBLIME_TEXT_DEFINE_SIGNAL_HANDLER();
  qljs_sublime_text_4_parser* p = new qljs_sublime_text_4_parser();
  return p;
}

void qljs_sublime_text_4_destroy_parser(qljs_sublime_text_4_parser* p) {
  delete p;
}

qljs_sublime_text_4_error qljs_sublime_text_4_replace_text(
    qljs_sublime_text_4_parser* p, int start_line, int start_character,
    int end_line, int end_character, const void* replacement_text_utf_8,
    size_t replacement_text_byte_count) {
  QLJS_SUBLIME_TEXT_TRY() {
    p->replace_text(start_line, start_character, end_line, end_character,
                    quick_lint_js::string8_view(
                        reinterpret_cast<const quick_lint_js::char8*>(
                            replacement_text_utf_8),
                        replacement_text_byte_count));
    return qljs_sublime_text_4_error{nullptr};
  }
  QLJS_SUBLIME_TEXT_CATCH() {
    qljs_sublime_text_4_error error =
        qljs_sublime_text_4_error{qljs_sublime_text_program_error_reports};
    QLJS_CLEAR_PROGRAM_ERROR();
    return error;
  }
}

const qljs_sublime_text_4_result* qljs_sublime_text_4_lint(
    qljs_sublime_text_4_parser* p) {
  QLJS_SUBLIME_TEXT_TRY() {
    return new qljs_sublime_text_4_result{.value = {.diagnostics = p->lint()},
                                          .is_diagnostics = true};
  }
  QLJS_SUBLIME_TEXT_CATCH() {
    qljs_sublime_text_4_error error =
        qljs_sublime_text_4_error{qljs_sublime_text_program_error_reports};
    QLJS_CLEAR_PROGRAM_ERROR();
    return new qljs_sublime_text_4_result{.value = {.error = error},
                                          .is_diagnostics = false};
  }
}
#endif

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
