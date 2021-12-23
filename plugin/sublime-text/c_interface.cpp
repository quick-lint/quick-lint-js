// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/crash-handling.h>
#include <quick-lint-js/program-report.h>
#include <quick-lint-js/sublime-text-3-location.h>


struct qljs_sublime_text_3_parser final
    : public quick_lint_js::qljs_document_base<
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
  qljs_sublime_text_3_parser* p = new qljs_sublime_text_3_parser();
  return p;
}

void qljs_sublime_text_3_destroy_parser(qljs_sublime_text_3_parser* p) {
  delete p;
}

const qljs_sublime_text_3_error* qljs_sublime_text_3_set_text(
    qljs_sublime_text_3_parser* p, const void* text_utf_8,
    size_t text_byte_count) {
  QLJS_SUBLIME_TEXT_TRY() {
    p->set_text(quick_lint_js::string8_view(
        reinterpret_cast<const quick_lint_js::char8*>(text_utf_8),
        text_byte_count));
    return nullptr;
  }
  QLJS_SUBLIME_TEXT_CATCH() {
    const char* message = QLJS_SUBLIME_TEXT_COPY_PROGRAM_REPORT();
    QLJS_SUBLIME_TEXT_CLEAR_PROGRAM_REPORT();
    return new qljs_sublime_text_3_error{message};
  }
}

const qljs_sublime_text_3_result* qljs_sublime_text_3_lint(
    qljs_sublime_text_3_parser* p) {
  QLJS_SUBLIME_TEXT_TRY() {
    return new qljs_sublime_text_3_result{.value = {.diagnostics = p->lint()},
                                          .is_diagnostics = true};
  }
  QLJS_SUBLIME_TEXT_CATCH() {
    const char* message = QLJS_SUBLIME_TEXT_COPY_PROGRAM_REPORT();
    QLJS_SUBLIME_TEXT_CLEAR_PROGRAM_REPORT();
    return new qljs_sublime_text_3_result{
        .value = {.error = new qljs_sublime_text_3_error{message}},
        .is_diagnostics = false};
  }
}

struct qljs_sublime_text_4_parser final
    : public quick_lint_js::qljs_document_base<
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
  QLJS_SUBLIME_TEXT_DEFINE_SIGNAL_HANDLER();
  qljs_sublime_text_4_parser* p = new qljs_sublime_text_4_parser();
  return p;
}

void qljs_sublime_text_4_destroy_parser(qljs_sublime_text_4_parser* p) {
  delete p;
}

const qljs_sublime_text_4_error* qljs_sublime_text_4_replace_text(
    qljs_sublime_text_4_parser* p, int start_line, int start_character,
    int end_line, int end_character, const void* replacement_text_utf_8,
    size_t replacement_text_byte_count) {
  QLJS_SUBLIME_TEXT_TRY() {
    p->replace_text(start_line, start_character, end_line, end_character,
                    quick_lint_js::string8_view(
                        reinterpret_cast<const quick_lint_js::char8*>(
                            replacement_text_utf_8),
                        replacement_text_byte_count));
    return nullptr;
  }
  QLJS_SUBLIME_TEXT_CATCH() {
    char* message = QLJS_SUBLIME_TEXT_COPY_PROGRAM_REPORT();
    QLJS_SUBLIME_TEXT_CLEAR_PROGRAM_REPORT();
    return new qljs_sublime_text_4_error{message};
  }
}

const qljs_sublime_text_4_result* qljs_sublime_text_4_lint(
    qljs_sublime_text_4_parser* p) {
  QLJS_SUBLIME_TEXT_TRY() {
    return new qljs_sublime_text_4_result{.value = {.diagnostics = p->lint()},
                                          .is_diagnostics = true};
  }
  QLJS_SUBLIME_TEXT_CATCH() {
    const char* message = QLJS_SUBLIME_TEXT_COPY_PROGRAM_REPORT();
    QLJS_SUBLIME_TEXT_CLEAR_PROGRAM_REPORT();
    return new qljs_sublime_text_4_result{
        .value = {.error = new qljs_sublime_text_4_error{message}},
        .is_diagnostics = false};
  }
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
