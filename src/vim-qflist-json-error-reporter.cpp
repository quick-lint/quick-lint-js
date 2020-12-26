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

#include <iostream>
#include <ostream>
#include <quick-lint-js/error.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/vim-qflist-json-error-reporter.h>
#include <string>

namespace quick_lint_js {
vim_qflist_json_error_reporter::vim_qflist_json_error_reporter(
    std::ostream &output)
    : output_(output) {
  this->output_ << "{\"qflist\": [";
}

void vim_qflist_json_error_reporter::set_source(padded_string_view input,
                                                const char *file_name,
                                                int vim_bufnr) {
  this->set_source(input, /*file_name=*/file_name,
                   /*vim_bufnr=*/std::optional<int>(vim_bufnr));
}

void vim_qflist_json_error_reporter::set_source(padded_string_view input,
                                                const char *file_name,
                                                std::optional<int> vim_bufnr) {
  this->locator_.emplace(input);
  this->file_name_ = file_name;
  this->bufnr_ = vim_bufnr.has_value() ? std::to_string(*vim_bufnr) : "";
}

void vim_qflist_json_error_reporter::set_source(padded_string_view input,
                                                const char *file_name) {
  this->set_source(input, /*file_name=*/file_name, /*vim_bufnr=*/std::nullopt);
}

void vim_qflist_json_error_reporter::set_source(padded_string_view input,
                                                int vim_bufnr) {
  this->locator_.emplace(input);
  this->file_name_.clear();
  this->bufnr_ = std::to_string(vim_bufnr);
}

void vim_qflist_json_error_reporter::finish() { this->output_ << "]}"; }

#define QLJS_ERROR_TYPE(name, struct_body, format_call) \
  void vim_qflist_json_error_reporter::report(name e) { \
    this->begin_error();                                \
    format_error(e, this->format());                    \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

void vim_qflist_json_error_reporter::report_fatal_error_unimplemented_character(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    const char8 *character) {
  error_reporter::write_fatal_error_unimplemented_character(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*character=*/character,
      /*locator=*/nullptr,
      /*out=*/std::cerr);
}

void vim_qflist_json_error_reporter::report_fatal_error_unimplemented_token(
    const char *qljs_file_name, int qljs_line, const char *qljs_function_name,
    token_type type, const char8 *token_begin) {
  error_reporter::write_fatal_error_unimplemented_token(
      /*qljs_file_name=*/qljs_file_name,
      /*qljs_line=*/qljs_line,
      /*qljs_function_name=*/qljs_function_name,
      /*type=*/type,
      /*token_begin=*/token_begin,
      /*locator=*/nullptr,
      /*out=*/std::cerr);
}

void vim_qflist_json_error_reporter::begin_error() {
  if (this->need_comma_) {
    this->output_ << ",\n";
  }
  this->need_comma_ = true;
}

vim_qflist_json_error_formatter vim_qflist_json_error_reporter::format() {
  QLJS_ASSERT(this->locator_.has_value());
  return vim_qflist_json_error_formatter(/*output=*/this->output_,
                                         /*locator=*/*this->locator_,
                                         /*file_name=*/this->file_name_,
                                         /*bufnr=*/this->bufnr_);
}

vim_qflist_json_error_formatter::vim_qflist_json_error_formatter(
    std::ostream &output, quick_lint_js::vim_locator &locator,
    std::string_view file_name, std::string_view bufnr)
    : output_(output),
      locator_(locator),
      file_name_(file_name),
      bufnr_(bufnr) {}

void vim_qflist_json_error_formatter::write_before_message(
    severity sev, const source_code_span &origin) {
  std::string_view severity_type;
  switch (sev) {
  case severity::error:
    severity_type = "E";
    break;
  case severity::note:
    // Don't write notes. Only write the main message.
    return;
  case severity::warning:
    severity_type = "W";
    break;
  }

  vim_source_range r = this->locator_.range(origin);
  auto end_col = origin.begin() == origin.end() ? r.begin.col : (r.end.col - 1);
  this->output_ << "{\"col\": " << r.begin.col << ", \"lnum\": " << r.begin.lnum
                << ", \"end_col\": " << end_col
                << ", \"end_lnum\": " << r.end.lnum << ", \"type\": \""
                << severity_type << "\", \"vcol\": 0, \"text\": \"";
}

void vim_qflist_json_error_formatter::write_message_part(severity sev,
                                                         string8_view message) {
  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  write_json_escaped_string(this->output_, message);
}

void vim_qflist_json_error_formatter::write_after_message(
    severity sev, const source_code_span &) {
  if (sev == severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  this->output_ << '\"';
  if (!this->bufnr_.empty()) {
    this->output_ << ", \"bufnr\": " << this->bufnr_;
  }
  if (!this->file_name_.empty()) {
    this->output_ << ", \"filename\": \"";
    write_json_escaped_string(this->output_, this->file_name_);
    this->output_ << '"';
  }
  this->output_ << '}';
}
}
