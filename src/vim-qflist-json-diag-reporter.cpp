// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/optional.h>
#include <quick-lint-js/output-stream.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/vim-qflist-json-diag-reporter.h>
#include <string>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
vim_qflist_json_diag_reporter::vim_qflist_json_diag_reporter(
    output_stream *output)
    : output_(*output) {
  this->output_.append_literal(u8"{\"qflist\": ["sv);
}

void vim_qflist_json_diag_reporter::set_source(padded_string_view input,
                                               const char *file_name,
                                               int vim_bufnr) {
  this->set_source(input, /*file_name=*/file_name,
                   /*vim_bufnr=*/std::optional<int>(vim_bufnr));
}

void vim_qflist_json_diag_reporter::set_source(padded_string_view input,
                                               const char *file_name,
                                               std::optional<int> vim_bufnr) {
  this->locator_.emplace(input);
  this->file_name_ = file_name;
  this->bufnr_ = vim_bufnr.has_value() ? std::to_string(*vim_bufnr) : "";
}

void vim_qflist_json_diag_reporter::set_source(padded_string_view input,
                                               const char *file_name) {
  this->set_source(input, /*file_name=*/file_name, /*vim_bufnr=*/std::nullopt);
}

void vim_qflist_json_diag_reporter::set_source(padded_string_view input,
                                               int vim_bufnr) {
  this->locator_.emplace(input);
  this->file_name_.clear();
  this->bufnr_ = std::to_string(vim_bufnr);
}

void vim_qflist_json_diag_reporter::finish() {
  this->output_.append_literal(u8"]}"sv);
}

void vim_qflist_json_diag_reporter::report_impl(diag_type type, void *error) {
  if (this->need_comma_) {
    this->output_.append_literal(u8",\n"sv);
  }
  this->need_comma_ = true;
  QLJS_ASSERT(this->locator_.has_value());
  vim_qflist_json_error_formatter formatter(/*output=*/&this->output_,
                                            /*locator=*/*this->locator_,
                                            /*file_name=*/this->file_name_,
                                            /*bufnr=*/this->bufnr_);
  formatter.format(get_diagnostic_info(type), error);
}

vim_qflist_json_error_formatter::vim_qflist_json_error_formatter(
    output_stream *output, quick_lint_js::vim_locator &locator,
    std::string_view file_name, std::string_view bufnr)
    : output_(*output),
      locator_(locator),
      file_name_(file_name),
      bufnr_(bufnr) {}

void vim_qflist_json_error_formatter::write_before_message(
    std::string_view code, diagnostic_severity sev,
    const source_code_span &origin) {
  string8_view severity_type{};
  switch (sev) {
  case diagnostic_severity::error:
    severity_type = u8"E"sv;
    break;
  case diagnostic_severity::note:
    // Don't write notes. Only write the main message.
    return;
  case diagnostic_severity::warning:
    severity_type = u8"W"sv;
    break;
  }

  vim_source_range r = this->locator_.range(origin);
  auto end_col = origin.begin() == origin.end() ? r.begin.col : (r.end.col - 1);
  this->output_.append_literal(u8"{\"col\": "sv);
  this->output_.append_decimal_integer(r.begin.col);
  this->output_.append_literal(u8", \"lnum\": "sv);
  this->output_.append_decimal_integer(r.begin.lnum);
  this->output_.append_literal(u8", \"end_col\": "sv);
  this->output_.append_decimal_integer(end_col);
  this->output_.append_literal(u8", \"end_lnum\": "sv);
  this->output_.append_decimal_integer(r.end.lnum);
  this->output_.append_literal(u8", \"type\": \""sv);
  this->output_.append_copy(severity_type);
  this->output_.append_literal(u8"\", \"nr\": \""sv);
  this->output_.append_copy(to_string8_view(code));
  this->output_.append_literal(u8"\", \"vcol\": 0, \"text\": \""sv);
}

void vim_qflist_json_error_formatter::write_message_part(
    [[maybe_unused]] std::string_view code, diagnostic_severity sev,
    string8_view message) {
  if (sev == diagnostic_severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  write_json_escaped_string(this->output_, message);
}

void vim_qflist_json_error_formatter::write_after_message(
    [[maybe_unused]] std::string_view code, diagnostic_severity sev,
    const source_code_span &) {
  if (sev == diagnostic_severity::note) {
    // Don't write notes. Only write the main message.
    return;
  }

  this->output_.append_copy(u8'\"');
  if (!this->bufnr_.empty()) {
    this->output_.append_literal(u8", \"bufnr\": "sv);
    this->output_.append_copy(to_string8_view(this->bufnr_));
  }
  if (!this->file_name_.empty()) {
    this->output_.append_literal(u8", \"filename\": \""sv);
    write_json_escaped_string(this->output_, to_string8_view(this->file_name_));
    this->output_.append_copy(u8'"');
  }
  this->output_.append_copy(u8'}');

  // If we don't flush, output is buffered. A lot of messages (e.g. over 4 KiB)
  // will fill the buffer and possibly force a flush in the middle of a message.
  // Then, a crash in the future (e.g. an assertion failure) will leave an
  // incomplete message written to the client. The client will have a hard time
  // extracting partial information from the incomplete JSON.
  //
  // If we flush now, it's less likely that a message ends up on the client. The
  // client can easily recover by adding a '}' at the end of the input to make
  // the incomplete JSON valid.
  this->output_.flush();
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
