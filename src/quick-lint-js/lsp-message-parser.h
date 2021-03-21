// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_MESSAGE_PARSER_H
#define QUICK_LINT_JS_LSP_MESSAGE_PARSER_H

#include <cstddef>
#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/narrow-cast.h>
#include <vector>

namespace quick_lint_js {
class lsp_message_parser_base {
 protected:
  struct parsed_message_headers {
    std::size_t content_length;
  };

  struct parsed_header {
    string8_view name;
    string8_view value;
    const char8* next;
  };

  const char8* find_content_begin(const char8* headers_begin);

  static parsed_message_headers parse_message_headers(
      const char8* headers_begin);
  static parsed_header parse_header(const char8*);
  static bool header_is(string8_view header_name,
                        string8_view expected_header_name);

  std::vector<char8> buffer_;

  // If !pending_message_content_length_.has_value(), buffer_ contains message
  // headers (and possibly message content and other messages afterwards).
  //
  // If pending_message_content_length_.has_value(), buffer_ contains message
  // content (and possibly other messages afterwards).
  std::optional<std::size_t> pending_message_content_length_;
};

// An lsp_message_parser parses Language Server Protocol's message headers.
//
// lsp_message_parser calls the following member function on Derived when a
// full message is successfully parsed:
//
// * void message_parsed(string8_view message_content)
//
// lsp_message_parser is ignorant of JSON and JSON-RPC. JSON and JSON-RPC are
// handled in lsp_endpoint.
//
// lsp_message_parser implements the Curiously Recurring Template Pattern
// (CRTP).
template <class Derived>
class lsp_message_parser : private lsp_message_parser_base {
 public:
  void append(string8_view data) {
    this->buffer_.insert(this->buffer_.end(), data.data(),
                         data.data() + data.size());
    this->parse();
  }

 private:
  void parse() {
    const char8* headers_or_content_begin = this->buffer_.data();
    for (;;) {
      if (this->pending_message_content_length_.has_value()) {
        // Parse message content.
        const char8* content_begin = headers_or_content_begin;
        const char8* content_end = this->parse_message_content(
            /*content_begin=*/content_begin,
            /*content_length=*/*this->pending_message_content_length_);
        if (!content_end) {
          break;
        }
        this->pending_message_content_length_ = std::nullopt;
        headers_or_content_begin = content_end;
      } else {
        // Parse message headers.
        const char8* headers_begin = headers_or_content_begin;
        const char8* content_begin = this->find_content_begin(headers_begin);
        if (!content_begin) {
          break;
        }
        parsed_message_headers headers =
            this->parse_message_headers(/*headers_begin=*/headers_begin);
        this->pending_message_content_length_ = headers.content_length;
        headers_or_content_begin = content_begin;
      }
    }

    this->buffer_.erase(this->buffer_.begin(),
                        this->buffer_.begin() +
                            (headers_or_content_begin - this->buffer_.data()));
  }

  const char8* parse_message_content(const char8* content_begin,
                                     std::size_t content_length) {
    std::size_t content_bytes_in_buffer = narrow_cast<std::size_t>(
        (this->buffer_.data() + this->buffer_.size()) - content_begin);
    bool full_content_received = content_bytes_in_buffer >= content_length;
    if (!full_content_received) {
      return nullptr;
    }

    const char8* content_end = content_begin + content_length;
    static_cast<Derived*>(this)->message_parsed(
        string8_view(content_begin, content_length));
    return content_end;
  }
};
}

#endif

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
