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

#ifndef QUICK_LINT_JS_PIPE_READER_H
#define QUICK_LINT_JS_PIPE_READER_H

#include <array>
#include <cstddef>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>

#if QLJS_HAVE_CXX_CONCEPTS
#define QLJS_PIPE_READER_PARSER ::quick_lint_js::pipe_reader_parser
#else
#define QLJS_PIPE_READER_PARSER class
#endif

namespace quick_lint_js {
#if QLJS_HAVE_CXX_CONCEPTS
template <class Parser>
concept pipe_reader_parser = requires(Parser p, string8_view data) {
  {p.append(data)};
};
#endif

// An pipe_reader consumes bytes of a pipe or socket and sends them to a
// pipe_reader_parser.
//
// A pipe_reader can be used to read Language Server Protocol data sent by a
// client.
template <QLJS_PIPE_READER_PARSER Parser>
class pipe_reader {
 public:
  template <class... Args>
  explicit pipe_reader(platform_file_ref pipe, Args&&... parser_args)
      : parser_(std::forward<Args>(parser_args)...), pipe_(pipe) {}

  void run() {
    for (;;) {
      // TODO(strager): Pick buffer size intelligently.
      std::array<char8, 1024> buffer;
      file_read_result read_result =
          this->pipe_.read(buffer.data(), buffer.size());
      if (read_result.at_end_of_file) {
        break;
      } else if (read_result.error_message.has_value()) {
        QLJS_UNIMPLEMENTED();
      } else {
        this->parser().append(string8_view(
            buffer.data(), narrow_cast<std::size_t>(read_result.bytes_read)));
      }
    }
  }

  Parser& parser() noexcept { return this->parser_; }

 private:
  Parser parser_;
  platform_file_ref pipe_;
};
}

#endif
