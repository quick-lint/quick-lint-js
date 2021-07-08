// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILE_H
#define QUICK_LINT_JS_FILE_H

#include <boost/leaf/common.hpp>
#include <boost/leaf/pred.hpp>
#include <boost/leaf/result.hpp>
#include <cstdio>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/sloppy-result.h>
#include <string>
#include <tuple>

namespace quick_lint_js {
struct read_file_result {
  padded_string content;
  std::string error;
  bool is_not_found_error = false;

  bool ok() const noexcept { return this->error.empty(); }
  void exit_if_not_ok() const;

  static read_file_result failure(const std::string &error);
};

struct e_api_read_file {};
struct e_file_too_large {};

// Possible error types:
//
// * boost::leaf::e_errno
// * boost::leaf::e_file_name
// * boost::leaf::windows::e_LastError
// * e_api_read_file (always present)
// * e_file_too_large
boost::leaf::result<padded_string> read_file(const char *path);
boost::leaf::result<padded_string> read_file(platform_file_ref);
boost::leaf::result<padded_string> read_stdin(void);

sloppy_result<padded_string> read_file_sloppy(const char *path);
sloppy_result<padded_string> read_file_sloppy(const char *path,
                                              platform_file_ref);

padded_string read_file_or_exit(const char *path);

void write_file(const std::string &path, string8_view content);
void write_file(const char *path, string8_view content);

// Valid signatures for handle_error:
// <<any rvalue result type>> handle_error();
template <class Func>
auto make_file_not_found_handler(const Func &handle_error) {
  return std::tuple(
#if QLJS_HAVE_WINDOWS_H
      [handle_error](boost::leaf::match_value<boost::leaf::windows::e_LastError,
                                              ERROR_FILE_NOT_FOUND>) {
        return handle_error();
      }
#endif
#if QLJS_HAVE_UNISTD_H
          [handle_error](
              boost::leaf::match_value<boost::leaf::e_errno, ENOENT>) {
            return handle_error();
          }
#endif
  );
}

// Valid signatures for handle_error:
// <<any rvalue result type>> handle_error(const std::string &message);
// <<any rvalue result type>> handle_error(std::string &&message);
template <class Func>
auto make_read_file_error_handlers(const Func &handle_error) {
  using namespace std::literals::string_literals;
  return std::tuple(
      [handle_error](e_api_read_file, e_file_too_large,
                     const boost::leaf::e_file_name &path) {
        return handle_error("file too large to read into memory: "s +
                            path.value);
      },
#if QLJS_HAVE_WINDOWS_H
      [handle_error](e_api_read_file,
                     const boost::leaf::windows::e_LastError &error,
                     const boost::leaf::e_file_name &path) {
        return handle_error("failed to read from "s + path.value + ": "s +
                            error_message(error));
      },
#endif
#if QLJS_HAVE_UNISTD_H
      [handle_error](e_api_read_file, const boost::leaf::e_errno &error,
                     const boost::leaf::e_file_name &path) {
        return handle_error("failed to read from "s + path.value + ": "s +
                            std::strerror(error.value));
      },
#endif
      [handle_error](e_api_read_file, e_file_too_large) {
        return handle_error("file too large to read into memory"s);
      },
#if QLJS_HAVE_WINDOWS_H
      [handle_error](e_api_read_file,
                     const boost::leaf::windows::e_LastError &error) {
        return handle_error("failed to read from file: "s +
                            error_message(error));
      },
#endif
#if QLJS_HAVE_UNISTD_H
      [handle_error](e_api_read_file, const boost::leaf::e_errno &error) {
        return handle_error("failed to read from file: "s +
                            std::strerror(error.value));
      },
#endif
      [handle_error](e_api_read_file) {
        QLJS_ASSERT(
            false);  // Other catch clauses should have happened instead.
        return handle_error("failed to read from file"s);
      });
}

template <class Result>
auto exit_on_read_file_error_handlers() {
  return make_read_file_error_handlers(
      [](const std::string &message) -> Result {
        std::fprintf(stderr, "error: %s\n", message.c_str());
        std::exit(1);
      });
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
