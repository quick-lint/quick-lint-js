// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/uri.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
// Returns an empty string on parse failure.
std::string parse_file_from_lsp_uri(std::string_view uri) {
#if defined(_WIN32)
  return parse_file_from_lsp_uri_win32(uri);
#else
  return parse_file_from_lsp_uri_posix(uri);
#endif
}

std::string parse_file_from_lsp_uri_posix(std::string_view uri) {
  if (!starts_with(uri, "file://")) {
    return "";
  }
  if (uri.size() < std::strlen("file://") + 1) {
    return "";
  }
  bool have_authority = uri[7] != '/';
  if (have_authority) {
    uri = uri.substr(std::strlen("file:"));
  } else {
    uri = uri.substr(std::strlen("file://"));
  }
  std::size_t query_start = uri.find('?');
  if (query_start != uri.npos) {
    uri = uri.substr(0, query_start);
  }
  std::size_t fragment_start = uri.find('#');
  if (fragment_start != uri.npos) {
    uri = uri.substr(0, fragment_start);
  }

  std::string result;
  std::size_t percent_index;
  while ((percent_index = uri.find('%')) != uri.npos) {
    result.append(uri.substr(0, percent_index));

    uri = uri.substr(percent_index + 1);
    if (uri.size() < 2) {
      return "";
    }
    unsigned char c;
    from_chars_result parse_result =
        from_chars_hex(uri.data(), uri.data() + 2, c);
    if (parse_result.ptr != uri.data() + 2) {
      return "";
    }
    QLJS_ASSERT(parse_result.ec == std::errc());
    result.push_back(static_cast<char>(c));
    uri = uri.substr(2);
  }
  result.append(uri);
  return result;
}

std::string parse_file_from_lsp_uri_win32(std::string_view uri) {
  std::string result = parse_file_from_lsp_uri_posix(uri);
  if (result.empty()) {
    return result;
  }

  for (char& c : result) {
    if (c == '/') {
      c = '\\';
    }
  }

  auto is_drive_letter = [](char c) -> bool { return isalpha(c); };
  bool is_drive = result.size() >= 3 && result[0] == '\\' &&
                  is_drive_letter(result[1]) && result[2] == ':' &&
                  (result.size() == 3 || result[3] == '\\');
  if (is_drive) {
    result.erase(result.begin(), result.begin() + 1);
  }

  return result;
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
