// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_SUPPORT_H
#define QUICK_LINT_JS_LSP_SUPPORT_H

#include <boost/json/value.hpp>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/boost-json.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/port/char8.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
inline string8 make_message(string8_view content) {
  return concat(u8"Content-Length: "_sv,
                to_string8_view(std::to_string(content.size())),
                u8"\r\n\r\n"_sv, content);
}

inline void expect_error(::boost::json::object& response, int error_code,
                         std::string_view error_message) {
  EXPECT_FALSE(response.contains("method"));
  EXPECT_EQ(look_up(response, "jsonrpc"), "2.0");
  EXPECT_FALSE(look_up(response).as_object().contains("result"));
  EXPECT_EQ(look_up(response, "error", "code"), error_code);
  // HACK(strager): std::string casts are necessary for older Google Test (e.g.
  // version 1.10.0).
  EXPECT_THAT(std::string(boost::json::string_view(
                  look_up(response, "error", "message").as_string())),
              ::testing::HasSubstr(std::string(error_message)));
}

inline void expect_error(::boost::json::value& response, int error_code,
                         std::string_view error_message) {
  expect_error(response.as_object(), error_code, error_message);
}

inline void expect_parse_error(::boost::json::value& message) {
  expect_error(message, -32700, "Parse error");
  EXPECT_EQ(look_up(message, "id"), ::boost::json::value());
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
