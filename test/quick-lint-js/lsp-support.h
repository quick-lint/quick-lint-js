// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_SUPPORT_H
#define QUICK_LINT_JS_LSP_SUPPORT_H

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/tjson.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
inline String8 make_message(String8_View content) {
  return concat(u8"Content-Length: "_sv,
                to_string8_view(std::to_string(content.size())),
                u8"\r\n\r\n"_sv, content);
}

inline void expect_error(const TJSON_Value& response, int error_code,
                         std::string_view error_message) {
  EXPECT_FALSE(response[u8"method"_sv].exists());
  EXPECT_EQ(response[u8"jsonrpc"_sv], u8"2.0"_sv);
  EXPECT_FALSE(response[u8"result"_sv].exists());
  EXPECT_EQ(response[u8"error"_sv][u8"code"_sv], error_code);
  // HACK(strager): std::string casts are necessary for older Google Test (e.g.
  // version 1.10.0).
  EXPECT_THAT(
      std::string(to_string_view(
          response[u8"error"_sv][u8"message"_sv].try_get_string().value())),
      ::testing::HasSubstr(std::string(error_message)));
}

inline void expect_parse_error(const TJSON_Value& message) {
  expect_error(message, -32700, "Parse error");
  EXPECT_EQ(message[u8"id"_sv], nullptr);
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
