// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/json/parse.hpp>
#include <boost/json/serialize.hpp>
#include <boost/json/value.hpp>
#include <cstdint>
#include <gtest/gtest.h>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/parse-json.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <simdjson.h>
#include <sstream>

namespace quick_lint_js {
::boost::json::value parse_boost_json(std::string_view json) {
  ::boost::json::error_code error;
  ::boost::json::value root =
      ::boost::json::parse(to_boost_string_view(json), error);
  EXPECT_FALSE(error) << json;
  return root;
}

#if QLJS_HAVE_CHAR8_T
::boost::json::value parse_boost_json(string8_view json) {
  return parse_boost_json(to_string_view(json));
}
#endif

::boost::json::value parse_boost_json(const byte_buffer &message) {
  string8 message_json;
  message_json.resize(message.size());
  message.copy_to(message_json.data());
  return parse_boost_json(message_json);
}

::boost::json::value simdjson_to_boost_json(
    ::simdjson::ondemand::value &value) {
  std::string_view json = value.raw_json_token();
  ::boost::json::error_code error;
  ::boost::json::value result =
      ::boost::json::parse(to_boost_string_view(json), error);
  EXPECT_FALSE(error);
  return result;
}

::boost::json::value simdjson_to_boost_json(
    ::simdjson::simdjson_result<::simdjson::ondemand::value> &&value) {
  ::simdjson::ondemand::value unwrapped_value;
  ::simdjson::error_code error = value.get(unwrapped_value);
  if (error != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  return simdjson_to_boost_json(unwrapped_value);
}

string8 json_to_string8(const ::boost::json::value &v) {
  return to_string8(::boost::json::serialize(v));
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
