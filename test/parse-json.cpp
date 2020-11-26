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

#include <cstdint>
#include <gtest/gtest.h>
#include <json/reader.h>
#include <json/value.h>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/parse-json.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <simdjson.h>
#include <sstream>

namespace quick_lint_js {
::Json::Value parse_json(std::stringstream &stream) {
  SCOPED_TRACE(stream.str());
  stream.seekg(0);
  ::Json::Value root;
  ::Json::CharReaderBuilder builder;
  builder.strictMode(&builder.settings_);
  ::Json::String errors;
  bool ok = ::Json::parseFromStream(builder, stream, &root, &errors);
  EXPECT_TRUE(ok) << errors;
  return root;
}

::Json::Value parse_json(const std::string &json) {
  ::Json::Value result;
  ::Json::String errors;
  bool ok = parse_json(json, &result, &errors);
  EXPECT_TRUE(ok) << errors;
  return result;
}

bool parse_json(std::string_view json, ::Json::Value *result,
                ::Json::String *errors) {
  ::Json::CharReaderBuilder readerBuilder;
  readerBuilder.strictMode(&readerBuilder.settings_);
  std::unique_ptr<::Json::CharReader> reader(readerBuilder.newCharReader());
  const char *json_chars = json.data();
  bool ok = reader->parse(json_chars, &json_chars[json.size()], result, errors);
  return ok;
}

#if QLJS_HAVE_CHAR8_T
bool parse_json(string8_view json, ::Json::Value *result,
                ::Json::String *errors) {
  return parse_json(reinterpret_cast<const char *>(json.data()), result,
                    errors);
}
#endif

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
::Json::Value simdjson_to_jsoncpp(const ::simdjson::dom::element &value) {
  switch (value.type()) {
  case ::simdjson::dom::element_type::INT64: {
    std::int64_t data;
    ::simdjson::error_code error = value.get(data);
    QLJS_ASSERT(error == ::simdjson::error_code::SUCCESS);
    return ::Json::Value(data);
  }

  case ::simdjson::dom::element_type::UINT64: {
    std::uint64_t data;
    ::simdjson::error_code error = value.get(data);
    QLJS_ASSERT(error == ::simdjson::error_code::SUCCESS);
    return ::Json::Value(data);
  }

  case ::simdjson::dom::element_type::STRING: {
    std::string_view data;
    ::simdjson::error_code error = value.get(data);
    QLJS_ASSERT(error == ::simdjson::error_code::SUCCESS);
    return ::Json::Value(data.data(), data.data() + data.size());
  }

  case ::simdjson::dom::element_type::ARRAY:
  case ::simdjson::dom::element_type::BOOL:
  case ::simdjson::dom::element_type::DOUBLE:
  case ::simdjson::dom::element_type::NULL_VALUE:
  case ::simdjson::dom::element_type::OBJECT:
    QLJS_UNIMPLEMENTED();
    break;
  }

  QLJS_UNREACHABLE();
}
QLJS_WARNING_POP

::Json::Value simdjson_to_jsoncpp(
    ::simdjson::simdjson_result<::simdjson::dom::element> &&value) {
  ::simdjson::dom::element unwrapped_value;
  ::simdjson::error_code error = value.get(unwrapped_value);
  if (error != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  return simdjson_to_jsoncpp(unwrapped_value);
}
}
