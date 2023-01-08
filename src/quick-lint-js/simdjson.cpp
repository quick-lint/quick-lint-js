// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/simdjson.h>
#include <simdjson.h>

namespace quick_lint_js {
string8_view get_raw_json(::simdjson::ondemand::value& value) {
  ::simdjson::ondemand::json_type type;
  if (value.type().get(type) != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  switch (type) {
  case ::simdjson::ondemand::json_type::boolean:
  case ::simdjson::ondemand::json_type::null:
  case ::simdjson::ondemand::json_type::number:
  case ::simdjson::ondemand::json_type::string:
    return to_string8_view(value.raw_json_token());

  case ::simdjson::ondemand::json_type::array:
  case ::simdjson::ondemand::json_type::object:
    QLJS_UNIMPLEMENTED();
  }
  QLJS_UNREACHABLE();
}

bool get_object(::simdjson::ondemand::object& root, const char* key1,
                const char* key2, ::simdjson::ondemand::object* out) {
  return root[key1][key2].get(*out) == ::simdjson::SUCCESS;
}

bool get_object(::simdjson::simdjson_result<::simdjson::ondemand::value>& root,
                const char* key, ::simdjson::ondemand::object* out) {
  return root[key].get(*out) == ::simdjson::SUCCESS;
}

bool get_array(::simdjson::ondemand::object& root, const char* key1,
               const char* key2, ::simdjson::ondemand::array* out) {
  return root[key1][key2].get(*out) == ::simdjson::SUCCESS;
}

bool get_value(::simdjson::ondemand::object& root, const char* key,
               ::simdjson::ondemand::value* out) {
  return root[key].get(*out) == ::simdjson::SUCCESS;
}

bool get_string(::simdjson::ondemand::object& root, const char* key,
                std::string_view* out) {
  return root[key].get(*out) == ::simdjson::SUCCESS;
}

bool get_string(::simdjson::ondemand::object& root, const char* key1,
                const char* key2, const char* key3, std::string_view* out) {
  return root[key1][key2][key3].get(*out) == ::simdjson::SUCCESS;
}

bool get_string(::simdjson::simdjson_result<::simdjson::ondemand::value>& root,
                const char* key, std::string_view* out) {
  return root[key].get(*out) == ::simdjson::SUCCESS;
}

bool get_string8(::simdjson::ondemand::object& root, const char* key,
                 string8_view* out) {
  std::string_view sv;
  if (!get_string(root, key, &sv)) {
    return false;
  }
  *out = to_string8_view(sv);
  return true;
}

bool get_string8(::simdjson::ondemand::object& root, const char* key1,
                 const char* key2, const char* key3, string8_view* out) {
  std::string_view sv;
  if (!get_string(root, key1, key2, key3, &sv)) {
    return false;
  }
  *out = to_string8_view(sv);
  return true;
}

bool get_string8(::simdjson::simdjson_result<::simdjson::ondemand::value>& root,
                 const char* key, string8_view* out) {
  std::string_view sv;
  if (!get_string(root, key, &sv)) {
    return false;
  }
  *out = to_string8_view(sv);
  return true;
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
