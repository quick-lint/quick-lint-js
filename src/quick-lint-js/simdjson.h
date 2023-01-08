// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SIMDJSON_H
#define QUICK_LINT_JS_SIMDJSON_H

#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/simdjson-fwd.h>

namespace quick_lint_js {
string8_view get_raw_json(::simdjson::ondemand::value&);

// Returns true on success.
//
// Returns false if root is not an object, or if root does not contain key, or
// if root[key] is not an object.
bool get_object(::simdjson::simdjson_result<::simdjson::ondemand::value>& root,
                const char* key, ::simdjson::ondemand::object* out);

// Returns true on success.
//
// Returns false if root does not contain key1, or if root[key1] does not
// contain key2, or if root[key1][key2] is not an object.
bool get_object(::simdjson::ondemand::object& root, const char* key1,
                const char* key2, ::simdjson::ondemand::object* out);

bool get_array(::simdjson::ondemand::object& root, const char* key1,
               const char* key2, ::simdjson::ondemand::array* out);

bool get_value(::simdjson::ondemand::object& root, const char* key,
               ::simdjson::ondemand::value* out);

bool get_string(::simdjson::ondemand::object& root, const char* key,
                std::string_view* out);
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
