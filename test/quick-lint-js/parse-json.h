// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PARSE_JSON_H
#define QUICK_LINT_JS_PARSE_JSON_H

#include <boost/json/value.hpp>
#include <iosfwd>
#include <quick-lint-js/boost-json.h>
#include <quick-lint-js/char8.h>
#include <simdjson.h>
#include <string>
#include <utility>

namespace quick_lint_js {
class byte_buffer;

::boost::json::value parse_boost_json(std::string_view);
#if QLJS_HAVE_CHAR8_T
::boost::json::value parse_boost_json(string8_view);
#endif
::boost::json::value parse_boost_json(const byte_buffer &);

::boost::json::value simdjson_to_boost_json(::simdjson::ondemand::value &);
::boost::json::value simdjson_to_boost_json(
    ::simdjson::simdjson_result<::simdjson::ondemand::value> &&);

string8 json_to_string8(const ::boost::json::value &);
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
