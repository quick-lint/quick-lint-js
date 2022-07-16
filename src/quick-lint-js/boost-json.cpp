// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(QLJS_BUILDING_QLJS_BOOST_JSON)

#include <boost/json/value.hpp>
#include <quick-lint-js/boost-json.h>

namespace quick_lint_js {
::boost::json::string_view to_boost_string_view(string8_view sv) {
  std::string_view std_sv = to_string_view(sv);
  return ::boost::json::string_view(std_sv.data(), std_sv.size());
}

#if QLJS_HAVE_CHAR8_T
::boost::json::string_view to_boost_string_view(std::string_view sv) {
  return ::boost::json::string_view(sv.data(), sv.size());
}
#endif

std::string_view to_string_view(::boost::json::string_view sv) {
  return std::string_view(sv.data(), sv.size());
}

std::string_view to_string_view(const ::boost::json::string& s) {
  return std::string_view(s.data(), s.size());
}

::boost::json::array* if_array(::boost::json::object& object,
                               ::boost::json::string_view key) {
  if (::boost::json::value* value = object.if_contains(key)) {
    return value->if_array();
  } else {
    return nullptr;
  }
}

bool* if_bool(::boost::json::object& object, ::boost::json::string_view key) {
  if (::boost::json::value* value = object.if_contains(key)) {
    return value->if_bool();
  } else {
    return nullptr;
  }
}

std::int64_t* if_int64(::boost::json::object& object,
                       ::boost::json::string_view key) {
  if (::boost::json::value* value = object.if_contains(key)) {
    return value->if_int64();
  } else {
    return nullptr;
  }
}

::boost::json::string* if_string(::boost::json::object& object,
                                 ::boost::json::string_view key) {
  if (::boost::json::value* value = object.if_contains(key)) {
    return value->if_string();
  } else {
    return nullptr;
  }
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
