// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PARSE_JSON_H
#define QUICK_LINT_JS_PARSE_JSON_H

#include <boost/json/value.hpp>
#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/narrow-cast.h>
#include <simdjson.h>
#include <string>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
::boost::json::value parse_boost_json(std::string_view);
#if QLJS_HAVE_CHAR8_T
::boost::json::value parse_boost_json(string8_view);
#endif

::boost::json::value simdjson_to_boost_json(::simdjson::ondemand::value &);
::boost::json::value simdjson_to_boost_json(
    ::simdjson::simdjson_result<::simdjson::ondemand::value> &&);

template <class... Keys>
struct look_up_impl;

template <class Key, class... OtherKeys>
struct look_up_impl<Key, OtherKeys...> {
  static ::boost::json::value look_up(const ::boost::json::value &root,
                                      Key &&key, OtherKeys &&... other_keys) {
    if constexpr (std::is_integral_v<Key>) {
      return look_up_impl<OtherKeys...>::look_up(
          root.as_array().at(narrow_cast<std::size_t>(key)),
          std::forward<OtherKeys>(other_keys)...);
    } else {
      return look_up_impl<OtherKeys...>::look_up(
          root.as_object().at(key), std::forward<OtherKeys>(other_keys)...);
    }
  }
};

template <>
struct look_up_impl<> {
  static ::boost::json::value look_up(const ::boost::json::value &root) {
    return root;
  }
};

template <class... Keys>
::boost::json::value look_up(const ::boost::json::value &root,
                             Keys &&... keys) {
  return look_up_impl<Keys...>::look_up(root, std::forward<Keys>(keys)...);
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
