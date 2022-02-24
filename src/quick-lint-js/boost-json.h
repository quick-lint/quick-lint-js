// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_BOOST_JSON_H
#define QUICK_LINT_JS_BOOST_JSON_H

#include <boost/json/value.hpp>
#include <cstddef>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/narrow-cast.h>
#include <type_traits>

namespace quick_lint_js {
::boost::json::string_view to_boost_string_view(string8_view sv);
#if QLJS_HAVE_CHAR8_T
::boost::json::string_view to_boost_string_view(std::string_view sv);
#endif

#if !defined(BOOST_JSON_STANDALONE)
std::string_view to_string_view(::boost::json::string_view sv);
#endif
std::string_view to_string_view(const ::boost::json::string &s);

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

::boost::json::array *if_array(::boost::json::object &,
                               ::boost::json::string_view key);
bool *if_bool(::boost::json::object &, ::boost::json::string_view key);
std::int64_t *if_int64(::boost::json::object &, ::boost::json::string_view key);
::boost::json::string *if_string(::boost::json::object &,
                                 ::boost::json::string_view key);
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
