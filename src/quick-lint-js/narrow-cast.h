// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_NARROW_CAST_H
#define QUICK_LINT_JS_NARROW_CAST_H

#include <limits>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/source-location.h>
#include <type_traits>

namespace quick_lint_js {
template <class T>
struct make_unsigned : public std::make_unsigned<T> {};

#if QLJS_HAVE_CHAR8_T
// HACK(strager): Work around older versions of libc++ not supporting
// std::make_unsigned<char8_t> despite the corresponding versions of Clang
// supporting char8_t.
template <>
struct make_unsigned<char8_t> {
  using type = char8_t;
};
#endif

template <class T>
using make_unsigned_t = typename make_unsigned<T>::type;

template <class T>
struct numeric_limits : public std::numeric_limits<T> {};

#if QLJS_HAVE_CHAR8_T
// HACK(strager): Work around older versions of libc++ not supporting
// std::numeric_limits<char8_t> despite the corresponding versions of Clang
// supporting char8_t.
template <>
struct numeric_limits<char8_t> {
  static constexpr char8_t lowest() noexcept {
    return static_cast<char8_t>(uchar_limits::lowest());
  }

  static constexpr char8_t(max)() noexcept {
    return static_cast<char8_t>((uchar_limits::max)());
  }

 private:
  using uchar_limits = numeric_limits<unsigned char>;
};
#endif

// TODO(strager): Use std::in_range if supported.
template <class Out, class In>
constexpr bool in_range([[maybe_unused]] In x) noexcept {
  using out_limits = numeric_limits<Out>;
  using unsigned_in = make_unsigned_t<In>;
  using unsigned_out = make_unsigned_t<Out>;
  if constexpr (std::is_same_v<In, Out>) {
    return true;
  } else if constexpr (std::is_signed_v<In> && std::is_signed_v<Out>) {
    return out_limits::lowest() <= x && x <= (out_limits::max)();
  } else if constexpr (std::is_signed_v<In> && !std::is_signed_v<Out>) {
    return 0 <= x && static_cast<unsigned_in>(x) <= (out_limits::max)();
  } else if constexpr (!std::is_signed_v<In> && std::is_signed_v<Out>) {
    return x <= unsigned_out{(out_limits::max)()};
  } else if constexpr (!std::is_signed_v<In> && !std::is_signed_v<Out>) {
    return x <= (out_limits::max)();
  }
}

template <class Out, class In>
Out narrow_cast(In x
#if !(defined(NDEBUG) && NDEBUG)
                ,
                source_location caller = source_location::current()
#endif
                    ) noexcept {
#if !(defined(NDEBUG) && NDEBUG)
  if (!in_range<Out>(x)) {
    if constexpr (source_location::valid()) {
      report_assertion_failure(caller.file_name(),
                               static_cast<int>(caller.line()),
                               caller.function_name(), "number not in range");
    } else {
      report_assertion_failure(__FILE__, __LINE__, __func__,
                               "number not in range");
    }
    QLJS_ASSERT_TRAP();
  }
#endif
  return static_cast<Out>(x);
}
}

#endif

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
