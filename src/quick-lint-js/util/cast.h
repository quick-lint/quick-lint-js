// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/in-range.h>
#include <quick-lint-js/port/source-location.h>
#include <type_traits>

namespace quick_lint_js {
template <class Out, class In>
Out narrow_cast(In x
#if !(defined(NDEBUG) && NDEBUG)
                ,
                Source_Location caller = Source_Location::current()
#endif
) {
#if !(defined(NDEBUG) && NDEBUG)
  if (!in_range<Out>(x)) {
    if constexpr (Source_Location::valid()) {
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

// Convert an integer to an enum.
//
// Example:
//
//   enum class Color : unsigned char { red, green, blue };
//   unsigned char raw = 1;                   // green
//   Color c = int_to_enum_cast<Color>(raw);  // Color::green
template <class Enum>
constexpr Enum int_to_enum_cast(std::underlying_type_t<Enum> value) {
  return static_cast<Enum>(value);
}

// Convert an enum to its integer value.
//
// Example:
//
//   enum class Color : unsigned char { red, green, blue };
//   unsigned char raw = enum_to_int_cast(Color::green);     // 1
template <class Enum>
constexpr std::underlying_type_t<Enum> enum_to_int_cast(Enum value) {
  return static_cast<std::underlying_type_t<Enum>>(value);
}

// Cast a pointer or reference to a pointer/reference to a derived class.
//
// In the presence of multiple inheritance, perform pointer adjustments (like
// what static_cast does).
template <class Derived_Pointer, class Base>
Derived_Pointer derived_cast(Base* base) {
  using Derived = std::remove_pointer_t<Derived_Pointer>;
  static_assert(std::is_base_of_v<Base, Derived>,
                "Derived should derive from Base");
  static_assert(!std::is_base_of_v<Derived, Base>,
                "Derived should not be the same type as Base");
  return static_cast<Derived_Pointer>(base);
}
template <class Derived_Reference, class Base>
Derived_Reference derived_cast(Base& base) {
  using Derived = std::remove_reference_t<Derived_Reference>;
  static_assert(std::is_base_of_v<Base, Derived>,
                "Derived should derive from Base");
  static_assert(!std::is_base_of_v<Derived, Base>,
                "Derived should not be the same type as Base");
  return static_cast<Derived_Reference>(base);
}

// Cast a pointer or reference to a pointer/reference to a base class.
//
// In the presence of multiple inheritance, perform pointer adjustments (like
// what static_cast does).
template <class Base_Pointer, class Derived>
Base_Pointer base_cast(Derived* base) {
  using Base = std::remove_pointer_t<Base_Pointer>;
  static_assert(std::is_base_of_v<Base, Derived>,
                "Derived should derive from Base");
  static_assert(!std::is_base_of_v<Derived, Base>,
                "Derived should not be the same type as Base");
  return base;
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
