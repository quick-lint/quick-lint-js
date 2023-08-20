// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <gtest/gtest.h>
#include <optional>
#include <ostream>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>

namespace quick_lint_js {
class Byte_Buffer;
class TJSON_Value;

// TJSON is a test-friendly DOM-style JSON parser.
//
// TJSON is designed for testing:
//
// * fast compile times
// * ergonomic API
// * error-forgiving API
// * mediocre performance and memory usage
//
// TJSON instances own the memory of TJSON_Value objects.
class TJSON {
 public:
  static constexpr std::size_t invalid_size = static_cast<std::size_t>(-1);

  explicit TJSON(String8_View json);
  explicit TJSON(const Byte_Buffer &json);

  TJSON(const TJSON &) = delete;
  TJSON &operator=(const TJSON &) = delete;

  TJSON(TJSON &&);
  TJSON &operator=(TJSON &&) = delete;  // TODO(strager)

  ~TJSON();

  // Look up a key in an object.
  //
  // If this is not an object, returns an erroring TJSON_Value.
  //
  // This TJSON must outlive the returned TJSON_Value.
  TJSON_Value operator[](String8_View object_key) const;

  // Look up an item in an array.
  //
  // If this is not an array, returns an erroring TJSON_Value.
  //
  // This TJSON must outlive the returned TJSON_Value.
  TJSON_Value operator[](std::size_t index) const;

  // The number of key-value pairs in an object, or the number of items in an
  // array, or invalid_size.
  std::size_t size() const;

  TJSON_Value root() const;

 private:
  struct Impl;

  Impl *impl_;

  friend TJSON_Value;
};

// TJSON_Value instances are non-owning. A TJSON instance owns the memory.
class TJSON_Value {
 public:
  String8_View to_string() const;

  // Look up a key in an object.
  //
  // If this is not an object, returns an erroring TJSON_Value.
  TJSON_Value operator[](String8_View object_key) const;

  // Look up an item in an array.
  //
  // If this is not an array, returns an erroring TJSON_Value.
  TJSON_Value operator[](std::size_t index) const;

  // If this is not an array, return nullopt.
  std::optional<Span<const TJSON_Value>> try_get_array() const;

  // If this is not an array, return an empty span.
  Span<const TJSON_Value> get_array_or_empty() const;

  // If this is not a string, return nullopt.
  std::optional<String8_View> try_get_string() const;

  // The number of key-value pairs in an object, or the number of items in an
  // array, or TJSON::invalid_size.
  std::size_t size() const;

  bool exists() const;

  bool is_array() const;
  bool is_object() const;

  friend bool operator==(TJSON_Value, int);
  friend bool operator!=(TJSON_Value, int);

  friend bool operator==(TJSON_Value, long);
  friend bool operator!=(TJSON_Value, long);

  friend bool operator==(TJSON_Value, long long);
  friend bool operator!=(TJSON_Value, long long);

  friend bool operator==(TJSON_Value, bool);
  friend bool operator!=(TJSON_Value, bool);

  friend bool operator==(TJSON_Value, String8_View);
  friend bool operator!=(TJSON_Value, String8_View);

  friend bool operator==(TJSON_Value, std::nullptr_t);
  friend bool operator!=(TJSON_Value, std::nullptr_t);

  friend bool operator==(TJSON_Value, const char *) = delete;
  friend bool operator!=(TJSON_Value, const char *) = delete;

 private:
  struct Impl;

  explicit TJSON_Value();
  explicit TJSON_Value(Impl *);

  Impl *impl_;

  friend TJSON;
};
}

namespace testing::internal {
template <>
void PrintTo(const quick_lint_js::TJSON_Value &, std::ostream *);
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
