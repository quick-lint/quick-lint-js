// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <gtest/gtest.h>
#include <ostream>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/tjson.h>
#include <simdjson.h>
#include <utility>

namespace quick_lint_js {
struct TJSON_Value::Impl {
  explicit Impl(TJSON::Impl* tjson_impl,
                ::simdjson::simdjson_result<::simdjson::dom::element> value)
      : tjson_impl(tjson_impl), value(value) {}

  TJSON::Impl* tjson_impl;
  ::simdjson::simdjson_result<::simdjson::dom::element> value;
};

struct TJSON::Impl {
  Monotonic_Allocator allocator{"TJSON"};

  ::simdjson::dom::parser parser;
  TJSON_Value root;

  Linked_Vector<TJSON_Value::Impl> values{&this->allocator};

  TJSON_Value make_value(
      ::simdjson::simdjson_result<::simdjson::dom::element>&& value) {
    TJSON_Value::Impl* value_impl =
        &this->values.emplace_back(this, std::move(value));
    return TJSON_Value(value_impl);
  }
};

TJSON::TJSON(String8_View json) {
  this->impl_ = new Impl();
  this->impl_->root = this->impl_->make_value(this->impl_->parser.parse(
      reinterpret_cast<const std::uint8_t*>(json.data()), json.size()));
  EXPECT_EQ(this->impl_->root.impl_->value.error(), ::simdjson::SUCCESS)
      << "parsing JSON failed: " << out_string8(json);
}

TJSON::TJSON(const Byte_Buffer& json) : TJSON(json.to_string8()) {}

TJSON::TJSON(TJSON&& other) : impl_(std::exchange(other.impl_, nullptr)) {}

TJSON::~TJSON() { delete this->impl_; }

TJSON_Value TJSON::operator[](String8_View object_key) const {
  return this->impl_->root[object_key];
}

TJSON_Value TJSON::operator[](std::size_t index) const {
  return this->impl_->root[index];
}

std::size_t TJSON::size() const { return this->impl_->root.size(); }

TJSON_Value TJSON::root() const { return this->impl_->root; }

TJSON_Value::TJSON_Value() : TJSON_Value(/*impl=*/nullptr) {}

TJSON_Value::TJSON_Value(Impl* impl) : impl_(impl) {}

String8_View TJSON_Value::to_string() const {
  ::simdjson::dom::element value;
  ::simdjson::error_code error = this->impl_->value.get(value);
  if (error != ::simdjson::SUCCESS) {
    // TODO(strager): Print the error and the JSON pointer.
    return u8"(error)"_sv;
  }
  std::string s = ::simdjson::minify(value);

  Monotonic_Allocator& allocator = this->impl_->tjson_impl->allocator;
  Span<char> out = allocator.allocate_uninitialized_span<char>(s.size());
  std::copy(s.begin(), s.end(), out.begin());
  return String8_View(reinterpret_cast<const Char8*>(out.data()),
                      narrow_cast<std::size_t>(out.size()));
}

TJSON_Value TJSON_Value::operator[](String8_View object_key) const {
  // FIXME(strager): simdjson's operator[] does not handle escape sequences
  // properly.
  return this->impl_->tjson_impl->make_value(
      this->impl_->value[to_string_view(object_key)]);
}

TJSON_Value TJSON_Value::operator[](std::size_t index) const {
  return this->impl_->tjson_impl->make_value(this->impl_->value.at(index));
}

std::optional<Span<const TJSON_Value>> TJSON_Value::try_get_array() const {
  TJSON::Impl* tjson_impl = this->impl_->tjson_impl;
  Bump_Vector<TJSON_Value, Monotonic_Allocator> items(
      "TJSON_Value::try_get_array items", &tjson_impl->allocator);
  ::simdjson::dom::array array;
  if (this->impl_->value.get(array) != ::simdjson::SUCCESS) {
    return std::nullopt;
  }
  for (::simdjson::dom::element item : array) {
    items.push_back(tjson_impl->make_value(std::move(item)));
  }
  return Span<const TJSON_Value>(items);
}

Span<const TJSON_Value> TJSON_Value::get_array_or_empty() const {
  return this->try_get_array().value_or(Span<const TJSON_Value>());
}

std::optional<String8_View> TJSON_Value::try_get_string() const {
  std::string_view string;
  if (this->impl_->value.get(string) != ::simdjson::SUCCESS) {
    return std::nullopt;
  }
  return to_string8_view(string);
}

std::size_t TJSON_Value::size() const {
  ::simdjson::dom::array array;
  if (this->impl_->value.get(array) == ::simdjson::SUCCESS) {
    return array.size();
  }
  ::simdjson::dom::object object;
  if (this->impl_->value.get(object) == ::simdjson::SUCCESS) {
    return object.size();
  }
  return TJSON::invalid_size;
}

bool TJSON_Value::exists() const {
  return this->impl_->value.error() == ::simdjson::SUCCESS;
}

bool TJSON_Value::is_array() const { return this->impl_->value.is_array(); }

bool TJSON_Value::is_object() const { return this->impl_->value.is_object(); }

bool operator==(TJSON_Value lhs, int rhs) {
  std::int64_t value_signed;
  if (lhs.impl_->value.get(value_signed) == ::simdjson::SUCCESS) {
    return value_signed == rhs;
  }
  // TODO(strager): Check std::uint64_t?
  return false;
}

bool operator==(TJSON_Value lhs, long rhs) {
  std::int64_t value_signed;
  if (lhs.impl_->value.get(value_signed) == ::simdjson::SUCCESS) {
    return value_signed == rhs;
  }
  // TODO(strager): Check std::uint64_t?
  return false;
}

bool operator==(TJSON_Value lhs, long long rhs) {
  std::int64_t value_signed;
  if (lhs.impl_->value.get(value_signed) == ::simdjson::SUCCESS) {
    return value_signed == rhs;
  }
  // TODO(strager): Check std::uint64_t?
  return false;
}

bool operator==(TJSON_Value lhs, bool rhs) {
  bool value;
  return lhs.impl_->value.get(value) == ::simdjson::SUCCESS && value == rhs;
}

bool operator==(TJSON_Value lhs, String8_View rhs) {
  std::string_view value;
  return lhs.impl_->value.get(value) == ::simdjson::SUCCESS &&
         value == to_string_view(rhs);
}

bool operator==(TJSON_Value lhs, std::nullptr_t) {
  return lhs.impl_->value.is_null();
}

#define QLJS_OPERATOR_NE(RHS_Type) \
  bool operator!=(TJSON_Value lhs, RHS_Type rhs) { return !(lhs == rhs); }
QLJS_OPERATOR_NE(int)
QLJS_OPERATOR_NE(long)
QLJS_OPERATOR_NE(long long)
QLJS_OPERATOR_NE(bool)
QLJS_OPERATOR_NE(String8_View)
QLJS_OPERATOR_NE(std::nullptr_t)
#undef QLJS_OPERATOR_NE
}

namespace testing::internal {
template <>
void PrintTo(const quick_lint_js::TJSON_Value& value, std::ostream* out) {
  *out << quick_lint_js::out_string8(value.to_string());
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
