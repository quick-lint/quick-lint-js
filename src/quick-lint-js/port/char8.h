// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_CHAR8_H
#define QUICK_LINT_JS_PORT_CHAR8_H

#include <algorithm>
#include <cstddef>
#include <iosfwd>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/port/have.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
#if QLJS_HAVE_CHAR8_T
using char8 = char8_t;
#else
using char8 = char;
#endif

// Alias std::u8string or std::string.
using string8 = std::basic_string<char8>;

std::size_t strlen(const char8 *);
int strlen_i(const char8 *);
const char8 *strchr(const char8 *haystack, char8 needle);
const char8 *strstr(const char8 *haystack, const char8 *needle);
std::size_t strspn(const char8 *haystack, const char8 *needles);

// Like std::u8string_view.
// TODO(strager): Move this into its own file.
class string8_view {
 public:
  using value_type = char8;
  using size_type = std::size_t;  // TODO(strager): Switch to a signed type.
  using iterator = const char8 *;

  inline static constexpr size_type npos = static_cast<size_type>(-1);

  static string8_view from_c_str(const char8 *data) noexcept {
    return string8_view(data, strlen(data));
  }

  /*implicit*/ string8_view() noexcept : begin_(nullptr), end_(this->begin_) {}

  string8_view(const string8 &s) noexcept
      : begin_(s.c_str()), end_(s.c_str() + s.size()) {}

  // Do not call. Use operator""_sv or call string8_view::from_c_str explicitly.
  string8_view(const char8 *) = delete;

  explicit constexpr string8_view(const char8 *data, size_type size) noexcept
      : string8_view(data, data + size) {}

  explicit constexpr string8_view(const char8 *begin, const char8 *end) noexcept
      : begin_(begin), end_(end) {}

  constexpr const char8 *data() const noexcept { return this->begin_; }
  constexpr size_type size() const noexcept {
    return static_cast<size_type>(this->end_ - this->begin_);
  }
  constexpr bool empty() const noexcept { return this->begin_ == this->end_; }

  constexpr const char8 *begin() const noexcept { return this->begin_; }
  constexpr const char8 *end() const noexcept { return this->end_; }

  const char8 &operator[](size_type index) const noexcept {
    QLJS_ASSERT(index < this->size());
    return this->begin_[index];
  }

  size_type find(char8 needle) const noexcept {
    return this->find(needle, /*begin_index=*/0);
  }

  size_type find(char8 needle, size_type begin_index) const noexcept {
    const char8 *it = std::find(this->begin_ + begin_index, this->end_, needle);
    if (it == this->end_) {
      return npos;
    }
    return static_cast<size_type>(it - this->begin_);
  }

  size_type find(string8_view needle) const noexcept {
    const char8 *it =
        std::search(this->begin_, this->end_, needle.begin_, needle.end_);
    if (it == this->end_) {
      return npos;
    }
    return static_cast<size_type>(it - this->begin_);
  }

  size_type find_first_not_of(string8_view needles) const noexcept {
    if (this->empty()) {
      return npos;
    }
    const char8 *c = this->begin_;
    for (;;) {
      if (!needles.contains(*c)) {
        return static_cast<size_type>(c - this->begin_);
      }
      ++c;
      if (c == this->end_) {
        return npos;
      }
    }
  }

  size_type find_last_not_of(string8_view needles) const noexcept {
    if (this->empty()) {
      return npos;
    }
    const char8 *c = this->end_ - 1;
    for (;;) {
      if (!needles.contains(*c)) {
        return static_cast<size_type>(c - this->begin_);
      }
      if (c == this->begin_) {
        return npos;
      }
      --c;
    }
  }

  template <class Predicate>
  size_type find_first_if(Predicate &&predicate) const {
    for (size_type i = 0; i < this->size(); ++i) {
      if (predicate((*this)[i])) {
        return i;
      }
    }
    return npos;
  }

  bool contains(char8 needle) const noexcept {
    return this->find(needle) != npos;
  }

  bool starts_with(string8_view prefix) const noexcept {
    if (prefix.size() > this->size()) {
      return false;
    }
    return this->substr(0, prefix.size()) == prefix;
  }

  string8_view substr(size_type begin_index) const noexcept {
    QLJS_ASSERT(begin_index <= this->size());
    return string8_view(this->begin_ + begin_index, this->end_);
  }

  // NOTE(strager): Unlike std::basic_string_view<>::substr, our
  // implementation does not support count==npos.
  string8_view substr(size_type begin_index, size_type count) const noexcept {
    QLJS_ASSERT(begin_index <= this->size());
    QLJS_ASSERT(count <= this->size());
    QLJS_ASSERT(begin_index <= this->size() - count);
    return string8_view(this->begin_ + begin_index, count);
  }

  explicit operator string8() const {
    return string8(this->begin_, this->end_);
  }

  friend string8 &operator+=(string8 &lhs, string8_view rhs) {
    lhs.append(rhs.begin_, rhs.end_);
    return lhs;
  }

  friend bool operator==(string8_view lhs, string8_view rhs) noexcept {
    return std::equal(lhs.begin_, lhs.end_, rhs.begin_, rhs.end_);
  }
  friend bool operator!=(string8_view lhs, string8_view rhs) noexcept {
    return !(lhs == rhs);
  }

  friend bool operator==(string8_view lhs, const string8 &rhs) noexcept {
    return lhs == string8_view(rhs);
  }
  friend bool operator!=(string8_view lhs, const string8 &rhs) noexcept {
    return !(lhs == rhs);
  }

  friend bool operator==(const string8 &lhs, string8_view rhs) noexcept {
    return string8_view(lhs) == rhs;
  }
  friend bool operator!=(const string8 &lhs, string8_view rhs) noexcept {
    return !(lhs == rhs);
  }

 private:
  const char8 *begin_;
  const char8 *end_;
};

// TODO(strager): Simplify this code.
class streamable_string8_view {
 public:
  friend std::ostream &operator<<(std::ostream &, streamable_string8_view);

 private:
  explicit streamable_string8_view(string8_view) noexcept;

  friend streamable_string8_view out_string8(string8_view) noexcept;

  string8_view sv_;
};

streamable_string8_view out_string8(const char8 *) noexcept;
streamable_string8_view out_string8(string8_view) noexcept;
streamable_string8_view out_string8(const string8 &) noexcept;

string8 to_string8(const std::string &);
string8 to_string8(std::string_view);
std::string to_string(const string8_view &);

std::string_view to_string_view(string8_view);
string8_view to_string8_view(std::string_view);

// The following functions treat ASCII A-Z as upper, a-z as lower, and all other
// code units as neither upper nor lower.
char8 toupper(char8) noexcept;
char8 tolower(char8) noexcept;
bool islower(char8) noexcept;
bool isupper(char8) noexcept;

bool haslower(string8_view);
bool hasupper(string8_view);

inline constexpr string8_view operator""_sv(const char8 *string,
                                            std::size_t length) noexcept {
  return string8_view(string, length);
}

#if QLJS_HAVE_CHAR8_T
inline string8_view operator""_s8v(const char *string,
                                   std::size_t length) noexcept {
  return string8_view(reinterpret_cast<const char8 *>(string), length);
}
#else
inline string8_view operator""_s8v(const char *string,
                                   std::size_t length) noexcept {
  return string8_view(string, length);
}
#endif
}

namespace testing::internal {
#if QLJS_HAVE_CHAR8_T
template <class T>
void PrintTo(const T &, std::ostream *);
template <>
void PrintTo(const char8_t &, std::ostream *);
template <>
void PrintTo(const char8_t *const &, std::ostream *);
template <>
void PrintTo(char8_t *const &, std::ostream *);
#endif
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
