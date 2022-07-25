// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_ALGORITHM_H
#define QUICK_LINT_JS_UTIL_ALGORITHM_H

#include <algorithm>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <type_traits>

namespace quick_lint_js {
// An alias for std::find which documents intent.
template <class It, class T>
It find_first(It begin, It end, const T& needle) {
  return std::find(begin, end, needle);
}

// Like std::find, but the element must exist.
//
// Precondition: std::find(begin, end, needle) != end
// Postcondition: result != end
template <class It, class T>
It find_existing(It begin, It end, const T& needle) {
  It result = std::find(begin, end, needle);
  QLJS_ASSERT(result != end);
  return result;
}

template <class Range, class T>
auto find_existing(Range&& haystack, const T& needle) {
  return find_existing(std::begin(haystack), std::end(haystack), needle);
}

// Like std::find, but the element must exist at most once.
//
// Precondition: std::count(begin, end, needle) <= 1
// Precondition: (*it == needle) must be valid for all it between begin and end.
//               (std::find does not have this requirement.)
template <class It, class T>
It find_unique(It begin, It end, const T& needle) {
  It result = std::find(begin, end, needle);
  if (result != end) {
    if constexpr (std::is_base_of_v<
                      std::forward_iterator_tag,
                      typename std::iterator_traits<It>::iterator_category>) {
      QLJS_ASSERT(std::find(std::next(result), end, needle) == end);
    }
  }
  return result;
}

template <class Range, class T>
auto find_unique(Range&& haystack, const T& needle) {
  return find_unique(std::begin(haystack), std::end(haystack), needle);
}

// Like find_unique, but the element must exist.
//
// Precondition: find_unique(begin, end, needle) != end
// Postcondition: result != end
template <class It, class T>
It find_unique_existing(It begin, It end, const T& needle) {
  It result = find_unique(begin, end, needle);
  QLJS_ASSERT(result != end);
  return result;
}

template <class Range, class T>
auto find_unique_existing(Range&& haystack, const T& needle) {
  return find_unique_existing(std::begin(haystack), std::end(haystack), needle);
}

// Like std::find_if, but the element must exist at most once.
//
// Precondition: std::count(begin, end, needle) <= 1
// Precondition: predicate(*it) must be valid for all it between begin and end.
//               (std::find_if does not have this requirement.)
template <class It, class Pred>
It find_unique_if(It begin, It end, Pred&& predicate) {
  It result = std::find_if(begin, end, predicate);
  if (result != end) {
    if constexpr (std::is_base_of_v<
                      std::forward_iterator_tag,
                      typename std::iterator_traits<It>::iterator_category>) {
      QLJS_ASSERT(std::find_if(std::next(result), end, predicate) == end);
    }
  }
  return result;
}

template <class Range, class Pred>
auto find_unique_if(Range&& haystack, Pred&& predicate) {
  return find_unique_if(std::begin(haystack), std::end(haystack), predicate);
}

// Like find_unique_if, but the element must exist.
//
// Precondition: find_unique_if(begin, end, needle) != end
// Postcondition: result != end
template <class It, class Pred>
It find_unique_existing_if(It begin, It end, Pred&& predicate) {
  It result = find_unique_if(begin, end, predicate);
  QLJS_ASSERT(result != end);
  return result;
}

template <class Range, class Pred>
auto find_unique_existing_if(Range&& haystack, Pred&& predicate) {
  return find_unique_existing_if(std::begin(haystack), std::end(haystack),
                                 predicate);
}

// An alias for std::ranges::any_of.
template <class Range, class Pred>
bool any_of(Range&& haystack, Pred&& predicate) {
  return std::any_of(std::begin(haystack), std::end(haystack), predicate);
}

// Like std::any_of, but comparing against a value using operator==.
template <class It, class T>
bool contains(It begin, It end, const T& needle) {
  return std::find(begin, end, needle) != end;
}

// Like std::ranges::any_of, but comparing against a value using operator==.
template <class Range, class T>
bool contains(Range&& haystack, const T& needle) {
  return contains(std::begin(haystack), std::end(haystack), needle);
}

// An alias for std::ranges::equal.
template <class RangeA, class RangeB>
bool ranges_equal(RangeA&& a, RangeB&& b) {
  return std::equal(std::begin(a), std::end(a), std::begin(b), std::end(b));
}

// An alias for std::ranges::equal.
template <class RangeA, class RangeB, class Pred>
auto ranges_equal(RangeA&& a, RangeB&& b, Pred&& predicate) {
  return std::equal(std::begin(a), std::end(a), std::begin(b), std::end(b),
                    predicate);
}

// An alias for std::ranges::fill.
template <class Range, class T>
void fill(Range&& r, const T& value) {
  std::fill(std::begin(r), std::end(r), value);
}

// An alias for std::ranges::sort.
template <class Range>
void sort(Range&& r) {
  std::sort(std::begin(r), std::end(r));
}

// An alias for std::ranges::sort.
template <class Range, class Comparator>
void sort(Range&& r, Comparator&& compare) {
  std::sort(std::begin(r), std::end(r), compare);
}

// An alias for std::ranges::reverse.
template <class Range>
void reverse(Range&& r) {
  std::reverse(std::begin(r), std::end(r));
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
