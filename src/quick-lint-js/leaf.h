// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LEAF_H
#define QUICK_LINT_JS_LEAF_H

#include <quick-lint-js/have.h>
#include <string>

namespace quick_lint_js {
// A POSIX error retrieved from the global errno variable.
struct e_errno {
  int error;
};

// A Windows error returned by GetLastError.
struct e_LastError {
  unsigned long error;
};

// The filesystem path associated with an error.
struct e_file_path {
  std::string path;
};

// Like boost::leaf::match_value, but for e_errno or e_LastError.
template <class Error, decltype(Error::error)... Conditions>
struct match_error {
  using error_type = Error;

  const Error& matched;

  static constexpr bool evaluate(const Error& error) noexcept {
    return ((error.error == Conditions) || ...);
  }
};
}

namespace boost::leaf {
template <class>
struct is_predicate;

template <class Error, decltype(Error::error)... Conditions>
struct is_predicate<quick_lint_js::match_error<Error, Conditions...>>
    : std::true_type {};
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
