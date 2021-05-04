// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <iostream>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>

namespace quick_lint_js {
#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  void error_collector::report(name e) {                      \
    this->errors.emplace_back(std::move(e));                  \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  error_collector::error::error(name &&data)                  \
      : kind_(kind::kind_##name), variant_##name##_(std::move(data)) {}
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call)              \
  template <>                                                              \
  bool holds_alternative<name>(const error_collector::error &e) noexcept { \
    return e.kind_ == error_collector::error::kind::kind_##name;           \
  }                                                                        \
                                                                           \
  template <>                                                              \
  const name &get<name>(const error_collector::error &e) noexcept {        \
    QLJS_ASSERT(holds_alternative<name>(e));                               \
    return e.variant_##name##_;                                            \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

void PrintTo(const error_collector::error &e, std::ostream *out) {
  switch (e.kind_) {
#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  case error_collector::error::kind::kind_##name:             \
    *out << #name;                                            \
    return;
    QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
  }
  QLJS_UNREACHABLE();
}

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  void PrintTo(const name &, std::ostream *out) { *out << #name; }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
