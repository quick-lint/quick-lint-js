// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <ostream>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>

namespace quick_lint_js {
void error_collector::report_impl(diag_type type, void *error) {
  switch (type) {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  case diag_type::name:                                                \
    this->errors.emplace_back(*reinterpret_cast<const name *>(error)); \
    break;
    QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
  }
}

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  error_collector::error::error(const name &data)                      \
      : type_(diag_type::name), variant_##name##_(std::move(data)) {}
QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE

diag_type error_collector::error::type() const noexcept { return this->type_; }

const char *error_collector::error::error_code() const noexcept {
  switch (this->type_) {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  case diag_type::name:                                                \
    return code;
    QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
  }
  QLJS_UNREACHABLE();
}

const void *error_collector::error::data() const noexcept {
  return &this->variant_diag_unexpected_token_;  // Arbitrary member.
}

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call)     \
  template <>                                                              \
  bool holds_alternative<name>(const error_collector::error &e) noexcept { \
    return e.type_ == diag_type::name;                                     \
  }                                                                        \
                                                                           \
  template <>                                                              \
  const name &get<name>(const error_collector::error &e) noexcept {        \
    QLJS_ASSERT(holds_alternative<name>(e));                               \
    return e.variant_##name##_;                                            \
  }
QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE

void PrintTo(const error_collector::error &e, std::ostream *out) {
  *out << e.type_;
}

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  void PrintTo(const name &, std::ostream *out) { *out << #name; }
QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
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
