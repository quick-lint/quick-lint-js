// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <ostream>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/unreachable.h>

namespace quick_lint_js {
void diag_collector::report_impl(diag_type type, void *diag) {
  switch (type) {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  case diag_type::name:                                                \
    this->errors.emplace_back(*reinterpret_cast<const name *>(diag));  \
    break;
    QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
  }
}

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  diag_collector::diag::diag(const name &data)                         \
      : type_(diag_type::name), variant_##name##_(std::move(data)) {}
QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE

diag_type diag_collector::diag::type() const noexcept { return this->type_; }

const char *diag_collector::diag::error_code() const noexcept {
  switch (this->type_) {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  case diag_type::name:                                                \
    return code;
    QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
  }
  QLJS_UNREACHABLE();
}

const void *diag_collector::diag::data() const noexcept {
  return &this->variant_diag_unexpected_token_;  // Arbitrary member.
}

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call)   \
  template <>                                                            \
  bool holds_alternative<name>(const diag_collector::diag &e) noexcept { \
    return e.type_ == diag_type::name;                                   \
  }                                                                      \
                                                                         \
  template <>                                                            \
  const name &get<name>(const diag_collector::diag &e) noexcept {        \
    QLJS_ASSERT(holds_alternative<name>(e));                             \
    return e.variant_##name##_;                                          \
  }
QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE

void PrintTo(const diag_collector::diag &e, std::ostream *out) {
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
