// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <ostream>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/unreachable.h>

namespace quick_lint_js {
void Diag_Collector::report_impl(Diag_Type type, void *diag) {
  switch (type) {
    // NOTE(strager): Avoid emplace_back as it leads to many template
    // instantiations, slowing down compilation.
#define QLJS_DIAG_TYPE_NAME(name)                                        \
  case Diag_Type::name:                                                  \
    this->errors.push_back(Diag(*reinterpret_cast<const name *>(diag))); \
    break;
    QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME
  }
}

#define QLJS_DIAG_TYPE_NAME(name)              \
  Diag_Collector::Diag::Diag(const name &data) \
      : type_(Diag_Type::name), variant_##name##_(std::move(data)) {}
QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME

Diag_Type Diag_Collector::Diag::type() const { return this->type_; }

const void *Diag_Collector::Diag::data() const {
  return &this->variant_Diag_Unexpected_Token_;  // Arbitrary member.
}

#define QLJS_DIAG_TYPE_NAME(name)                               \
  template <>                                                   \
  bool holds_alternative<name>(const Diag_Collector::Diag &e) { \
    return e.type_ == Diag_Type::name;                          \
  }                                                             \
                                                                \
  template <>                                                   \
  const name &get<name>(const Diag_Collector::Diag &e) {        \
    QLJS_ASSERT(holds_alternative<name>(e));                    \
    return e.variant_##name##_;                                 \
  }
QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME

void PrintTo(const Diag_Collector::Diag &e, std::ostream *out) {
  *out << e.type_;
}

#define QLJS_DIAG_TYPE_NAME(name) \
  void PrintTo(const name &, std::ostream *out) { *out << #name; }
QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME
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
