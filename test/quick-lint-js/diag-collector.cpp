// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <ostream>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/unreachable.h>

namespace quick_lint_js {
void diag_collector_static_assertions() {
#define QLJS_DIAG_TYPE_NAME(Diag_Type)                                       \
  static_assert(sizeof(Diag_Collector::Diag::storage_) >= sizeof(Diag_Type), \
                "storage should be big enough to fit " #Diag_Type);          \
  static_assert(std::is_trivially_copyable_v<Diag_Type>,                     \
                #Diag_Type " should be trivially copyable");
  QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME
}

void Diag_Collector::report_impl(Diag_Type type, void *diag) {
  this->errors.push_back(Diag(type, diag));
}

#define QLJS_DIAG_TYPE_NAME(Type) \
  Diag_Collector::Diag::Diag(const Type &data) : Diag(Diag_Type::Type, &data) {}
QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME

Diag_Type Diag_Collector::Diag::type() const { return this->type_; }

const void *Diag_Collector::Diag::data() const { return &this->storage_; }

Diag_Collector::Diag::Diag(Diag_Type type, const void *data) {
  static constexpr unsigned char diag_sizes[] = {
#define QLJS_DIAG_TYPE_NAME(Diag_Type) sizeof(Diag_Type),
      QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME
  };

  this->type_ = type;
  std::memcpy(&this->storage_, data,
              diag_sizes[static_cast<std::ptrdiff_t>(type)]);
}

void PrintTo(const Diag_Collector::Diag &e, std::ostream *out) {
  *out << e.type_;
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
