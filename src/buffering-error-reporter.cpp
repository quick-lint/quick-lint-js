// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <memory>
#include <quick-lint-js/buffering-error-reporter.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <type_traits>
#include <vector>

namespace quick_lint_js {
struct buffering_error_reporter::impl {
  struct any_error {
    union underlying_error {
      explicit underlying_error() noexcept {}

#define QLJS_ERROR_TYPE(name, code, struct_body, format)             \
  ::quick_lint_js::name name;                                        \
  static_assert(std::is_trivially_copyable_v<::quick_lint_js::name>, \
                #name " should be trivially copyable");
      QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
    };

    error_type type;
    underlying_error error;
  };

  std::vector<any_error> errors_;
};

buffering_error_reporter::buffering_error_reporter()
    : impl_(std::make_unique<impl>()) {}

buffering_error_reporter::buffering_error_reporter(
    buffering_error_reporter &&) = default;

buffering_error_reporter &buffering_error_reporter::operator=(
    buffering_error_reporter &&) = default;

buffering_error_reporter::~buffering_error_reporter() = default;

void buffering_error_reporter::report_impl(error_type type, void *error) {
  static constexpr unsigned char error_sizes[] = {
#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
  sizeof(::quick_lint_js::name),
      QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
  };

  impl::any_error &e = this->impl_->errors_.emplace_back();
  e.type = type;
  std::memcpy(&e.error, error, error_sizes[static_cast<std::ptrdiff_t>(type)]);
}

void buffering_error_reporter::copy_into(error_reporter *other) const {
  for (impl::any_error &error : this->impl_->errors_) {
    switch (error.type) {
#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
  case error_type::name:                                 \
    other->report(error.error.name);                     \
    break;
      QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
    }
  }
}

void buffering_error_reporter::move_into(error_reporter *other) {
  this->copy_into(other);
}

bool buffering_error_reporter::empty() const noexcept {
  return this->impl_->errors_.empty();
}

void buffering_error_reporter::clear() noexcept {
  return this->impl_->errors_.clear();
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
