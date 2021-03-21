// Copyright (C) 2020  Matthew Glazar
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
    enum class error_kind {
#define QLJS_ERROR_TYPE(name, code, struct_body, format) name,
      QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
    };

    union underlying_error {
#define QLJS_ERROR_TYPE(name, code, struct_body, format)             \
  ::quick_lint_js::name name;                                        \
  static_assert(std::is_trivially_copyable_v<::quick_lint_js::name>, \
                #name " should be trivially copyable");
      QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
    };

    error_kind kind;
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

#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
  void buffering_error_reporter::report(name error) {    \
    this->impl_->errors_.push_back(impl::any_error{      \
        .kind = impl::any_error::error_kind::name,       \
        .error = {.name = error},                        \
    });                                                  \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

void buffering_error_reporter::report_fatal_error_unimplemented_character(
    const char *, int, const char *, const char8 *) {}

void buffering_error_reporter::report_fatal_error_unimplemented_token(
    const char *, int, const char *, token_type, const char8 *) {}

void buffering_error_reporter::move_into(error_reporter *other) {
  for (impl::any_error &error : this->impl_->errors_) {
    switch (error.kind) {
#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
  case impl::any_error::error_kind::name:                \
    other->report(error.error.name);                     \
    break;
      QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
    }
  }
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
