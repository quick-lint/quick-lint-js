// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <memory>
#include <quick-lint-js/buffering-error-reporter.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/unreachable.h>
#include <variant>
#include <vector>

namespace quick_lint_js {
struct buffering_error_reporter::impl {
  using any_error = std::variant<std::monostate
#define QLJS_ERROR_TYPE(name, code, struct_body, format) , name
                                     QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
                                 >;

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
    this->impl_->errors_.emplace_back(std::move(error)); \
  }
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

void buffering_error_reporter::report_fatal_error_unimplemented_character(
    const char *, int, const char *, const char8 *) {}

void buffering_error_reporter::report_fatal_error_unimplemented_token(
    const char *, int, const char *, token_type, const char8 *) {}

void buffering_error_reporter::move_into(error_reporter *other) {
  struct error_visitor {
    [[noreturn]] void operator()(std::monostate) { QLJS_UNREACHABLE(); }

#define QLJS_ERROR_TYPE(name, code, struct_body, format) \
  void operator()(const name &error) { this->other->report(error); }
    QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

    error_reporter *other;
  };

  for (impl::any_error &error : this->impl_->errors_) {
    std::visit(error_visitor{other}, error);
  }
}
}
