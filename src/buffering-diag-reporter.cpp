// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/container/pmr/memory_resource.hpp>
#include <boost/container/pmr/polymorphic_allocator.hpp>
#include <deque>
#include <memory>
#include <quick-lint-js/allocator.h>
#include <quick-lint-js/buffering-diag-reporter.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <type_traits>

namespace quick_lint_js {
struct buffering_diag_reporter::impl {
  explicit impl(boost::container::pmr::memory_resource *memory) noexcept
      : memory_(memory) {}

  struct any_error {
    union underlying_error {
      explicit underlying_error() noexcept {}

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format)    \
  ::quick_lint_js::name name;                                        \
  static_assert(std::is_trivially_copyable_v<::quick_lint_js::name>, \
                #name " should be trivially copyable");
      QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
    };

    diag_type type;
    underlying_error error;
  };

  boost::container::pmr::memory_resource *memory_;
  std::deque<any_error, boost::container::pmr::polymorphic_allocator<any_error>>
      errors_{this->memory_};
};

void buffering_diag_reporter::impl_deleter::operator()(impl *i) noexcept {
  if (i) {
    delete_object(i->memory_, i);
  }
}

buffering_diag_reporter::buffering_diag_reporter(
    boost::container::pmr::memory_resource *memory)
    : impl_(new_object<impl>(memory, memory)) {}

buffering_diag_reporter::buffering_diag_reporter(buffering_diag_reporter &&) =
    default;

buffering_diag_reporter &buffering_diag_reporter::operator=(
    buffering_diag_reporter &&) = default;

buffering_diag_reporter::~buffering_diag_reporter() = default;

void buffering_diag_reporter::report_impl(diag_type type, void *error) {
  static constexpr unsigned char error_sizes[] = {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format) \
  sizeof(::quick_lint_js::name),
      QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
  };

  impl::any_error &e = this->impl_->errors_.emplace_back();
  e.type = type;
  std::memcpy(&e.error, error, error_sizes[static_cast<std::ptrdiff_t>(type)]);
}

void buffering_diag_reporter::copy_into(diag_reporter *other) const {
  for (impl::any_error &error : this->impl_->errors_) {
    other->report_impl(error.type, &error.error);
  }
}

void buffering_diag_reporter::move_into(diag_reporter *other) {
  this->copy_into(other);
}

bool buffering_diag_reporter::empty() const noexcept {
  return this->impl_->errors_.empty();
}

void buffering_diag_reporter::clear() noexcept {
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
