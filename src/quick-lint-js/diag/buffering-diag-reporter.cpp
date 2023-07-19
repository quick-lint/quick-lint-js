// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <memory>
#include <quick-lint-js/container/allocator.h>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/diag/buffering-diag-reporter.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/unreachable.h>
#include <type_traits>

namespace quick_lint_js {
struct Buffering_Diag_Reporter::Impl {
  explicit Impl(Memory_Resource *memory) noexcept : memory_(memory) {}

  struct Any_Diag {
    union underlying_diag {
      explicit underlying_diag() noexcept {}

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format)    \
  ::quick_lint_js::name name;                                        \
  static_assert(std::is_trivially_copyable_v<::quick_lint_js::name>, \
                #name " should be trivially copyable");
      QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
    };

    Diag_Type type;
    underlying_diag diag;
  };

  Memory_Resource *memory_;
  Linked_Vector<Any_Diag> diagnostics_{this->memory_};
};

void Buffering_Diag_Reporter::Impl_Deleter::operator()(Impl *i) noexcept {
  if (i) {
    delete_object(i->memory_, i);
  }
}

Buffering_Diag_Reporter::Buffering_Diag_Reporter(Memory_Resource *memory)
    : impl_(new_object<Impl>(memory, memory)) {}

Buffering_Diag_Reporter::Buffering_Diag_Reporter(Buffering_Diag_Reporter &&) =
    default;

Buffering_Diag_Reporter &Buffering_Diag_Reporter::operator=(
    Buffering_Diag_Reporter &&) = default;

Buffering_Diag_Reporter::~Buffering_Diag_Reporter() = default;

void Buffering_Diag_Reporter::report_impl(Diag_Type type, void *diag) {
  static constexpr unsigned char diag_sizes[] = {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format) \
  sizeof(::quick_lint_js::name),
      QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
  };

  Impl::Any_Diag &e = this->impl_->diagnostics_.emplace_back();
  e.type = type;
  std::memcpy(&e.diag, diag, diag_sizes[static_cast<std::ptrdiff_t>(type)]);
}

void Buffering_Diag_Reporter::copy_into(Diag_Reporter *other) const {
  this->impl_->diagnostics_.for_each([&](const Impl::Any_Diag &diag) {
    // TODO(strager): Make report_impl accept a const pointer to remove this
    // const_cast.
    other->report_impl(diag.type, &const_cast<Impl::Any_Diag &>(diag).diag);
  });
}

void Buffering_Diag_Reporter::move_into(Diag_Reporter *other) {
  this->copy_into(other);
}

bool Buffering_Diag_Reporter::empty() const noexcept {
  return this->impl_->diagnostics_.empty();
}

void Buffering_Diag_Reporter::clear() noexcept {
  return this->impl_->diagnostics_.clear();
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
