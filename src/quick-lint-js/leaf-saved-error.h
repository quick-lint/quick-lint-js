// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LEAF_H
#define QUICK_LINT_JS_LEAF_H

#include <boost/leaf/context.hpp>
#include <boost/leaf/error.hpp>
#include <boost/leaf/result.hpp>
#include <optional>
#include <quick-lint-js/optional.h>
#include <tuple>
#include <utility>

namespace quick_lint_js {
// Each type in Errors must be copy-constructible. This requirement allows
// errors to be both saved and propagated.
template <class... Errors>
class leaf_saved_error {
 public:
  template <class Func>
  static auto /*std::pair<(result of Func), leaf_saved_error<Errors...>>*/ save_error(Func&& callback) {
    boost::leaf::context<Errors...> error_context;
    auto activated_error_context = boost::leaf::activate_context(error_context);
    auto result = std::forward<Func>(callback)();
    if (result) {
      return std::make_pair(std::move(result), leaf_saved_error());
    } else {
      boost::leaf::error_id id = result.error();
      error_context.deactivate();
      return std::make_pair(std::move(result), leaf_saved_error(std::move(error_context.tup()), id.value()));
    }
  }

  explicit leaf_saved_error() noexcept {}

  template <class Error>
  const Error* get() const noexcept {
    return quick_lint_js::get(std::get<std::optional<Error>>(this->errors_));
  }

  template<class T>
  void propagate_copy(boost::leaf::result<T>& result) {
    if (!result) {
      this->propagate_copy_impl(result.get_error_id().value(), std::index_sequence_for<Errors...>());
    }
  }

  template<std::size_t... Indexes>
  void propagate_copy_impl(int error_id, std::index_sequence<Indexes...>) {
    (this->maybe_propagate_copy_one<Indexes>(error_id), ...);
  }

  template<std::size_t Index>
  void maybe_propagate_copy_one(int error_id) {
    auto& our_slot = std::get<Index>(this->errors_);
    if (our_slot.has_value()) {
      using slot_type = typename std::decay_t<decltype(our_slot)>::value_type;
      boost::leaf::leaf_detail::slot<slot_type> *slot = boost::leaf::leaf_detail::tl_slot_ptr<slot_type>::p;
      if (slot) {
        // @@@ should we check slot->has_value?
        slot->put(error_id, *our_slot);
      }
    }
  }

  friend bool operator==(const leaf_saved_error& lhs,
                         const leaf_saved_error& rhs) noexcept {
    return lhs.errors_ == rhs.errors_;
  }

  friend bool operator!=(const leaf_saved_error& lhs,
                         const leaf_saved_error& rhs) noexcept {
    return !(lhs == rhs);
  }

 private:
  template <class... Ts>
  explicit leaf_saved_error(std::tuple<Ts...>&& slots, int id) noexcept
      : errors_(from_slots(std::move(slots), id,
                           std::index_sequence_for<Ts...>())) {}

  template <class Slots, std::size_t... Indexes>
  static auto from_slots(Slots&& slots, int id,
                         std::index_sequence<Indexes...>) {
    auto slot_to_optional = [&](auto& slot) {
      return slot.has_value(id) ? std::make_optional(std::move(slot.value(id)))
                                : std::nullopt;
    };
    return std::make_tuple(slot_to_optional(std::get<Indexes>(slots))...);
  }

  // TODO(strager): A tuple of std::optional is very bloated. Pack the
  // optional bits together to reduce the class size.
  std::tuple<std::optional<Errors>...> errors_;
};

template <class... Errors, class Func>
auto /*std::pair<(result of Func), leaf_saved_error<Errors...>>*/ leaf_save_error(Func&& callback) {
  return leaf_saved_error<Errors...>::save_error(std::forward<Func>(callback));
}
}

#endif

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
