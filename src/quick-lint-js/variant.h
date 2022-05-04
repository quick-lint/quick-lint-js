// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VARIANT_H
#define QUICK_LINT_JS_VARIANT_H

#include <cstddef>
#include <cstdint>
#include <new>
#include <quick-lint-js/assert.h>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
// Like std::variant<Ts...>, but simpler and with more copy-paste.
template <class... Ts>
class variant;

struct monostate {
  friend bool operator==(monostate, monostate) noexcept { return true; }
  friend bool operator!=(monostate, monostate) noexcept { return false; }
};

template <class T, class... Ts>
bool holds_alternative(const variant<Ts...>& v) noexcept {
  return v.template holds_alternative<T>();
}

template <class T, class... Ts>
T& get(variant<Ts...>& v) noexcept {
  return v.template get<T>();
}

template <class T, class... Ts>
auto&& get(variant<Ts...>&& v) noexcept {
  return std::move(v).template get<T>();
}

template <class T, class... Ts>
const T& get(const variant<Ts...>& v) noexcept {
  return v.template get<T>();
}

template <std::size_t Index, class... Ts>
auto& get(variant<Ts...>& v) noexcept {
  return v.template get<Index>();
}

template <std::size_t Index, class... Ts>
auto&& get(variant<Ts...>&& v) noexcept {
  return std::move(v).template get<Index>();
}

template <std::size_t Index, class... Ts>
const auto& get(const variant<Ts...>& v) noexcept {
  return v.template get<Index>();
}

template <class Visitor, class... Ts>
auto visit(Visitor&& f, variant<Ts...>& v) {
  return v.visit(std::forward<Visitor>(f));
}

template <class Visitor, class... Ts>
auto visit(Visitor&& f, variant<Ts...>&& v) {
  return std::move(v).visit(std::forward<Visitor>(f));
}

template <class Visitor, class... Ts>
auto visit(Visitor&& f, const variant<Ts...>& v) {
  return v.visit(std::forward<Visitor>(f));
}

#define QLJS_VARIANT_MIXIN                                                  \
  template <class T>                                                        \
      auto&& get() && noexcept {                                            \
    return std::move(this->template get<T>());                              \
  }                                                                         \
                                                                            \
  template <class T>                                                        \
  const T& get() const& noexcept {                                          \
    return const_cast<variant*>(this)->template get<T>();                   \
  }                                                                         \
                                                                            \
  template <std::size_t Index>                                              \
      auto&& get() && noexcept {                                            \
    return std::move(this->template get<Index>());                          \
  }                                                                         \
                                                                            \
  template <std::size_t Index>                                              \
  const auto& get() const& noexcept {                                       \
    return const_cast<variant*>(this)->template get<Index>();               \
  }                                                                         \
                                                                            \
  friend bool operator!=(const variant& lhs, const variant& rhs) noexcept { \
    return !(lhs == rhs);                                                   \
  }

#define QLJS_VARIANT_MOVE_ASSIGN_CASE(index)                                   \
  case index:                                                                  \
    if (this->tag_ == index) {                                                 \
      this->data_##index##_ = std::move(other.data_##index##_);                \
    } else {                                                                   \
      this->destruct();                                                        \
      this->tag_ = other.tag_;                                                 \
      new (&this->data_##index##_) T##index(std::move(other.data_##index##_)); \
    }                                                                          \
    break;

// TODO(strager): Use std::launder whenever accessing this->data_N_ like a good
// C++ citizen.

template <class T0>
class variant<T0> {
 public:
  QLJS_VARIANT_MIXIN

  /*implicit*/ variant(T0&& data) : data_0_(std::move(data)) {}

  template <class... Args>
  explicit variant(std::in_place_type_t<T0>, Args&&... args)
      : data_0_(std::forward<Args>(args)...) {}

  template <class... Args>
  explicit variant(std::in_place_index_t<0>, Args&&... args)
      : data_0_(std::forward<Args>(args)...) {}

  variant(const variant&) = default;
  variant& operator=(const variant&) = default;

  variant(variant&&) = default;
  variant& operator=(variant&&) = default;

  ~variant() = default;

  std::size_t index() const noexcept { return 0; }

  template <class T>
  bool holds_alternative() const noexcept {
    static_assert(std::is_same_v<T, T0>, "unexpected T");
    return true;
  }

  template <class T>
      T& get() & noexcept {
    static_assert(std::is_same_v<T, T0>, "unexpected T");
    return this->data_0_;
  }

  template <std::size_t Index>
      T0& get() & noexcept {
    static_assert(Index == 0, "unexpected Index");
    return this->data_0_;
  }

  template <class Visitor>
  auto visit(Visitor&& f) & {
    return std::forward<Visitor>(f)(this->data_0_);
  }

  template <class Visitor>
  auto visit(Visitor&& f) && {
    return std::forward<Visitor>(f)(std::move(this->data_0_));
  }

  template <class Visitor>
  auto visit(Visitor&& f) const& {
    return std::forward<Visitor>(f)(this->data_0_);
  }

  friend bool operator==(const variant& lhs, const variant& rhs) noexcept {
    return lhs.data_0_ == rhs.data_0_;
  }

 private:
  T0 data_0_;
};

template <class T0, class T1>
class variant<T0, T1> {
 public:
  QLJS_VARIANT_MIXIN

  /*implicit*/ variant(T0&& data) : tag_(0), data_0_(std::move(data)) {}
  /*implicit*/ variant(T1&& data) : tag_(1), data_1_(std::move(data)) {}

  template <class... Args>
  explicit variant(std::in_place_type_t<T0>, Args&&... args)
      : tag_(0), data_0_(std::forward<Args>(args)...) {}
  template <class... Args>
  explicit variant(std::in_place_type_t<T1>, Args&&... args)
      : tag_(1), data_1_(std::forward<Args>(args)...) {}

  template <class... Args>
  explicit variant(std::in_place_index_t<0>, Args&&... args)
      : tag_(0), data_0_(std::forward<Args>(args)...) {}
  template <class... Args>
  explicit variant(std::in_place_index_t<1>, Args&&... args)
      : tag_(1), data_1_(std::forward<Args>(args)...) {}

  variant(const variant& other) : tag_(other.tag_) {
    switch (this->tag_) {
    case 0:
      new (&this->data_0_) T0(other.data_0_);
      break;
    case 1:
      new (&this->data_1_) T1(other.data_1_);
      break;
    default:
      QLJS_UNREACHABLE();
    }
  }

  variant& operator=(const variant&) = delete;  // TODO(strager)

  variant(variant&& other) : tag_(other.tag_) {
    switch (this->tag_) {
    case 0:
      new (&this->data_0_) T0(std::move(other.data_0_));
      break;
    case 1:
      new (&this->data_1_) T1(std::move(other.data_1_));
      break;
    default:
      QLJS_UNREACHABLE();
    }
  }

  variant& operator=(variant&& other) {
    if (&other == this) {
      return *this;
    }
    switch (other.tag_) {
      QLJS_VARIANT_MOVE_ASSIGN_CASE(0)
      QLJS_VARIANT_MOVE_ASSIGN_CASE(1)
    default:
      QLJS_UNREACHABLE();
    }
    return *this;
  }

  ~variant() { this->destruct(); }

  std::size_t index() const noexcept { return this->tag_; }

  template <class T>
  bool holds_alternative() const noexcept {
    if constexpr (std::is_same_v<T, T0>) {
      return this->tag_ == 0;
    } else if constexpr (std::is_same_v<T, T1>) {
      return this->tag_ == 1;
    } else {
      static_assert(std::is_same_v<T, T0>, "unexpected T");
    }
  }

  template <class T>
      T& get() & noexcept {
    if constexpr (std::is_same_v<T, T0>) {
      QLJS_ASSERT(this->tag_ == 0);
      return this->data_0_;
    } else if constexpr (std::is_same_v<T, T1>) {
      QLJS_ASSERT(this->tag_ == 1);
      return this->data_1_;
    } else {
      static_assert(std::is_same_v<T, T0>, "unexpected T");
    }
  }

  template <std::size_t Index>
      auto& get() & noexcept {
    if constexpr (Index == 0) {
      QLJS_ASSERT(this->tag_ == 0);
      return this->data_0_;
    } else if constexpr (Index == 1) {
      QLJS_ASSERT(this->tag_ == 1);
      return this->data_1_;
    } else {
      static_assert(Index == 0, "unexpected Index");
    }
  }

  template <class Visitor>
  auto visit(Visitor&& f) & {
    switch (this->tag_) {
    case 0:
      return std::forward<Visitor>(f)(this->data_0_);
    case 1:
      return std::forward<Visitor>(f)(this->data_1_);
    default:
      QLJS_UNREACHABLE();
    }
  }

  template <class Visitor>
  auto visit(Visitor&& f) && {
    switch (this->tag_) {
    case 0:
      return std::forward<Visitor>(f)(std::move(this->data_0_));
    case 1:
      return std::forward<Visitor>(f)(std::move(this->data_1_));
    default:
      QLJS_UNREACHABLE();
    }
  }

  template <class Visitor>
  auto visit(Visitor&& f) const& {
    switch (this->tag_) {
    case 0:
      return std::forward<Visitor>(f)(this->data_0_);
    case 1:
      return std::forward<Visitor>(f)(this->data_1_);
    default:
      QLJS_UNREACHABLE();
    }
  }

  friend bool operator==(const variant& lhs, const variant& rhs) noexcept {
    if (lhs.tag_ != rhs.tag_) {
      return false;
    }
    switch (lhs.tag_) {
    case 0:
      return lhs.data_0_ == rhs.data_0_;
    case 1:
      return lhs.data_1_ == rhs.data_1_;
    default:
      QLJS_UNREACHABLE();
    }
  }

 private:
  void destruct() {
    switch (this->tag_) {
    case 0:
      this->data_0_.~T0();
      break;
    case 1:
      this->data_1_.~T1();
      break;
    default:
      QLJS_UNREACHABLE();
    }
  }

  std::uint8_t tag_;
  union {
    T0 data_0_;
    T1 data_1_;
  };
};

template <class T0, class T1, class T2>
class variant<T0, T1, T2> {
 public:
  QLJS_VARIANT_MIXIN

  /*implicit*/ variant(T0&& data) : tag_(0), data_0_(std::move(data)) {}
  /*implicit*/ variant(T1&& data) : tag_(1), data_1_(std::move(data)) {}
  /*implicit*/ variant(T2&& data) : tag_(2), data_2_(std::move(data)) {}

  template <class... Args>
  explicit variant(std::in_place_type_t<T0>, Args&&... args)
      : tag_(0), data_0_(std::forward<Args>(args)...) {}
  template <class... Args>
  explicit variant(std::in_place_type_t<T1>, Args&&... args)
      : tag_(1), data_1_(std::forward<Args>(args)...) {}
  template <class... Args>
  explicit variant(std::in_place_type_t<T2>, Args&&... args)
      : tag_(2), data_2_(std::forward<Args>(args)...) {}

  template <class... Args>
  explicit variant(std::in_place_index_t<0>, Args&&... args)
      : tag_(0), data_0_(std::forward<Args>(args)...) {}
  template <class... Args>
  explicit variant(std::in_place_index_t<1>, Args&&... args)
      : tag_(1), data_1_(std::forward<Args>(args)...) {}
  template <class... Args>
  explicit variant(std::in_place_index_t<2>, Args&&... args)
      : tag_(2), data_2_(std::forward<Args>(args)...) {}

  variant(const variant& other) : tag_(other.tag_) {
    switch (this->tag_) {
    case 0:
      new (&this->data_0_) T0(other.data_0_);
      break;
    case 1:
      new (&this->data_1_) T1(other.data_1_);
      break;
    case 2:
      new (&this->data_2_) T2(other.data_2_);
      break;
    default:
      QLJS_UNREACHABLE();
    }
  }

  variant& operator=(const variant&) = delete;  // TODO(strager)

  variant(variant&& other) : tag_(other.tag_) {
    switch (this->tag_) {
    case 0:
      new (&this->data_0_) T0(std::move(other.data_0_));
      break;
    case 1:
      new (&this->data_1_) T1(std::move(other.data_1_));
      break;
    case 2:
      new (&this->data_2_) T2(std::move(other.data_2_));
      break;
    default:
      QLJS_UNREACHABLE();
    }
  }

  variant& operator=(variant&& other) {
    if (&other == this) {
      return *this;
    }
    switch (other.tag_) {
      QLJS_VARIANT_MOVE_ASSIGN_CASE(0)
      QLJS_VARIANT_MOVE_ASSIGN_CASE(1)
      QLJS_VARIANT_MOVE_ASSIGN_CASE(2)
    default:
      QLJS_UNREACHABLE();
    }
    return *this;
  }

  ~variant() { this->destruct(); }

  std::size_t index() const noexcept { return this->tag_; }

  template <class T>
  bool holds_alternative() const noexcept {
    if constexpr (std::is_same_v<T, T0>) {
      return this->tag_ == 0;
    } else if constexpr (std::is_same_v<T, T1>) {
      return this->tag_ == 1;
    } else if constexpr (std::is_same_v<T, T2>) {
      return this->tag_ == 2;
    } else {
      static_assert(std::is_same_v<T, T0>, "unexpected T");
    }
  }

  template <class T>
      T& get() & noexcept {
    if constexpr (std::is_same_v<T, T0>) {
      QLJS_ASSERT(this->tag_ == 0);
      return this->data_0_;
    } else if constexpr (std::is_same_v<T, T1>) {
      QLJS_ASSERT(this->tag_ == 1);
      return this->data_1_;
    } else if constexpr (std::is_same_v<T, T2>) {
      QLJS_ASSERT(this->tag_ == 2);
      return this->data_2_;
    } else {
      static_assert(std::is_same_v<T, T0>, "unexpected T");
    }
  }

  template <std::size_t Index>
      auto& get() & noexcept {
    if constexpr (Index == 0) {
      QLJS_ASSERT(this->tag_ == 0);
      return this->data_0_;
    } else if constexpr (Index == 1) {
      QLJS_ASSERT(this->tag_ == 1);
      return this->data_1_;
    } else if constexpr (Index == 2) {
      QLJS_ASSERT(this->tag_ == 2);
      return this->data_2_;
    } else {
      static_assert(Index == 0, "unexpected Index");
    }
  }

  template <class Visitor>
  auto visit(Visitor&& f) & {
    switch (this->tag_) {
    case 0:
      return std::forward<Visitor>(f)(this->data_0_);
    case 1:
      return std::forward<Visitor>(f)(this->data_1_);
    case 2:
      return std::forward<Visitor>(f)(this->data_2_);
    default:
      QLJS_UNREACHABLE();
    }
  }

  template <class Visitor>
  auto visit(Visitor&& f) && {
    switch (this->tag_) {
    case 0:
      return std::forward<Visitor>(f)(std::move(this->data_0_));
    case 1:
      return std::forward<Visitor>(f)(std::move(this->data_1_));
    case 2:
      return std::forward<Visitor>(f)(std::move(this->data_2_));
    default:
      QLJS_UNREACHABLE();
    }
  }

  template <class Visitor>
  auto visit(Visitor&& f) const& {
    switch (this->tag_) {
    case 0:
      return std::forward<Visitor>(f)(this->data_0_);
    case 1:
      return std::forward<Visitor>(f)(this->data_1_);
    case 2:
      return std::forward<Visitor>(f)(this->data_2_);
    default:
      QLJS_UNREACHABLE();
    }
  }

  friend bool operator==(const variant& lhs, const variant& rhs) noexcept {
    if (lhs.tag_ != rhs.tag_) {
      return false;
    }
    switch (lhs.tag_) {
    case 0:
      return lhs.data_0_ == rhs.data_0_;
    case 1:
      return lhs.data_1_ == rhs.data_1_;
    case 2:
      return lhs.data_2_ == rhs.data_2_;
    default:
      QLJS_UNREACHABLE();
    }
  }

 private:
  void destruct() {
    switch (this->tag_) {
    case 0:
      this->data_0_.~T0();
      break;
    case 1:
      this->data_1_.~T1();
      break;
    case 2:
      this->data_2_.~T2();
      break;
    default:
      QLJS_UNREACHABLE();
    }
  }

  std::uint8_t tag_;
  union {
    T0 data_0_;
    T1 data_1_;
    T2 data_2_;
  };
};

#undef QLJS_VARIANT_MIXIN
#undef QLJS_VARIANT_MOVE_ASSIGN_CASE
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
