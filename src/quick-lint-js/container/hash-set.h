// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_HASH_SET_H
#define QUICK_LINT_JS_CONTAINER_HASH_SET_H

#include <quick-lint-js/container/hash-map.h>
#include <tuple>
#include <utility>

namespace quick_lint_js {
struct empty_hash_map_value {};

// Like std::unordered_set.
template <class Key>
class hash_set {
 private:
  using map = hash_map<Key, empty_hash_map_value>;

 public:
  using size_type = typename map::size_type;

  class iterator {
   public:
    using difference_type = typename map::iterator::difference_type;
    using iterator_category = typename map::iterator::iterator_category;
    using pointer = const Key*;
    using reference = const Key&;
    using value_type = Key;

    const Key& operator*() const noexcept { return this->it_->first; }

    const Key* operator->() const noexcept { return &**this; }

    iterator& operator++() noexcept {
      ++this->it_;
      return *this;
    }

    iterator& operator++(int) noexcept {
      iterator old = *this;
      ++*this;
      return old;
    }

    friend bool operator==(const iterator& lhs, const iterator& rhs) {
      return lhs.it_ == rhs.it_;
    }

    friend bool operator!=(const iterator& lhs, const iterator& rhs) {
      return !(lhs == rhs);
    }

   private:
    explicit iterator(typename map::iterator it) : it_(it) {}

    typename map::iterator it_;

    friend class hash_set;
  };

  class const_iterator {
   public:
    using difference_type = typename map::const_iterator::difference_type;
    using iterator_category = typename map::const_iterator::iterator_category;
    using pointer = const Key*;
    using reference = const Key&;
    using value_type = Key;

    const Key& operator*() const noexcept { return this->it_->first; }

    const Key* operator->() const noexcept { return &**this; }

    const_iterator& operator++() noexcept {
      ++this->it_;
      return *this;
    }

    const_iterator& operator++(int) noexcept {
      const_iterator old = *this;
      ++*this;
      return old;
    }

    friend bool operator==(const const_iterator& lhs,
                           const const_iterator& rhs) {
      return lhs.it_ == rhs.it_;
    }

    friend bool operator!=(const const_iterator& lhs,
                           const const_iterator& rhs) {
      return !(lhs == rhs);
    }

   private:
    explicit const_iterator(typename map::const_iterator it) : it_(it) {}

    typename map::const_iterator it_;

    friend class hash_set;
  };

  template <class K>
  bool contains(const K& key) const noexcept {
    return this->set_.find(key) != this->set_.end();
  }

  template <class K>
  const_iterator find(const K& key) const noexcept {
    return iterator(this->set_.find(key));
  }
  template <class K>
  iterator find(const K& key) noexcept {
    return iterator(this->set_.find(key));
  }

  iterator begin() noexcept { return iterator(this->set_.begin()); }
  const_iterator begin() const noexcept {
    return const_iterator(this->set_.begin());
  }

  iterator end() noexcept { return iterator(this->set_.end()); }
  const_iterator end() const noexcept {
    return const_iterator(this->set_.end());
  }

  bool empty() const noexcept { return this->set_.empty(); }

  size_type size() const noexcept { return this->set_.size(); }

  // Copies the key on insertion.
  std::pair<iterator, bool> insert(const Key& key) {
    return this->emplace(key);
  }

  template <class... Args>
  std::pair<iterator, bool> emplace(Args&&... args) {
    auto [it, inserted] = this->set_.emplace(
        std::piecewise_construct, std::forward_as_tuple<Args...>(args...),
        std::tuple<empty_hash_map_value>(empty_hash_map_value()));
    return std::pair<iterator, bool>(iterator(it), inserted);
  }

  template <class K>
  size_type erase(K&& key) {
    auto it = this->find(std::forward<K>(key));
    if (it == this->end()) {
      return 0;
    }
    this->erase(it);
    return 1;
  }

  void erase(iterator it) { this->set_.erase(it.it_); }

  void reserve(size_type size) { this->set_.reserve(size); }

 private:
  map set_;
};
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
