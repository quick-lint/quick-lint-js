// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_HASH_SET_H
#define QUICK_LINT_JS_CONTAINER_HASH_SET_H

#include <quick-lint-js/container/hash-map.h>
#include <tuple>
#include <utility>

namespace quick_lint_js {
struct Empty_Hash_Map_Value {};

// Like std::unordered_set.
template <class Key>
class Hash_Set {
 private:
  using Map = Hash_Map<Key, Empty_Hash_Map_Value>;

 public:
  using size_type = typename Map::size_type;

  class Iterator {
   public:
    using difference_type = typename Map::iterator::difference_type;
    using iterator_category = typename Map::iterator::iterator_category;
    using pointer = const Key*;
    using reference = const Key&;
    using value_type = Key;

    const Key& operator*() const { return this->it_->first; }

    const Key* operator->() const { return &**this; }

    Iterator& operator++() {
      ++this->it_;
      return *this;
    }

    Iterator& operator++(int) {
      Iterator old = *this;
      ++*this;
      return old;
    }

    friend bool operator==(const Iterator& lhs, const Iterator& rhs) {
      return lhs.it_ == rhs.it_;
    }

    friend bool operator!=(const Iterator& lhs, const Iterator& rhs) {
      return !(lhs == rhs);
    }

   private:
    explicit Iterator(typename Map::iterator it) : it_(it) {}

    typename Map::iterator it_;

    friend class Hash_Set;
  };

  class Const_Iterator {
   public:
    using difference_type = typename Map::const_iterator::difference_type;
    using iterator_category = typename Map::const_iterator::iterator_category;
    using pointer = const Key*;
    using reference = const Key&;
    using value_type = Key;

    const Key& operator*() const { return this->it_->first; }

    const Key* operator->() const { return &**this; }

    Const_Iterator& operator++() {
      ++this->it_;
      return *this;
    }

    Const_Iterator& operator++(int) {
      Const_Iterator old = *this;
      ++*this;
      return old;
    }

    friend bool operator==(const Const_Iterator& lhs,
                           const Const_Iterator& rhs) {
      return lhs.it_ == rhs.it_;
    }

    friend bool operator!=(const Const_Iterator& lhs,
                           const Const_Iterator& rhs) {
      return !(lhs == rhs);
    }

   private:
    explicit Const_Iterator(typename Map::const_iterator it) : it_(it) {}

    typename Map::const_iterator it_;

    friend class Hash_Set;
  };

  template <class K>
  bool contains(const K& key) const {
    return this->set_.find(key) != this->set_.end();
  }

  template <class K>
  Const_Iterator find(const K& key) const {
    return Iterator(this->set_.find(key));
  }
  template <class K>
  Iterator find(const K& key) {
    return Iterator(this->set_.find(key));
  }

  Iterator begin() { return Iterator(this->set_.begin()); }
  Const_Iterator begin() const { return Const_Iterator(this->set_.begin()); }

  Iterator end() { return Iterator(this->set_.end()); }
  Const_Iterator end() const { return Const_Iterator(this->set_.end()); }

  bool empty() const { return this->set_.empty(); }

  size_type size() const { return this->set_.size(); }

  // Copies the key on insertion.
  std::pair<Iterator, bool> insert(const Key& key) {
    return this->emplace(key);
  }

  template <class... Args>
  std::pair<Iterator, bool> emplace(Args&&... args) {
    auto [it, inserted] = this->set_.emplace(
        std::piecewise_construct, std::forward_as_tuple<Args...>(args...),
        std::tuple<Empty_Hash_Map_Value>(Empty_Hash_Map_Value()));
    return std::pair<Iterator, bool>(Iterator(it), inserted);
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

  void erase(Iterator it) { this->set_.erase(it.it_); }

  void reserve(size_type size) { this->set_.reserve(size); }

 private:
  Map set_;
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
