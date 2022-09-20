// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONTAINER_HASH_MAP_H
#define QUICK_LINT_JS_CONTAINER_HASH_MAP_H

#include <quick-lint-js/container/hash.h>
#include <unordered_map>
#include <utility>

namespace quick_lint_js {
// Like std::unordered_map.
template <class Key, class Value, class Hash = hasher<Key>>
class hash_map {
 private:
  using unordered_map = std::unordered_map<Key, Value, Hash>;

 public:
  using const_iterator = typename unordered_map::const_iterator;
  using iterator = typename unordered_map::iterator;
  using size_type = typename unordered_map::size_type;
  using value_type = typename unordered_map::value_type;

  explicit hash_map() = default;

  explicit hash_map(std::initializer_list<value_type> init) : map_(init) {}

  template <class K>
  const_iterator find(const K& key) const noexcept {
    return this->map_.find(key);
  }
  template <class K>
  iterator find(const K& key) noexcept {
    return this->map_.find(key);
  }

  iterator begin() noexcept { return this->map_.begin(); }
  const_iterator begin() const noexcept { return this->map_.begin(); }

  iterator end() noexcept { return this->map_.end(); }
  const_iterator end() const noexcept { return this->map_.end(); }

  bool empty() const noexcept { return this->map_.empty(); }

  size_type size() const noexcept { return this->map_.size(); }

  // Copies the key if successful.
  template <class... Args>
  std::pair<iterator, bool> try_emplace(const Key& key, Args&&... args) {
    return this->map_.try_emplace(key, std::forward<Args>(args)...);
  }

  // Moves the key if successful.
  template <class... Args>
  std::pair<iterator, bool> try_emplace(Key&& key, Args&&... args) {
    return this->map_.try_emplace(std::move(key), std::forward<Args>(args)...);
  }

  template <class... Args>
  std::pair<iterator, bool> emplace(Args&&... args) {
    return this->map_.emplace(std::forward<Args>(args)...);
  }

  // Copies the key if inserted.
  template <class V>
  std::pair<iterator, bool> insert_or_assign(const Key& key, V&& value) {
    return this->map_.insert_or_assign(key, std::forward<V>(value));
  }

  // Moves the key if inserted.
  template <class V>
  std::pair<iterator, bool> insert_or_assign(Key&& key, V&& value) {
    return this->map_.insert_or_assign(std::move(key), std::forward<V>(value));
  }

  // Copies the key if inserted.
  Value& operator[](const Key& key) { return this->map_[key]; }

  // Moves the key if inserted.
  Value& operator[](Key&& key) { return this->map_[std::move(key)]; }

  template <class K>
  size_type erase(K&& key) {
    return this->map_.erase(std::forward<K>(key));
  }

  void erase(iterator it) { this->map_.erase(it); }

  void clear() { this->map_.clear(); }

  void reserve(size_type size) { this->map_.reserve(size); }

 private:
  unordered_map map_;
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
