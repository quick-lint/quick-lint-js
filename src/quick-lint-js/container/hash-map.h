// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/container/hash.h>
#include <quick-lint-js/port/memory-resource.h>
#include <type_traits>
#include <unordered_map>
#include <utility>

namespace quick_lint_js {
// Like std::pmr::polymorphic_allocator<T>, but with allocator propagation
// enabled and with no default allocator.
template <class T>
class Hash_Map_Allocator {
 public:
  using value_type = T;

  Hash_Map_Allocator(Memory_Resource* memory) : memory_(memory) {}

  template <class U>
  explicit Hash_Map_Allocator(Hash_Map_Allocator<U> other)
      : memory_(other.resource()) {}

  Hash_Map_Allocator(const Hash_Map_Allocator&) = default;
  Hash_Map_Allocator& operator=(const Hash_Map_Allocator&) = default;

  Hash_Map_Allocator(Hash_Map_Allocator&&) = default;
  Hash_Map_Allocator& operator=(Hash_Map_Allocator&&) = default;

  T* allocate(std::size_t size) {
    return static_cast<T*>(
        this->memory_->allocate(size * sizeof(T), alignof(T)));
  }

  void deallocate(T* p, std::size_t size) {
    return this->memory_->deallocate(p, size * sizeof(T), alignof(T));
  }

  friend bool operator==(Hash_Map_Allocator lhs, Hash_Map_Allocator rhs) {
    return lhs.memory_ == rhs.memory_;
  }
  friend bool operator!=(Hash_Map_Allocator lhs, Hash_Map_Allocator rhs) {
    return !(lhs == rhs);
  }

  Hash_Map_Allocator select_on_container_copy_construction() { return *this; }

  using propagate_on_container_copy_assignment = std::true_type;
  using propagate_on_container_move_assignment = std::true_type;
  using propagate_on_container_swap = std::true_type;

  using is_always_empty = std::false_type;

  Memory_Resource* resource() const { return this->memory_; }

 private:
  Memory_Resource* memory_;
};

// Like std::unordered_map.
template <class Key, class Value, class Hash = Hasher<Key>>
class Hash_Map {
 private:
  using Key_Equal = std::equal_to<>;
  using Allocator = Hash_Map_Allocator<std::pair<const Key, Value>>;
  using Unordered_Map =
      std::unordered_map<Key, Value, Hash, Key_Equal, Allocator>;

 public:
  using allocator_type = Allocator;
  using const_iterator = typename Unordered_Map::const_iterator;
  using iterator = typename Unordered_Map::iterator;
  using size_type = typename Unordered_Map::size_type;
  using value_type = typename Unordered_Map::value_type;

  // TODO(strager): Require a Memory_Resource.
  explicit Hash_Map() : Hash_Map(new_delete_resource()) {}

  // Needed for AllocatorAwareContainer.
  explicit Hash_Map(const Allocator& allocator)
      : Hash_Map(allocator.resource()) {}

  explicit Hash_Map(Memory_Resource* memory) : map_(memory) {}

  // TODO(strager): Require a Memory_Resource.
  explicit Hash_Map(std::initializer_list<value_type> init)
      : Hash_Map(init, new_delete_resource()) {}

  explicit Hash_Map(std::initializer_list<value_type> init,
                    Memory_Resource* memory)
      : map_(init, /*bucket_count=*/0, memory) {}

  Hash_Map(const Hash_Map&) = default;
  // Propagates the Memory_Resource.
  Hash_Map& operator=(const Hash_Map&) = default;

  Hash_Map(Hash_Map&&) = default;
  // Propagates the Memory_Resource.
  Hash_Map& operator=(Hash_Map&&) = default;

  // Needed for AllocatorAwareContainer.
  Hash_Map(const Hash_Map& other, const Allocator& allocator)
      : map_(other.map_, allocator) {}

  // Needed for AllocatorAwareContainer.
  const Allocator& get_allocator() { return this->map_.get_allocator(); }

  template <class K>
  const_iterator find(const K& key) const {
    return this->map_.find(key);
  }
  template <class K>
  iterator find(const K& key) {
    return this->map_.find(key);
  }

  iterator begin() { return this->map_.begin(); }
  const_iterator begin() const { return this->map_.begin(); }

  iterator end() { return this->map_.end(); }
  const_iterator end() const { return this->map_.end(); }

  bool empty() const { return this->map_.empty(); }

  size_type size() const { return this->map_.size(); }

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
  Unordered_Map map_;
};

// A Hash_Map with pointer-stable values.
template <class Key, class Value, class Hash = Hasher<Key>>
using Stable_Hash_Map = Hash_Map<Key, Value, Hash>;
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
