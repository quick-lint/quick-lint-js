// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_SYNCHRONIZED_H
#define QUICK_LINT_JS_UTIL_SYNCHRONIZED_H

#include <quick-lint-js/port/thread.h>

namespace quick_lint_js {
template <class Data>
class Lock_Ptr;

// A wrapper around data and a mutex.
//
// Inspired by folly::Synchronized.
template <class Data>
class Synchronized {
 public:
  // Acquire the mutex. When the returned lock_ptr is destructed, the mutex is
  // released.
  Lock_Ptr<Data> lock();

  // get_unsafe does not acquire the mutex.
  Data* get_without_lock_unsafe() { return &this->data_; }

 private:
  Mutex mutex_;
  Data data_;

  friend class Lock_Ptr<Data>;
};

// Like std::lock_guard, but with additional access to lock-protected data.
//
// To create a Lock_Ptr, see the Synchronized class.
template <class Data>
class Lock_Ptr {
 private:
  // Do not call directly. Call Synchronized<Data>::lock instead.
  explicit Lock_Ptr(Synchronized<Data>* synchronized)
      : synchronized_(synchronized) {
    this->lock();
  }

 public:
  Lock_Ptr(const Lock_Ptr&) = delete;
  Lock_Ptr& operator=(const Lock_Ptr&) = delete;

  // Releases the lock.
  ~Lock_Ptr() { this->unlock(); }

  // Gives access to the lock-protected data.
  Data& operator*() { return this->synchronized_->data_; }
  Data* operator->() { return &this->synchronized_->data_; }

  Mutex* get_mutex_unsafe() { return &this->synchronized_->mutex_; }

 private:
  void lock() { return this->synchronized_->mutex_.lock(); }
  void unlock() { return this->synchronized_->mutex_.unlock(); }

  Synchronized<Data>* synchronized_;

  friend class Synchronized<Data>;
};

template <class Data>
inline Lock_Ptr<Data> Synchronized<Data>::lock() {
  return Lock_Ptr<Data>(this);
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
