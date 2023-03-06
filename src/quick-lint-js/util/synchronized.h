// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_SYNCHRONIZED_H
#define QUICK_LINT_JS_UTIL_SYNCHRONIZED_H

#include <quick-lint-js/port/thread.h>

namespace quick_lint_js {
template <class Data>
class lock_ptr;

// A wrapper around data and a mutex.
//
// Inspired by folly::Synchronized.
template <class Data>
class synchronized {
 public:
  // Acquire the mutex. When the returned lock_ptr is destructed, the mutex is
  // released.
  lock_ptr<Data> lock();

 private:
  mutex mutex_;
  Data data_;

  friend class lock_ptr<Data>;
};

// Like std::lock_guard, but with additional access to lock-protected data.
//
// To create a lock_ptr, see the synchronized class.
template <class Data>
class lock_ptr {
 private:
  // Do not call directly. Call synchronized<Data>::lock instead.
  explicit lock_ptr(synchronized<Data>* synchronized)
      : synchronized_(synchronized) {
    this->lock();
  }

 public:
  lock_ptr(const lock_ptr&) = delete;
  lock_ptr& operator=(const lock_ptr&) = delete;

  // Releases the lock.
  ~lock_ptr() { this->unlock(); }

  // Gives access to the lock-protected data.
  Data& operator*() { return this->synchronized_->data_; }
  Data* operator->() { return &this->synchronized_->data_; }

 private:
  void lock() { return this->synchronized_->mutex_.lock(); }
  void unlock() { return this->synchronized_->mutex_.unlock(); }

  synchronized<Data>* synchronized_;

  friend class synchronized<Data>;
};

template <class Data>
inline lock_ptr<Data> synchronized<Data>::lock() {
  return lock_ptr<Data>(this);
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
