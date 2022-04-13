// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/thread.h>
#include <utility>

#if defined(QLJS_THREADS_WINDOWS)
#include <process.h>
#include <quick-lint-js/windows.h>
#endif

#if defined(QLJS_THREADS_POSIX)
#include <pthread.h>
#endif

namespace quick_lint_js {
#if defined(QLJS_THREADS_WINDOWS)
thread::thread() noexcept : thread_handle_(nullptr) {}

thread& thread::operator=(thread&& other) {
  QLJS_ASSERT(!this->thread_handle_.valid());

  this->thread_handle_ = std::move(other.thread_handle_);

  QLJS_ASSERT(!other.thread_handle_.valid());
  return *this;
}

thread::~thread() { QLJS_ASSERT(!this->thread_handle_.valid()); }

void thread::join() {
  QLJS_ASSERT(this->thread_handle_.valid());
  ::DWORD rc = ::WaitForSingleObject(this->thread_handle_.get(), INFINITE);
  QLJS_ALWAYS_ASSERT(rc != WAIT_FAILED);
  QLJS_ASSERT(rc == WAIT_OBJECT_0);
  this->thread_handle_.close();
}

void thread::start(os_thread_routine thread_routine, void* user_data) {
  QLJS_ASSERT(!this->thread_handle_.valid());

  std::uintptr_t thread_handle = ::_beginthreadex(
      /*security=*/nullptr,
      /*stack_size=*/0,
      /*start_address=*/thread_routine,
      /*arglist=*/user_data,
      /*initflag=*/0,
      /*thrdaddr=*/nullptr);
  QLJS_ALWAYS_ASSERT(thread_handle != 0);
  this->thread_handle_ =
      windows_handle_file(reinterpret_cast<::HANDLE>(thread_handle));
}

void mutex::lock() { ::AcquireSRWLockExclusive(&this->mutex_handle_); }

void mutex::unlock() { ::ReleaseSRWLockExclusive(&this->mutex_handle_); }

condition_variable::condition_variable() {
  ::InitializeConditionVariable(&this->cond_var_handle_);
}

condition_variable::~condition_variable() = default;

void condition_variable::wait(std::unique_lock<mutex>& lock) {
  ::BOOL ok = ::SleepConditionVariableSRW(
      &this->cond_var_handle_, &lock.mutex()->mutex_handle_,
      /*dwMilliseconds=*/INFINITE, /*Flags=*/0);
  QLJS_ALWAYS_ASSERT(ok);
}

void condition_variable::notify_one() {
  ::WakeConditionVariable(&this->cond_var_handle_);
}

void condition_variable::notify_all() {
  ::WakeAllConditionVariable(&this->cond_var_handle_);
}
#endif

#if defined(QLJS_THREADS_POSIX)
thread::thread() noexcept = default;

thread& thread::operator=(thread&& other) {
  QLJS_ASSERT(!this->thread_is_running_);

  std::memcpy(&this->thread_handle_, &other.thread_handle_,
              sizeof(this->thread_handle_));
  this->thread_is_running_ = std::exchange(other.thread_is_running_, false);

  QLJS_ASSERT(!other.thread_is_running_);
  return *this;
}

thread::~thread() { QLJS_ASSERT(!this->thread_is_running_); }

void thread::join() {
  QLJS_ASSERT(this->thread_is_running_);

  int rc = ::pthread_join(this->thread_handle_, /*retval=*/nullptr);
  QLJS_ALWAYS_ASSERT(rc == 0);
  this->thread_is_running_ = false;
}

void thread::start(os_thread_routine thread_routine, void* user_data) {
  QLJS_ASSERT(!this->thread_is_running_);

  int rc = ::pthread_create(/*thread=*/&this->thread_handle_,
                            /*attr=*/nullptr,
                            /*start_routine=*/thread_routine,
                            /*arg=*/user_data);
  QLJS_ALWAYS_ASSERT(rc == 0);
  this->thread_is_running_ = true;
}

void mutex::lock() {
  int rc = ::pthread_mutex_lock(&this->mutex_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

void mutex::unlock() {
  int rc = ::pthread_mutex_unlock(&this->mutex_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

condition_variable::condition_variable() {
  int rc = ::pthread_cond_init(&this->cond_var_handle_, /*attr=*/nullptr);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

condition_variable::~condition_variable() {
  int rc = ::pthread_cond_destroy(&this->cond_var_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

void condition_variable::wait(std::unique_lock<mutex>& lock) {
  int rc = ::pthread_cond_wait(&this->cond_var_handle_,
                               &lock.mutex()->mutex_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

void condition_variable::notify_one() {
  int rc = ::pthread_cond_signal(&this->cond_var_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

void condition_variable::notify_all() {
  int rc = ::pthread_cond_broadcast(&this->cond_var_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}
#endif
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
