// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/cast.h>
#include <utility>

#if defined(QLJS_THREADS_WINDOWS)
#include <process.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#endif

#if defined(QLJS_THREADS_POSIX)
#include <pthread.h>
#endif

#if QLJS_HAVE_MACH
#include <mach/mach_init.h>
#include <mach/thread_act.h>
#endif

#if QLJS_HAVE_SYS_THR_H
#include <sys/thr.h>
#endif

#if QLJS_HAVE_GETTID
#include <sys/types.h>
#endif

#if QLJS_HAVE_GETTID_SYSCALL
#include <sys/syscall.h>
#include <unistd.h>
#endif

namespace quick_lint_js {
#if defined(QLJS_THREADS_NONE)
void Mutex::lock() {}

void Mutex::unlock() {}

Condition_Variable::Condition_Variable() {}

Condition_Variable::~Condition_Variable() {}

void Condition_Variable::wait(std::unique_lock<Mutex>&) {
  // For single-threaded programs, wait() would hang. Let's crash instead.
  QLJS_CRASH_ALLOWING_CORE_DUMP();
}

void Condition_Variable::wait_raw(Mutex*) {
  // For single-threaded programs, wait_raw() would hang. Let's crash instead.
  QLJS_CRASH_ALLOWING_CORE_DUMP();
}

void Condition_Variable::notify_one() {}

void Condition_Variable::notify_all() {}
#endif

#if defined(QLJS_THREADS_WINDOWS)
Thread::Thread() : thread_handle_(nullptr) {}

Thread& Thread::operator=(Thread&& other) {
  QLJS_ASSERT(!this->thread_handle_.valid());

  this->thread_handle_ = std::move(other.thread_handle_);
  this->closure_ = std::exchange(other.closure_, nullptr);
  this->destroy_closure_ = std::exchange(other.destroy_closure_, nullptr);

  QLJS_ASSERT(!other.thread_handle_.valid());
  return *this;
}

Thread::~Thread() { QLJS_ASSERT(!this->thread_handle_.valid()); }

bool Thread::joinable() const { return this->thread_handle_.valid(); }

void Thread::join() {
  QLJS_ASSERT(this->joinable());
  ::DWORD rc = ::WaitForSingleObject(this->thread_handle_.get(), INFINITE);
  QLJS_ALWAYS_ASSERT(rc != WAIT_FAILED);
  QLJS_ASSERT(rc == WAIT_OBJECT_0);
  this->thread_handle_.close();

  this->destroy_closure_(this->closure_);
  this->closure_ = nullptr;
  this->destroy_closure_ = nullptr;
}

void Thread::terminate() {
  QLJS_ASSERT(this->joinable());
  ::BOOL ok = ::TerminateThread(this->thread_handle_.get(), 0);
  QLJS_ASSERT(ok);
}

void Thread::start(OS_Thread_Routine thread_routine, void* user_data) {
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
      Windows_Handle_File(reinterpret_cast<::HANDLE>(thread_handle));
}

void Mutex::lock() { ::AcquireSRWLockExclusive(&this->mutex_handle_); }

void Mutex::unlock() { ::ReleaseSRWLockExclusive(&this->mutex_handle_); }

Condition_Variable::Condition_Variable() {
  ::InitializeConditionVariable(&this->cond_var_handle_);
}

Condition_Variable::~Condition_Variable() = default;

void Condition_Variable::wait(std::unique_lock<Mutex>& lock) {
  this->wait_raw(lock.mutex());
}

void Condition_Variable::wait_raw(Mutex* lock) {
  ::BOOL ok =
      ::SleepConditionVariableSRW(&this->cond_var_handle_, &lock->mutex_handle_,
                                  /*dwMilliseconds=*/INFINITE, /*Flags=*/0);
  QLJS_ALWAYS_ASSERT(ok);
}

void Condition_Variable::notify_one() {
  ::WakeConditionVariable(&this->cond_var_handle_);
}

void Condition_Variable::notify_all() {
  ::WakeAllConditionVariable(&this->cond_var_handle_);
}
#endif

#if defined(QLJS_THREADS_POSIX)
Thread::Thread() = default;

Thread& Thread::operator=(Thread&& other) {
  QLJS_ASSERT(!this->thread_is_running_);

  std::memcpy(&this->thread_handle_, &other.thread_handle_,
              sizeof(this->thread_handle_));
  this->thread_is_running_ = std::exchange(other.thread_is_running_, false);
  this->closure_ = std::exchange(other.closure_, nullptr);
  this->destroy_closure_ = std::exchange(other.destroy_closure_, nullptr);

  QLJS_ASSERT(!other.thread_is_running_);
  return *this;
}

Thread::~Thread() { QLJS_ASSERT(!this->thread_is_running_); }

bool Thread::joinable() const { return this->thread_is_running_; }

void Thread::join() {
  QLJS_ASSERT(this->joinable());

  int rc = ::pthread_join(this->thread_handle_, /*retval=*/nullptr);
  QLJS_ALWAYS_ASSERT(rc == 0);

  this->destroy_closure_(this->closure_);
  this->closure_ = nullptr;
  this->destroy_closure_ = nullptr;

  this->thread_is_running_ = false;
}

void Thread::start(OS_Thread_Routine thread_routine, void* user_data) {
  QLJS_ASSERT(!this->thread_is_running_);

  int rc = ::pthread_create(/*thread=*/&this->thread_handle_,
                            /*attr=*/nullptr,
                            /*start_routine=*/thread_routine,
                            /*arg=*/user_data);
  QLJS_ALWAYS_ASSERT(rc == 0);
  this->thread_is_running_ = true;
}

void Mutex::lock() {
  int rc = ::pthread_mutex_lock(&this->mutex_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

void Mutex::unlock() {
  int rc = ::pthread_mutex_unlock(&this->mutex_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

Condition_Variable::Condition_Variable() {
  int rc = ::pthread_cond_init(&this->cond_var_handle_, /*attr=*/nullptr);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

Condition_Variable::~Condition_Variable() {
  int rc = ::pthread_cond_destroy(&this->cond_var_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

void Condition_Variable::wait(std::unique_lock<Mutex>& lock) {
  this->wait_raw(lock.mutex());
}

void Condition_Variable::wait_raw(Mutex* lock) {
  int rc = ::pthread_cond_wait(&this->cond_var_handle_, &lock->mutex_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

void Condition_Variable::notify_one() {
  int rc = ::pthread_cond_signal(&this->cond_var_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}

void Condition_Variable::notify_all() {
  int rc = ::pthread_cond_broadcast(&this->cond_var_handle_);
  QLJS_ALWAYS_ASSERT(rc == 0);
}
#endif

std::uint64_t get_current_thread_id() {
#if QLJS_HAVE_WINDOWS_H
  return ::GetCurrentThreadId();
#elif QLJS_HAVE_GETTID
  pid_t tid = ::gettid();
  if (tid < 0) {
    // NOTE(strager): We can't log an error message here because our logging
    // calls this function.
    return 0;
  }
  return narrow_cast<std::uint64_t>(tid);
#elif QLJS_HAVE_GETTID_SYSCALL
  long rc = ::syscall(__NR_gettid);
  if (rc < 0) {
    // NOTE(strager): We can't log an error message here because our logging
    // calls this function.
    return 0;
  }
  return narrow_cast<std::uint64_t>(rc);
#elif QLJS_HAVE_MACH
  ::thread_identifier_info_data_t info;
  ::mach_msg_type_number_t info_count = THREAD_IDENTIFIER_INFO_COUNT;
  ::kern_return_t rc =
      ::thread_info(::mach_thread_self(), THREAD_IDENTIFIER_INFO,
                    reinterpret_cast<thread_info_t>(&info), &info_count);
  QLJS_ALWAYS_ASSERT(rc == KERN_SUCCESS);
  QLJS_ALWAYS_ASSERT(info_count == THREAD_IDENTIFIER_INFO_COUNT);
  return info.thread_id;
#elif QLJS_HAVE_SYS_THR_H
  long thread_id;
  int rc = ::thr_self(&thread_id);
  QLJS_ALWAYS_ASSERT(rc == 0);
  return narrow_cast<std::uint64_t>(thread_id);
#else
#warning "Unsupported platform"
  return 0;
#endif
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
