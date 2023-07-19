// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// This module wraps OS threading primitives.
//
// GCC+MinGW's threading implementation on Windows is heavy-weight, increasing
// binary size. Our classes use Win32 APIs directly, avoiding bloat.

#ifndef QUICK_LINT_JS_PORT_THREAD_H
#define QUICK_LINT_JS_PORT_THREAD_H

#include <cstdint>
#include <memory>
#include <mutex>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/warning.h>
#include <utility>

#if !QLJS_HAVE_THREADS
#define QLJS_THREADS_NONE
#elif QLJS_HAVE_WINDOWS_H
#define QLJS_THREADS_WINDOWS
#elif QLJS_HAVE_PTHREAD_H
#define QLJS_THREADS_POSIX
#else
#error "Unsupported platform"
#endif

#if defined(QLJS_THREADS_WINDOWS)
#include <quick-lint-js/io/file-handle.h>
#endif

#if defined(QLJS_THREADS_POSIX)
#include <pthread.h>
#endif

namespace quick_lint_js {
template <class Data>
class Lock_Ptr;

#if QLJS_HAVE_THREADS
// A reimplementation of std::thread.
class Thread {
#if defined(QLJS_THREADS_WINDOWS)
  using OS_Thread_Routine = unsigned(__stdcall *)(void *user_data);
#elif defined(QLJS_THREADS_POSIX)
  using OS_Thread_Routine = void *(*)(void *user_data);
#endif

 public:
  explicit Thread() noexcept;

  template <class Func>
  explicit Thread(Func &&func) : Thread() {
    this->start(std::forward<Func>(func));
  }

  Thread(const Thread &) = delete;
  Thread &operator=(const Thread &) = delete;

  Thread(Thread &&) = delete;  // TODO(strager)
  Thread &operator=(Thread &&);

  ~Thread();

  template <class Func>
  void start(Func &&func) {
    std::unique_ptr<Thread_Closure<Func>> closure =
        std::make_unique<Thread_Closure<Func>>(std::forward<Func>(func));
    this->start(Thread_Closure<Func>::run, closure.get());
    closure.release();
  }

  bool joinable() const noexcept;
  void join();

 private:
  void start(OS_Thread_Routine thread_routine, void *user_data);

  template <class Func>
  struct Thread_Closure {
    Func func;

    explicit Thread_Closure(Func &&func) : func(std::move(func)) {}

    static
#if defined(QLJS_THREADS_WINDOWS)
        unsigned __stdcall
#elif defined(QLJS_THREADS_POSIX)
        void *
#endif
        run(void *user_data) {
      std::unique_ptr<Thread_Closure> self(
          static_cast<Thread_Closure *>(user_data));
      self->func();
#if defined(QLJS_THREADS_WINDOWS)
      return 0;
#elif defined(QLJS_THREADS_POSIX)
      return nullptr;
#endif
    }
  };

#if defined(QLJS_THREADS_WINDOWS)
  Windows_Handle_File thread_handle_;
#elif defined(QLJS_THREADS_POSIX)
  ::pthread_t thread_handle_;
  bool thread_is_running_ = false;
#endif
};
#endif

// A reimplementation of std::mutex.
class Mutex {
 public:
  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")
  explicit constexpr Mutex() noexcept
#if defined(QLJS_THREADS_WINDOWS)
      : mutex_handle_(SRWLOCK_INIT)
#elif defined(QLJS_THREADS_POSIX)
      : mutex_handle_(PTHREAD_MUTEX_INITIALIZER)
#endif
  {
  }
  QLJS_WARNING_POP

  // This destructor is technically constexpr, but the constexpr keyword is not
  // allowed in C++17.
  /*constexpr*/ ~Mutex() = default;

  Mutex(const Mutex &) = delete;
  Mutex(Mutex &&) = delete;

  void lock();
  void unlock();

 private:
#if defined(QLJS_THREADS_WINDOWS)
  ::SRWLOCK mutex_handle_;
#elif defined(QLJS_THREADS_POSIX)
  ::pthread_mutex_t mutex_handle_;
#endif

  friend class Condition_Variable;
};

// A reimplementation of std::condition_variable.
class Condition_Variable {
 public:
  explicit Condition_Variable();
  ~Condition_Variable();

  Condition_Variable(const Condition_Variable &) = delete;
  Condition_Variable(Condition_Variable &&) = delete;

  template <class Lock, class Predicate>
  void wait(Lock &lock, Predicate stop_waiting) {
    while (!stop_waiting()) {
      this->wait(lock);
    }
  }

  void wait(std::unique_lock<Mutex> &);

  template <class Data>
  void wait(Lock_Ptr<Data> &locked) {
    this->wait_raw(locked.get_mutex_unsafe());
  }

  void wait_raw(Mutex *);

  void notify_one();
  void notify_all();

 private:
#if defined(QLJS_THREADS_WINDOWS)
  ::CONDITION_VARIABLE cond_var_handle_;
#elif defined(QLJS_THREADS_POSIX)
  ::pthread_cond_t cond_var_handle_;
#endif
};

std::uint64_t get_current_thread_id() noexcept;
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
