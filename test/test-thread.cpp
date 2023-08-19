// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <chrono>
#include <gtest/gtest.h>
#include <iostream>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>
#include <thread>

#if defined(_WIN32)
#include <quick-lint-js/port/windows-error.h>
#include <quick-lint-js/port/windows.h>
#endif

using namespace std::literals::chrono_literals;

namespace quick_lint_js {
namespace {
TEST(Test_Thread, print_thread_id) {
  // This is a manual test.
  std::cerr << "main thread ID: " << get_current_thread_id() << '\n';
}

TEST(Test_Thread, thread_id_is_stable) {
  std::uint64_t id = get_current_thread_id();
  std::uint64_t id2 = get_current_thread_id();
  EXPECT_EQ(id, id2) << "thread ID should not change between calls";
}

#if QLJS_HAVE_THREADS
TEST(Test_Thread, thread_ids_differ_between_threads) {
  std::uint64_t main_id = get_current_thread_id();
  std::uint64_t other_id;

  Thread other_thread([&] { other_id = get_current_thread_id(); });
  other_thread.join();

  EXPECT_NE(main_id, other_id) << "thread IDs should differ";
}
#endif

#if defined(_WIN32)
TEST(Test_Thread, terminate_interrupts_wait_for_single_object_SLOW) {
  Windows_Handle_File event(::CreateEventA(
      /*lpEventAttributes=*/nullptr,
      /*bManualReset=*/true, /*bInitialState=*/false,
      /*lpName=*/nullptr));
  ASSERT_TRUE(event.valid()) << windows_error_message(::GetLastError());

  std::atomic<bool> thread_started = false;
  std::atomic<bool> wait_returned = false;
  Thread other_thread([&] {
    thread_started = true;
    (void)::WaitForSingleObject(event.get(), INFINITE);
    wait_returned = true;
  });

  // Wait for other_thread to call WaitForSingleObject.
  while (!thread_started) {
    std::this_thread::sleep_for(1ms);
  }
  std::this_thread::sleep_for(1ms);

  other_thread.terminate();
  other_thread.join();
  EXPECT_FALSE(wait_returned) << "thread should have stopped without returning "
                                 "from WaitForSingleObject";
}

TEST(Test_Thread, terminate_does_nothing_for_a_stopped_thread_SLOW) {
  Windows_Handle_File event(::CreateEventA(
      /*lpEventAttributes=*/nullptr,
      /*bManualReset=*/true, /*bInitialState=*/false,
      /*lpName=*/nullptr));
  ASSERT_TRUE(event.valid()) << windows_error_message(::GetLastError());

  std::atomic<bool> thread_routine_finished = false;
  Thread other_thread([&] { thread_routine_finished = true; });

  // Wait for other_thread to finish.
  while (!thread_routine_finished) {
    std::this_thread::sleep_for(1ms);
  }
  // Wait for possibly-expensive thread cleanup by the OS.
  std::this_thread::sleep_for(10ms);

  // Thread::terminate should not crash internally.
  other_thread.terminate();
  EXPECT_TRUE(other_thread.joinable());
  // Thread::join should not crash internally.
  other_thread.join();
}
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
