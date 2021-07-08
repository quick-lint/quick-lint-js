// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <chrono>
#include <condition_variable>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <mutex>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/event-loop.h>
#include <quick-lint-js/pipe.h>
#include <quick-lint-js/spy-lsp-message-parser.h>
#include <thread>

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using namespace std::literals::chrono_literals;

namespace quick_lint_js {
namespace {
void write_full_message(platform_file_ref, string8_view);

struct spy_event_loop : public event_loop<spy_event_loop> {
  explicit spy_event_loop(platform_file_ref pipe) : pipe_(pipe) {}

  platform_file_ref get_readable_pipe() const { return this->pipe_; }

  void append(string8_view data) {
    std::unique_lock lock(this->mutex_);
    this->read_data_.append(data);
    this->new_data_.notify_all();
  }

  string8 get_read_data() {
    std::unique_lock lock(this->mutex_);
    return this->read_data_;
  }

  template <class Func>
  void wait_until_data(Func&& predicate) {
    std::unique_lock lock(this->mutex_);
    this->new_data_.wait(lock, [this, &predicate]() -> bool {
      return predicate(this->read_data_);
    });
  }

#if QLJS_HAVE_KQUEUE || QLJS_HAVE_POLL
  std::optional<posix_fd_file_ref> get_pipe_write_fd() const {
    return this->pipe_write_fd_;
  }

  template <class... Args>
  void on_pipe_write_event(Args&&... args) {
    this->pipe_write_event_callback_(std::forward<Args>(args)...);
  }

  template <class Func>
  void set_pipe_write(posix_fd_file_ref fd, Func on_event) {
    this->pipe_write_fd_ = fd;
    this->pipe_write_event_callback_ = on_event;
  }
#endif

#if QLJS_HAVE_KQUEUE
  void on_fs_changed_kevents() {}
#endif

#if QLJS_HAVE_INOTIFY
  std::optional<posix_fd_file_ref> get_inotify_fd() const {
    return std::nullopt;
  }

  void on_fs_changed_event(const ::pollfd&) {}
#endif

#if defined(_WIN32)
  void on_fs_changed_event(::OVERLAPPED*,
                           [[maybe_unused]] ::DWORD number_of_bytes_transferred,
                           [[maybe_unused]] ::DWORD error) {}
#endif

 private:
  platform_file_ref pipe_;

  std::mutex mutex_;
  std::condition_variable new_data_;

  // Protected by mutex_:
  string8 read_data_;

#if QLJS_HAVE_KQUEUE || QLJS_HAVE_POLL
  std::optional<posix_fd_file_ref> pipe_write_fd_;
#endif

#if QLJS_HAVE_KQUEUE
  std::function<void(const struct ::kevent&)> pipe_write_event_callback_;
#elif QLJS_HAVE_POLL
  std::function<void(const ::pollfd&)> pipe_write_event_callback_;
#endif
};

class test_event_loop : public ::testing::Test {
 public:
  pipe_fds pipe = make_pipe_for_event_loop();
  spy_event_loop loop{this->pipe.reader.ref()};

 private:
  static pipe_fds make_pipe_for_event_loop() {
    pipe_fds pipe = make_pipe();
#if QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING
    pipe.reader.set_pipe_non_blocking();
#endif
    return pipe;
  }
};

TEST_F(test_event_loop, stops_on_pipe_read_eof) {
  this->pipe.writer.close();

  this->loop.run();
  // run() should terminate.
}

TEST_F(test_event_loop, reads_data_in_pipe_buffer) {
  write_full_message(this->pipe.writer.ref(), u8"Hi");
  this->pipe.writer.close();

  this->loop.run();

  EXPECT_EQ(this->loop.get_read_data(), u8"Hi");
}

TEST_F(test_event_loop, reads_many_messages) {
  std::thread writer_thread([this]() {
    write_full_message(this->pipe.writer.ref(), u8"first");
    this->loop.wait_until_data(
        [](const string8& data) -> bool { return data == u8"first"; });

    write_full_message(this->pipe.writer.ref(), u8"SECOND");
    this->loop.wait_until_data(
        [](const string8& data) -> bool { return data == u8"firstSECOND"; });

    this->pipe.writer.close();
  });

  this->loop.run();

  writer_thread.join();
  EXPECT_EQ(this->loop.get_read_data(), u8"firstSECOND");
}

#if QLJS_HAVE_KQUEUE || QLJS_HAVE_POLL
TEST_F(test_event_loop, signals_writable_pipe) {
  bool called = false;
  this->loop.set_pipe_write(this->pipe.writer.ref(),
                            [this, &called](const auto& event) {
                              called = true;
#if QLJS_HAVE_KQUEUE
                              EXPECT_EQ(event.ident, this->pipe.writer.get());
                              EXPECT_EQ(event.filter, EVFILT_WRITE);
#elif QLJS_HAVE_POLL
                              EXPECT_EQ(event.fd, this->pipe.writer.get());
                              EXPECT_TRUE(event.revents & POLLOUT);
#endif
                              // Stop event_loop::run.
                              this->pipe.writer.close();
                            });

  this->loop.run();
  EXPECT_TRUE(called);
}

TEST_F(test_event_loop, does_not_write_to_unwritable_pipe) {
  // Make a pipe such that POLLOUT will not be signalled.
  pipe_fds full_pipe = make_pipe();
  full_pipe.writer.set_pipe_non_blocking();
  write_full_message(full_pipe.writer.ref(),
                     string8(full_pipe.writer.get_pipe_buffer_size(), 'x'));

  this->loop.set_pipe_write(full_pipe.writer.ref(), [](const auto&) {
    ADD_FAILURE() << "on_pipe_write_event should not be called";
  });

  std::thread writer_thread([this]() {
    std::this_thread::sleep_for(10ms);
    // Interrupt event_loop::run on the main thread.
    this->pipe.writer.close();
  });
  this->loop.run();

  writer_thread.join();
}
#endif

void write_full_message(platform_file_ref file, string8_view message) {
  std::optional<int> bytes_written =
      file.write(message.data(), narrow_cast<int>(message.size()));
  EXPECT_TRUE(bytes_written.has_value()) << file.get_last_error_message();
  EXPECT_EQ(bytes_written, message.size());
}
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
