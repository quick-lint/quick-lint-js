// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <boost/leaf/capture.hpp>
#include <boost/leaf/context.hpp>
#include <boost/leaf/handle_errors.hpp>
#include <boost/leaf/result.hpp>
#include <condition_variable>
#include <cstddef>
#include <cstring>
#include <future>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <mutex>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/pipe-writer.h>
#include <quick-lint-js/pipe.h>
#include <quick-lint-js/sloppy-result.h>
#include <thread>

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using namespace std::literals::chrono_literals;

namespace quick_lint_js {
namespace {
class test_pipe_writer : public ::testing::Test {
 public:
  pipe_fds pipe = make_pipe_for_pipe_writer();
  pipe_writer writer{this->pipe.writer.ref()};

 private:
  static pipe_fds make_pipe_for_pipe_writer() {
    pipe_fds pipe = make_pipe();
#if !QLJS_PIPE_WRITER_SEPARATE_THREAD
    pipe.writer.set_pipe_non_blocking();
#endif
    return pipe;
  }
};

byte_buffer byte_buffer_of(string8_view data) {
  byte_buffer bb;
  bb.append_copy(data);
  return bb;
}

TEST_F(test_pipe_writer, large_write_sends_fully) {
  std::future<sloppy_result<padded_string>> data_future = std::async(
      std::launch::async,
      [this] { return read_file_sloppy("<pipe>", this->pipe.reader.ref()); });

  string8 to_write =
      u8"[" + string8(this->pipe.writer.get_pipe_buffer_size() * 3, u8'x') +
      u8"]";
  this->writer.write(byte_buffer_of(to_write));
  this->writer.flush();
  this->pipe.writer.close();

  sloppy_result<padded_string> data = data_future.get();
  ASSERT_TRUE(data.ok()) << data.error();
  EXPECT_EQ(*data, to_write);
}

// pipe_reader_thread reads data from a pipe using a background thread. When
// expected_data_size bytes are received, a promise's value is set.
class pipe_reader_thread {
 public:
  void start(platform_file_ref pipe) {
    this->receiving_thread_ =
        std::async(std::launch::async, [this, pipe]() mutable -> void {
          for (;;) {
            std::array<char8, (1 << 16)> buffer;
            file_read_result read_result =
                pipe.read(buffer.data(), buffer.size());
            if (!read_result) QLJS_UNIMPLEMENTED();
            if (read_result.at_end_of_file()) {
              return;
            } else {
              std::unique_lock<std::mutex> lock(this->mutex_);
              this->received_data.append(string8_view(
                  buffer.data(),
                  narrow_cast<std::size_t>(read_result.bytes_read())));
              this->data_received_.notify_one();
            }
          }
        });
  }

  void wait_until_size(std::size_t expected_data_size) {
    std::unique_lock<std::mutex> lock(this->mutex_);
    this->data_received_.wait(
        lock, [&] { return this->received_data.size() >= expected_data_size; });
  }

  void join() { this->receiving_thread_.get(); }

  // Locked by mutex_:
  string8 received_data;

 private:
  std::mutex mutex_;
  std::condition_variable data_received_;
  std::future<void> receiving_thread_;
};

TEST_F(test_pipe_writer, large_write_with_no_reader_does_not_block) {
  string8 to_write =
      u8"[" + string8(this->pipe.writer.get_pipe_buffer_size() * 3, u8'x') +
      u8"]";
  this->writer.write(byte_buffer_of(to_write));  // Shouldn't block.

  pipe_reader_thread read_thread;
  read_thread.start(this->pipe.reader.ref());

  this->writer.flush();
  read_thread.wait_until_size(to_write.size());
  EXPECT_EQ(read_thread.received_data, to_write);

  this->pipe.writer.close();  // Stop read_thread.
  read_thread.join();
  EXPECT_EQ(read_thread.received_data, to_write)
      << "more data should not have arrived after closing the pipe";
}

TEST_F(test_pipe_writer,
       multiple_small_messages_with_no_reader_does_not_block) {
  this->writer.write(byte_buffer_of(u8"hello"));  // Shouldn't block.
  this->writer.write(byte_buffer_of(u8", "));     // Shouldn't block.
  std::this_thread::sleep_for(1ms);  // Attempt to expose a race condition.
  this->writer.write(byte_buffer_of(u8"world"));  // Shouldn't block.
  this->writer.write(byte_buffer_of(u8"!"));      // Shouldn't block.
  string8 expected_data = u8"hello, world!";

  pipe_reader_thread read_thread;
  read_thread.start(this->pipe.reader.ref());

  this->writer.flush();
  read_thread.wait_until_size(expected_data.size());
  EXPECT_EQ(read_thread.received_data, expected_data);

  this->pipe.writer.close();  // Stop read_thread.
  read_thread.join();
  EXPECT_EQ(read_thread.received_data, expected_data)
      << "more data should not have arrived after closing the pipe";
}

TEST_F(test_pipe_writer,
       multiple_large_messages_with_no_reader_does_not_block) {
  string8 xs(this->pipe.writer.get_pipe_buffer_size() * 3, u8'x');
  string8 ys(this->pipe.writer.get_pipe_buffer_size() * 2, u8'y');
  this->writer.write(byte_buffer_of(xs));  // Shouldn't block.
  std::this_thread::sleep_for(
      std::chrono::milliseconds(1));  // Attempt to expose a race condition.
  this->writer.write(byte_buffer_of(ys));  // Shouldn't block.
  static string8 expected_data;
  expected_data = xs + ys;

  pipe_reader_thread read_thread;
  read_thread.start(this->pipe.reader.ref());

  this->writer.flush();
  read_thread.wait_until_size(expected_data.size());
  EXPECT_EQ(read_thread.received_data, expected_data);

  this->pipe.writer.close();  // Stop read_thread.
  read_thread.join();
  EXPECT_EQ(read_thread.received_data, expected_data)
      << "more data should not have arrived after closing the pipe";
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
