// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstring>
#include <future>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/lsp-message-parser.h>
#include <quick-lint-js/lsp-pipe-writer.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/pipe-reader.h>
#include <quick-lint-js/pipe.h>
#include <thread>

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using namespace std::literals::chrono_literals;

namespace quick_lint_js {
namespace {
std::size_t pipe_buffer_size(platform_file_ref);

class test_lsp_pipe_writer : public ::testing::Test {
 public:
  explicit test_lsp_pipe_writer() { this->pipe.writer.set_pipe_non_blocking(); }

  pipe_fds pipe = make_pipe();
  // @@@ hack: call set_pipe_non_blocking before ctor
  lsp_pipe_writer writer {(this->pipe.writer.set_pipe_non_blocking(), this->pipe.writer.ref())};
};

byte_buffer byte_buffer_of(string8_view data) {
  byte_buffer bb;
  bb.append_copy(data);
  return bb;
}

TEST_F(test_lsp_pipe_writer, small_message_includes_content_length) {
  this->writer.send_message(byte_buffer_of(u8"hi"));
  this->writer.flush();
  this->pipe.writer.close();

  read_file_result data = read_file("<pipe>", this->pipe.reader.ref());
  ASSERT_TRUE(data.ok()) << data.error;
  EXPECT_EQ(data.content, u8"Content-Length: 2\r\n\r\nhi");
}

TEST_F(test_lsp_pipe_writer, large_message_sends_fully) {
  std::future<read_file_result> data_future = std::async(
      std::launch::async,
      [this]() { return read_file("<pipe>", this->pipe.reader.ref()); });

  string8 message =
      u8"[" + string8(pipe_buffer_size(this->pipe.writer.ref()) * 3, u8'x') +
      u8"]";
  this->writer.send_message(byte_buffer_of(message));
  this->writer.flush();
  this->pipe.writer.close();

  read_file_result data = data_future.get();
  ASSERT_TRUE(data.ok()) << data.error;

  string8_view data_content = data.content.string_view();
  EXPECT_NE(data_content.find(message), data_content.npos);
}

TEST_F(test_lsp_pipe_writer, large_message_with_no_reader_does_not_block) {
  string8 message =
      u8"[" + string8(pipe_buffer_size(this->pipe.writer.ref()) * 3, u8'x') +
      u8"]";
  this->writer.send_message(byte_buffer_of(message));  // Shouldn't block.
  std::this_thread::sleep_for(std::chrono::seconds(10)); // @@@
  static std::promise<string8> received_message_promise;
  received_message_promise = std::promise<string8>();

  // Read exactly as many bytes as needed to parse the message, then set
  // received_message_promise.
  std::future<void> receiving_thread = std::async(std::launch::async, [this] {
    struct message_handler : lsp_message_parser<message_handler> {
      void message_parsed(string8_view message_content) {
        received_message_promise.set_value(string8(message_content));
      }
    };
    pipe_reader<message_handler> reader(this->pipe.reader.ref());
    reader.run();
  });

  string8 data = received_message_promise.get_future().get();
  this->pipe.writer.close();  // Stop receiving_thread ASAP.
  EXPECT_EQ(data, message);
}

std::size_t pipe_buffer_size([[maybe_unused]] platform_file_ref pipe) {
#if QLJS_HAVE_F_GETPIPE_SZ
  int size = ::fcntl(pipe.get(), F_GETPIPE_SZ);
  EXPECT_NE(size, -1);
  return narrow_cast<std::size_t>(size);
#elif defined(__APPLE__)
  // See BIG_PIPE_SIZE in <xnu>/bsd/sys/pipe.h.
  return 65536;
#elif defined(_WIN32)
  DWORD outBufferSize = 0;
  EXPECT_TRUE(::GetNamedPipeInfo(pipe.get(), /*lpFlags=*/nullptr,
                                 &outBufferSize, /*lpInBufferSize=*/nullptr,
                                 /*lpMaxInstances=*/nullptr))
      << windows_handle_file::get_last_error_message();
  return outBufferSize;
#else
#error "Unknown platform"
#endif
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
