// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/pipe-reader.h>
#include <quick-lint-js/pipe.h>
#include <quick-lint-js/spy-lsp-message-parser.h>
#include <thread>

using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
void write_full_message(platform_file_ref, string8_view);

class test_lsp_pipe_reader : public ::testing::Test {
 public:
  pipe_fds pipe = make_pipe();
  pipe_reader<spy_lsp_message_parser> reader{this->pipe.reader.ref()};
  spy_lsp_message_parser &parser = this->reader.parser();
};

TEST_F(test_lsp_pipe_reader, stops_on_eof) {
  this->pipe.writer.close();

  this->reader.run();
  // run() should terminate.
}

TEST_F(test_lsp_pipe_reader, reads_data_in_pipe_buffer) {
  write_full_message(this->pipe.writer.ref(), u8"Content-Length: 2\r\n\r\nHi");
  this->pipe.writer.close();

  reader.run();

  EXPECT_THAT(this->parser.messages(), ElementsAre(u8"Hi"));
}

TEST_F(test_lsp_pipe_reader, reads_many_messages) {
  std::thread writer_thread([this]() {
    write_full_message(this->pipe.writer.ref(),
                       u8"Content-Length: 5\r\n\r\nfirst");
    parser.wait_until_messages(
        [](const std::vector<string8> &messages) -> bool {
          return messages.size() >= 1;
        });

    write_full_message(this->pipe.writer.ref(),
                       u8"Content-Length: 6\r\n\r\nsecond");
    parser.wait_until_messages(
        [](const std::vector<string8> &messages) -> bool {
          return messages.size() >= 2;
        });

    this->pipe.writer.close();
  });

  this->reader.run();

  writer_thread.join();
  EXPECT_THAT(this->parser.messages(), ElementsAre(u8"first", u8"second"));
}

TEST_F(test_lsp_pipe_reader,
       reads_data_in_pipe_buffer_as_it_arrives_before_writer_close) {
  std::thread writer_thread([this]() {
    EXPECT_THAT(this->parser.messages(), IsEmpty());

    write_full_message(this->pipe.writer.ref(),
                       u8"Content-Length: 5\r\n\r\nfirst");

    parser.wait_until_messages(
        [](const std::vector<string8> &messages) -> bool {
          return !messages.empty();
        });
    EXPECT_THAT(this->parser.messages(), ElementsAre(u8"first"));

    this->pipe.writer.close();
  });

  this->reader.run();

  writer_thread.join();
}

void write_full_message(platform_file_ref file, string8_view message) {
  std::optional<int> bytes_written =
      file.write(message.data(), narrow_cast<int>(message.size()));
  EXPECT_TRUE(bytes_written.has_value()) << file.get_last_error_message();
  EXPECT_EQ(bytes_written, message.size());
}
}
}
