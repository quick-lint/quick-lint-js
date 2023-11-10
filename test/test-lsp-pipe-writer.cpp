// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No pipes on the web.
#else

#include <future>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/lsp/lsp-message-parser.h>
#include <quick-lint-js/lsp/lsp-pipe-writer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/util/cast.h>
#include <thread>

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

namespace quick_lint_js {
namespace {
class Test_LSP_Pipe_Writer : public ::testing::Test {
 public:
  Pipe_FDs pipe = make_pipe_for_pipe_writer();
  LSP_Pipe_Writer writer{this->pipe.writer.ref()};

 private:
  static Pipe_FDs make_pipe_for_pipe_writer() {
    Pipe_FDs pipe = make_pipe();
#if !QLJS_PIPE_WRITER_SEPARATE_THREAD
    pipe.writer.set_pipe_non_blocking();
#endif
    return pipe;
  }
};

Byte_Buffer byte_buffer_of(String8_View data) {
  Byte_Buffer bb;
  bb.append_copy(data);
  return bb;
}

TEST_F(Test_LSP_Pipe_Writer, small_message_includes_content_length) {
  this->writer.send_message(byte_buffer_of(u8"hi"_sv));
  this->writer.flush();
  this->pipe.writer.close();

  Result<Padded_String, Read_File_IO_Error> data =
      read_file("<pipe>", this->pipe.reader.ref());
  ASSERT_TRUE(data.ok()) << data.error().to_string();
  EXPECT_EQ(*data, u8"Content-Length: 2\r\n\r\nhi"_sv);
}
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
