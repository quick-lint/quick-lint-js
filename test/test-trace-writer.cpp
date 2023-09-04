// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/logging/trace-reader.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/port/source-location.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/binary-writer.h>

using ::testing::ElementsAreArray;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(Test_Trace_Writer, write_header) {
  Async_Byte_Queue data;
  Trace_Writer w(&data);
  w.write_header(Trace_Context{
      .thread_id = 0x1234,
  });

  data.commit();
  EXPECT_THAT(data.take_committed_string8(),
              ElementsAreArray<std::uint8_t>({
                  // clang-format off
                  // CTF magic
                  0xc1, 0x1f, 0xfc, 0xc1,

                  // quick-lint-js metadata UUID
                  0x71, 0x75, 0x69, 0x63, 0x6b, 0x2d, 0x5f, 0x49,  //
                  0x3e, 0xb9, 0x6c, 0x69, 0x6e, 0x74, 0x6a, 0x73,

                  // Thread ID
                  0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

                  // Compression mode
                  0x00,
                  // clang-format on
              }));
}

// For most Trace_Writer tests, see test-trace-reader-writer.cpp.
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
