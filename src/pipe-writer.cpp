// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/pipe-writer.h>

#if QLJS_HAVE_WRITEV
#include <sys/uio.h>
#include <unistd.h>
#endif

namespace quick_lint_js {
pipe_writer::pipe_writer(platform_file_ref pipe) : pipe_(pipe) {}

void pipe_writer::write(byte_buffer_iovec&& data) {
  while (data.iovec_count() != 0) {
#if QLJS_HAVE_WRITEV
    ::ssize_t raw_bytes_written =
        ::writev(this->pipe_.get(), data.iovec(), data.iovec_count());
    if (raw_bytes_written < 0) {
      QLJS_UNIMPLEMENTED();
    }
    std::size_t bytes_written = narrow_cast<std::size_t>(raw_bytes_written);
#else
    const byte_buffer_chunk& chunk = data.iovec()[0];
    QLJS_ASSERT(chunk.size != 0);  // Writing can hang if given size 0.
    std::optional<int> raw_bytes_written =
        this->pipe_.write(chunk.data, narrow_cast<int>(chunk.size));
    if (!raw_bytes_written.has_value()) {
      QLJS_UNIMPLEMENTED();
    }
    std::size_t bytes_written = narrow_cast<std::size_t>(*raw_bytes_written);
#endif
    QLJS_ASSERT(bytes_written != 0);
    data.remove_front(bytes_written);
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
