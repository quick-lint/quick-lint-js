// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No event loops on the web.
#else

#include <atomic>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/io/event-loop.h>

#if defined(_WIN32)
#include <quick-lint-js/port/windows.h>
#endif

namespace quick_lint_js {
Event_Loop_Base::Event_Loop_Base() = default;

void Event_Loop_Base::keep_alive() { this->alive_count_ += 1; }

void Event_Loop_Base::un_keep_alive() {
  int old_alive_count = this->alive_count_.fetch_sub(1);
  QLJS_ASSERT(
      old_alive_count > 0 &&
      "un_keep_alive must not be called on a stopping or stopped event loop");
  bool should_stop = old_alive_count <= 1;
  if (should_stop) {
    this->request_stop();
  }
}

void Event_Loop_Base::stop_event_loop_testing_only() {
  this->alive_count_ = 0;
  this->request_stop();
}

bool Event_Loop_Base::is_stop_requested() const {
  return this->alive_count_ <= 0;
}

Event_Loop_Base::Read_From_Pipe_Result
Event_Loop_Base::handle_read_from_pipe_result(
    const File_Read_Result& read_result, Span<const Char8> buffer,
    Platform_File_Ref pipe, Event_Loop_Pipe_Read_Delegate* delegate) {
  if (!read_result.ok()) {
#if QLJS_HAVE_UNISTD_H
    if (read_result.error().is_would_block_try_again_error()) {
#if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING
      return Read_From_Pipe_Result::no_data;
#else
      QLJS_UNREACHABLE();
#endif
    }
#elif defined(_WIN32)
    if (read_result.error().error == ERROR_OPERATION_ABORTED) {
      return Read_From_Pipe_Result::aborted;
    }
#endif
    delegate->on_pipe_read_error(this, pipe, read_result.error());
    return Read_From_Pipe_Result::error;
  }

  if (read_result.at_end_of_file()) {
    delegate->on_pipe_read_end(this, pipe);
    return Read_From_Pipe_Result::end;
  } else {
    QLJS_ASSERT(read_result.bytes_read() != 0);
    delegate->on_pipe_read_data(
        this, pipe,
        String8_View(buffer.data(),
                     narrow_cast<std::size_t>(read_result.bytes_read())));
    return Read_From_Pipe_Result::data;
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
