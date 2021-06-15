// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_EVENT_LOOP_H
#define QUICK_LINT_JS_EVENT_LOOP_H

#include <array>
#include <cstddef>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/narrow-cast.h>

namespace quick_lint_js {
#if QLJS_HAVE_CXX_CONCEPTS
template <class Delegate>
concept event_loop_delegate = requires(Delegate d, string8_view data) {
  {d.get_readable_pipe()};
  {d.append(data)};
};
#endif

// An event_loop implements I/O concurrency on a single thread.
//
// An event_loop manages the following types of I/O:
//
// * a readable pipe
//
// event_loop uses the CRTP pattern. Inherit from event_loop<your_class>.
// your_class must satisfy the event_loop_delegate concept.
template <class Derived>
class event_loop {
 public:
  void run() {
    for (;;) {
      // TODO(strager): Pick buffer size intelligently.
      std::array<char8, 1024> buffer;
      file_read_result read_result = this->derived().get_readable_pipe().read(
          buffer.data(), buffer.size());
      if (read_result.at_end_of_file) {
        break;
      } else if (read_result.error_message.has_value()) {
        QLJS_UNIMPLEMENTED();
      } else {
        this->derived().append(string8_view(
            buffer.data(), narrow_cast<std::size_t>(read_result.bytes_read)));
      }
    }
  }

 private:
#if QLJS_HAVE_CXX_CONCEPTS
  event_loop_delegate
#endif
      auto&
      derived() {
    return *static_cast<Derived*>(this);
  }
};
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
