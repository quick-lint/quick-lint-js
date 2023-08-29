// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/logging/trace-reader-generated.h>
#include <quick-lint-js/logging/trace-types.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/binary-reader.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string_view>
#include <vector>

namespace quick_lint_js {
// Designed for convenience, not efficiency.
class Trace_Reader {
 public:
  explicit Trace_Reader();
  ~Trace_Reader();

  void append_bytes(const void* data, std::size_t data_size);

  // The memory returned by pull_new_events is only valid until the next call to
  // pull_new_events, append_bytes, or ~Trace_Reader. In other words,
  // pull_new_events and append_bytes deallocate memory allocated by the
  // previous call to pull_new_events.
  std::vector<Parsed_Trace_Event> pull_new_events();
  void pull_new_events(std::vector<Parsed_Trace_Event>& out);

  // Private to Trace_Reader's generated code.
  std::u16string_view parse_utf16le_string(Checked_Binary_Reader&);
  String8_View parse_utf8_string(Checked_Binary_Reader&);
  String8_View parse_utf8_zstring(Checked_Binary_Reader&);

  // Private to Trace_Reader's generated code.
  template <class Item, class Func>
  Span<Item> parse_array(Checked_Binary_Reader& r, Func&& parse_item) {
    std::uint64_t count = r.u64_le();
    Span<Item> items =
        this->memory_.allocate_span<Item>(narrow_cast<std::size_t>(count));
    for (Span_Size i = 0; i < items.size(); ++i) {
      items[i] = parse_item();
    }
    return items;
  }

  // Private to Trace_Reader's generated code.
  void on_error(Parsed_Trace_Event_Type);

 private:
  void parse_one(Checked_Binary_Reader&);
  void parse_header(Checked_Binary_Reader&);
  void parse_event(Checked_Binary_Reader&);

  std::vector<std::uint8_t> queue_;
  std::vector<Parsed_Trace_Event> parsed_events_;
  bool parsed_header_ = false;
  bool encountered_error_ = false;
  // The number of bytes at the beginning of this->queue_ which pull_new_events
  // has already parsed. These bytes can be erased.
  std::size_t parsed_bytes_ = 0;

  Monotonic_Allocator memory_{"Trace_Reader::memory"};
};
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
