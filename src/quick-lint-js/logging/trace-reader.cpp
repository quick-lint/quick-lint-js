// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include "quick-lint-js/port/endian.h"
#include <csetjmp>
#include <cstddef>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/logging/trace-reader.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/binary-reader.h>
#include <vector>

namespace quick_lint_js {
Trace_Reader::Trace_Reader() = default;

Trace_Reader::~Trace_Reader() = default;

void Trace_Reader::append_bytes(const void* data, std::size_t data_size) {
  const std::uint8_t* new_bytes = reinterpret_cast<const std::uint8_t*>(data);
  this->queue_.insert(this->queue_.end(), new_bytes, new_bytes + data_size);
}

std::vector<Parsed_Trace_Event> Trace_Reader::pull_new_events() {
  std::vector<Parsed_Trace_Event> result;
  this->pull_new_events(result);
  return result;
}

void Trace_Reader::pull_new_events(std::vector<Parsed_Trace_Event>& out) {
  // Delete already-parsed data from this->queue_. We can't do this after
  // parsing because the parsed event objects point to memory inside
  // this->queue_.
  {
    std::vector<std::uint8_t>::iterator begin = this->queue_.begin();
    std::vector<std::uint8_t>::iterator end =
        begin + narrow_cast<std::ptrdiff_t>(this->parsed_bytes_);
#if defined(QLJS_DEBUG) && QLJS_DEBUG
    std::fill(begin, end, 'x');
#endif
    this->queue_.erase(begin, end);
  }

  std::jmp_buf buf;
  auto unexpected_end_of_file = [&buf]() {
    std::longjmp(buf, 1);
    QLJS_UNREACHABLE();
  };
  Checked_Binary_Reader r(this->queue_.data(), this->queue_.size(),
                          unexpected_end_of_file);

  const std::uint8_t* volatile committed = r.cursor();
  if (setjmp(buf) == 0) {
    while (!r.eof() && !this->encountered_error_) {
      this->parse_one(r);
      committed = r.cursor();
    }
  } else {
    // End of file prematurely reached.
  }
  this->parsed_bytes_ =
      narrow_cast<std::size_t>(committed - this->queue_.data());

  out.insert(out.end(), this->parsed_events_.begin(),
             this->parsed_events_.end());
  this->parsed_events_.clear();
}

void Trace_Reader::parse_one(Checked_Binary_Reader& r) {
  if (this->parsed_header_) {
    this->parse_event(r);
  } else {
    this->parse_header(r);
  }
}

void Trace_Reader::parse_header(Checked_Binary_Reader& r) {
  QLJS_ASSERT(!this->parsed_header_);

  std::uint32_t magic = r.u32_le();
  if (magic != 0xc1fc1fc1) {
    this->on_error(Parsed_Trace_Event_Type::error_invalid_magic);
    return;
  }

  static constexpr std::uint8_t quick_lint_js_uuid[] = {
      0x71, 0x75, 0x69, 0x63, 0x6b, 0x2d, 0x5f, 0x49,
      0x3e, 0xb9, 0x6c, 0x69, 0x6e, 0x74, 0x6a, 0x73,
  };
  const std::uint8_t* uuid = r.advance(std::size(quick_lint_js_uuid));
  if (!std::equal(std::begin(quick_lint_js_uuid), std::end(quick_lint_js_uuid),
                  uuid)) {
    this->on_error(Parsed_Trace_Event_Type::error_invalid_uuid);
    return;
  }

  std::uint64_t thread_id = r.u64_le();

  std::uint8_t compression_mode = r.u8();
  if (compression_mode != 0) {
    this->on_error(Parsed_Trace_Event_Type::error_unsupported_compression_mode);
    return;
  }

  this->parsed_events_.push_back(Parsed_Trace_Event{
      .type = Parsed_Trace_Event_Type::packet_header,
      // TODO(strager): Don't initialize .header. (Move it inside the union?)
      .header = Trace_Event_Header{.timestamp = 0},
      .packet_header =
          Trace_Context{
              .thread_id = thread_id,
          },
  });
  this->parsed_header_ = true;
}

std::u16string_view Trace_Reader::parse_utf16le_string(
    Checked_Binary_Reader& r) {
  std::uint64_t length = r.u64_le();
  std::uint8_t* bytes = const_cast<std::uint8_t*>(r.advance(length * 2));
  static_assert(sizeof(char16_t) == 2 * sizeof(std::uint8_t));
  Span<char16_t> string(reinterpret_cast<char16_t*>(bytes),
                        narrow_cast<Span_Size>(length));
  read_little_endian_in_place(string);
  return std::u16string_view(string.data(),
                             narrow_cast<std::size_t>(string.size()));
}

String8_View Trace_Reader::parse_utf8_string(Checked_Binary_Reader& r) {
  std::uint64_t length = r.u64_le();
  const std::uint8_t* bytes = r.advance(length);
  static_assert(sizeof(Char8) == sizeof(std::uint8_t));
  return String8_View(reinterpret_cast<const Char8*>(bytes), length);
}

String8_View Trace_Reader::parse_utf8_zstring(Checked_Binary_Reader& r) {
  const std::uint8_t* bytes = r.cursor();
  r.find_and_skip_byte(0x00);
  const std::uint8_t* end = r.cursor() - 1;
  return String8_View(reinterpret_cast<const Char8*>(bytes),
                      narrow_cast<std::size_t>(end - bytes));
}

void Trace_Reader::on_error(Parsed_Trace_Event_Type error) {
  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")
  this->parsed_events_.push_back(Parsed_Trace_Event{
      .type = error,
      // Don't initialize any union member.
  });
  QLJS_WARNING_POP
  this->encountered_error_ = true;
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
