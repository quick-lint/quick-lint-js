// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <map>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/vector-profiler.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/logging/trace-types.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/cast.h>
#include <string>

QLJS_WARNING_IGNORE_MSVC(4996)  // Function or variable may be unsafe.

namespace quick_lint_js {
#if QLJS_FEATURE_VECTOR_PROFILING
Vector_Instrumentation Vector_Instrumentation::instance;
#endif

void Vector_Instrumentation::clear() { this->entries_.lock()->clear(); }

std::vector<Vector_Instrumentation::Entry> Vector_Instrumentation::entries() {
  return *this->entries_.lock();
}

std::vector<Vector_Instrumentation::Entry>
Vector_Instrumentation::take_entries() {
  std::vector<Vector_Instrumentation::Entry> result;
  swap(result, *this->entries_.lock());
  return result;
}

void Vector_Instrumentation::add_entry(std::uintptr_t object_id,
                                       const char *owner,
                                       Vector_Instrumentation::Event event,
                                       std::uintptr_t data_pointer,
                                       std::size_t size, std::size_t capacity) {
  this->entries_.lock()->emplace_back(Entry{
      .object_id = object_id,
      .owner = owner,
      .event = event,
      .data_pointer = data_pointer,
      .size = size,
      .capacity = capacity,
  });
}

#if QLJS_FEATURE_VECTOR_PROFILING
void Vector_Instrumentation::register_dump_on_exit_if_requested() {
  const char *dump_vectors_value = std::getenv("QLJS_DUMP_VECTORS");
  bool should_dump_on_exit = dump_vectors_value && *dump_vectors_value != '\0';
  if (should_dump_on_exit) {
    // HACK(strager): Force the stderr File_Output_Stream to be destructed
    // *after* our std::atexit callback is called.
    //
    // Without this dummy call, the File_Output_Stream is destructed before our
    // callback, causing uses of the File_Output_Stream to likely crash due to
    // the vtable being wiped during destruction.
    (void)File_Output_Stream::get_stderr();

    std::atexit([]() -> void {
      auto entries = instance.entries();
      Output_Stream &out = *File_Output_Stream::get_stderr();

      {
        Vector_Max_Size_Histogram_By_Owner hist;
        hist.add_entries(entries);
        Monotonic_Allocator memory("Vector_Instrumentation dump_on_exit");
        Vector_Max_Size_Histogram_By_Owner::dump(
            hist.histogram(&memory), out,
            Vector_Max_Size_Histogram_By_Owner::Dump_Options{
                .maximum_line_length = 80,
                .max_adjacent_empty_rows = 5,
            });
      }
      out.append_copy(u8'\n');

      {
        Vector_Capacity_Change_Histogram_By_Owner hist;
        hist.add_entries(entries);
        Vector_Capacity_Change_Histogram_By_Owner::dump(
            hist.histogram(), out,
            Vector_Capacity_Change_Histogram_By_Owner::Dump_Options{
                .maximum_line_length = 80,
            });
      }

      out.flush();
    });
  }
}
#endif

Vector_Max_Size_Histogram_By_Owner::Vector_Max_Size_Histogram_By_Owner() =
    default;

Vector_Max_Size_Histogram_By_Owner::~Vector_Max_Size_Histogram_By_Owner() =
    default;

void Vector_Max_Size_Histogram_By_Owner::add_entries(
    const std::vector<Vector_Instrumentation::Entry> &entries) {
  for (const Vector_Instrumentation::Entry &entry : entries) {
    std::pair key(entry.owner, entry.object_id);
    std::size_t &object_size = this->object_sizes_[key];
    object_size = std::max(entry.size, object_size);

    if (entry.event == Vector_Instrumentation::Event::destroy) {
      this->histogram_[entry.owner][object_size] += 1;
      this->object_sizes_.erase(key);
    }
  }
}

Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>
Vector_Max_Size_Histogram_By_Owner::histogram(
    Monotonic_Allocator *memory) const {
  // TODO(strager): Avoid this copy.
  Hash_Map<const char *, Hash_Map<std::size_t, int>> histogram =
      this->histogram_;

  for (auto &[owner_and_object_id, size] : this->object_sizes_) {
    histogram[owner_and_object_id.first][size] += 1;
  }

  // Convert Hash_Map into std::map.
  // TODO(strager): Avoid this conversion. Convert straight into vectors.
  // NOTE(strager): We might need to merge some inner maps because we built the
  // Hash_Map with pointer comparison but we're building the std::map with
  // string comparison.
  std::map<std::string_view, std::map<std::size_t, int>> stable_histogram;
  for (auto &[owner, counts] : histogram) {
    auto &stable_counts = stable_histogram[owner];
    for (auto &[size, count] : counts) {
      stable_counts[size] += count;
    }
  }

  // NOTE(strager): We use Raw_Bump_Vector to prevent vector profiling code from
  // emitting events itself.
  Raw_Bump_Vector<Trace_Vector_Max_Size_Histogram_By_Owner_Entry>
      out_entries_by_owner(memory);
  out_entries_by_owner.reserve(
      narrow_cast<Bump_Vector_Size>(stable_histogram.size()));
  for (auto &[owner, counts] : stable_histogram) {
    Raw_Bump_Vector<Trace_Vector_Max_Size_Histogram_Entry> out_entries(memory);
    out_entries.reserve(narrow_cast<Bump_Vector_Size>(counts.size()));
    for (auto &[size, count] : counts) {
      out_entries.push_back(Trace_Vector_Max_Size_Histogram_Entry{
          .max_size = narrow_cast<std::uint64_t>(size),
          .count = narrow_cast<std::uint64_t>(count),
      });
    }
    out_entries_by_owner.push_back(
        Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
            .owner = to_string8_view(owner),
            .max_size_entries = out_entries.release_to_span(),
        });
  }
  return out_entries_by_owner.release_to_span();
}

void Vector_Max_Size_Histogram_By_Owner::dump(
    Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry> histogram,
    Output_Stream &out) {
  return dump(histogram, out, Dump_Options());
}

void Vector_Max_Size_Histogram_By_Owner::dump(
    Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry> histogram,
    Output_Stream &out, const Dump_Options &options) {
  bool need_blank_line = false;
  for (const auto &[group_name, object_size_histogram] : histogram) {
    QLJS_ASSERT(!object_size_histogram.empty());

    if (need_blank_line) {
      out.append_copy(u8'\n');
    }
    need_blank_line = true;

    out.append_literal(u8"Max sizes for "_sv);
    out.append_copy(group_name);
    out.append_literal(u8":\n"_sv);

    std::uint64_t max_count = 0;
    std::uint64_t total_count = 0;
    for (const auto &[_object_size, count] : object_size_histogram) {
      total_count += count;
      max_count = std::max(max_count, count);
    }
    std::uint64_t max_object_size = object_size_histogram.back().max_size;

    int max_digits_in_legend = static_cast<int>(
        std::ceil(std::log10(static_cast<double>(max_object_size + 1))));
    int legend_length = max_digits_in_legend + 9;
    int maximum_bar_length = options.maximum_line_length - legend_length;
    double bar_scale_factor =
        max_count > narrow_cast<std::uint64_t>(maximum_bar_length)
            ? static_cast<double>(maximum_bar_length) /
                  static_cast<double>(max_count)
            : 1.0;

    std::size_t next_object_size = 0;
    for (auto &[object_size, count] : object_size_histogram) {
      QLJS_ASSERT(count != 0);

      QLJS_ASSERT(options.max_adjacent_empty_rows > 0);
      if (object_size - next_object_size >
          narrow_cast<std::size_t>(options.max_adjacent_empty_rows)) {
        out.append_literal(u8"...\n"_sv);
      } else {
        for (std::size_t i = next_object_size; i < object_size; ++i) {
          out.append_padded_decimal_integer(i, max_digits_in_legend, u8' ');
          out.append_literal(u8"  ( 0%)\n"_sv);
        }
      }

      out.append_padded_decimal_integer(object_size, max_digits_in_legend,
                                        u8' ');
      out.append_literal(u8"  ("_sv);
      if (count == total_count) {
        out.append_literal(u8"ALL"_sv);
      } else {
        double count_fraction =
            static_cast<double>(count) / static_cast<double>(total_count);
        out.append_padded_decimal_integer(
            static_cast<unsigned>(std::round(count_fraction * 100)), 2, u8' ');
        out.append_literal(u8"%"_sv);
      }
      out.append_copy(u8')');

      int bar_width =
          std::max(1, static_cast<int>(std::floor(static_cast<double>(count) *
                                                  bar_scale_factor)));
      out.append_literal(u8"  "_sv);
      for (int i = 0; i < bar_width; ++i) {
        out.append_copy(u8'*');
      }
      out.append_copy(u8'\n');

      next_object_size = object_size + 1;
    }
  }

  out.flush();
}

Vector_Capacity_Change_Histogram_By_Owner::
    Vector_Capacity_Change_Histogram_By_Owner() = default;

Vector_Capacity_Change_Histogram_By_Owner::
    ~Vector_Capacity_Change_Histogram_By_Owner() = default;

void Vector_Capacity_Change_Histogram_By_Owner::add_entries(
    const std::vector<Vector_Instrumentation::Entry> &entries) {
  for (const Vector_Instrumentation::Entry &entry : entries) {
    Capacity_Change_Histogram &h = this->histogram_[entry.owner];
    if (entry.event == Vector_Instrumentation::Event::append) {
      auto old_object_it = this->objects_.find(entry.object_id);
      QLJS_ASSERT(old_object_it != this->objects_.end());
      Vector_Info &old_object = old_object_it->second;
      if (old_object.data_pointer == entry.data_pointer) {
        h.appends_reusing_capacity += 1;
      } else if (old_object.size == 0) {
        h.appends_initial_capacity += 1;
      } else {
        h.appends_growing_capacity += 1;
      }
    }
    this->objects_[entry.object_id] = Vector_Info{
        .data_pointer = entry.data_pointer,
        .size = entry.size,
    };
  }
}

std::map<std::string_view,
         Vector_Capacity_Change_Histogram_By_Owner::Capacity_Change_Histogram>
Vector_Capacity_Change_Histogram_By_Owner::histogram() const {
  return this->histogram_;
}

void Vector_Capacity_Change_Histogram_By_Owner::dump(
    const std::map<std::string_view, Capacity_Change_Histogram> &histogram,
    Output_Stream &out, const Dump_Options &options) {
  out.append_literal(
      u8R"(vector capacity changes:
(C=copied; z=initial alloc; -=used internal capacity)
)"_sv);

  int count_width = 1;
  for (auto &[owner, h] : histogram) {
    count_width = std::max(
        count_width,
        narrow_cast<int>(std::max(
            std::max(std::to_string(h.appends_growing_capacity).size(),
                     std::to_string(h.appends_initial_capacity).size()),
            std::to_string(h.appends_reusing_capacity).size())));
  }

  for (auto &[owner, h] : histogram) {
    std::size_t append_count = h.appends_growing_capacity +
                               h.appends_initial_capacity +
                               h.appends_reusing_capacity;
    if (append_count == 0) {
      continue;
    }

    out.append_copy(to_string8_view(owner));
    out.append_literal(u8":\n"_sv);

    // Example:
    //  5C  0z 15_ |CCCCC_______________|
    out.append_padded_decimal_integer(h.appends_growing_capacity, count_width,
                                      u8' ');
    out.append_literal(u8"C "_sv);
    out.append_padded_decimal_integer(h.appends_initial_capacity, count_width,
                                      u8' ');
    out.append_literal(u8"z "_sv);
    out.append_padded_decimal_integer(h.appends_reusing_capacity, count_width,
                                      u8' ');
    out.append_literal(u8"_ |"_sv);
    int graph_columns =
        options.maximum_line_length -
        (count_width * 3 + narrow_cast<int>(std::strlen("C z _ ||")));
    int columns_growing_capacity =
        narrow_cast<int>(narrow_cast<std::uintmax_t>(graph_columns) *
                         h.appends_growing_capacity / append_count);  // 'C'
    for (int i = 0; i < columns_growing_capacity; ++i) {
      out.append_copy(u8'C');
    }
    int columns_initial_capacity =
        narrow_cast<int>(narrow_cast<std::uintmax_t>(graph_columns) *
                         h.appends_initial_capacity / append_count);  // 'z'
    for (int i = 0; i < columns_initial_capacity; ++i) {
      out.append_copy(u8'z');
    }
    int columns_reusing_capacity =
        graph_columns -
        (columns_growing_capacity + columns_initial_capacity);  // '_'
    for (int i = 0; i < columns_reusing_capacity; ++i) {
      out.append_copy(u8'_');
    }
    out.append_copy(u8"|\n"_sv);
  }
  out.flush();
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
