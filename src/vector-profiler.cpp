// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <limits>
#include <map>
#include <ostream>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/vector-profiler.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/warning.h>
#include <string>

#if QLJS_FEATURE_VECTOR_PROFILING
#include <iostream>
#endif

QLJS_WARNING_IGNORE_MSVC(4996)  // Function or variable may be unsafe.

namespace quick_lint_js {
std::ostream &operator<<(std::ostream &out,
                         const vector_instrumentation::entry &e) {
  out << "entry{.owner = \"" << e.owner << "\", .event = ";
  switch (e.event) {
  case vector_instrumentation::event::append:
    out << "append";
    break;
  case vector_instrumentation::event::assign:
    out << "assign";
    break;
  case vector_instrumentation::event::clear:
    out << "clear";
    break;
  case vector_instrumentation::event::create:
    out << "create";
    break;
  case vector_instrumentation::event::destroy:
    out << "destroy";
    break;
  }
  out << ", .size = " << e.size << ", .capacity = " << e.capacity << "}";
  return out;
}

#if QLJS_FEATURE_VECTOR_PROFILING
vector_instrumentation vector_instrumentation::instance;
#endif

void vector_instrumentation::clear() { this->entries_.clear(); }

std::vector<vector_instrumentation::entry> vector_instrumentation::entries()
    const {
  return this->entries_;
}

std::map<std::string, std::map<std::size_t, int>>
vector_instrumentation::max_size_histogram_by_owner() const {
  std::map<std::string, std::map<std::size_t, int>> histogram;
  std::map<std::pair<std::string, std::uintptr_t>, std::size_t> object_sizes;
  for (const vector_instrumentation::entry &entry : this->entries_) {
    std::pair key(entry.owner, entry.object_id);
    std::size_t &object_size = object_sizes[key];
    object_size = std::max(entry.size, object_size);

    if (entry.event == event::destroy) {
      histogram[entry.owner][object_size] += 1;
      object_sizes.erase(key);
    }
  }
  for (auto &[owner_and_object_id, size] : object_sizes) {
    histogram[owner_and_object_id.first][size] += 1;
  }
  return histogram;
}

void vector_instrumentation::dump_max_size_histogram(
    const std::map<std::string, std::map<std::size_t, int>> &histogram,
    std::ostream &out) {
  return dump_max_size_histogram(histogram, out, dump_options());
}

void vector_instrumentation::dump_max_size_histogram(
    const std::map<std::string, std::map<std::size_t, int>> &histogram,
    std::ostream &out, const dump_options &options) {
  bool need_blank_line = false;
  for (const auto &[group_name, object_size_histogram] : histogram) {
    QLJS_ASSERT(!object_size_histogram.empty());

    if (need_blank_line) {
      out << '\n';
    }
    need_blank_line = true;

    out << "Max sizes for " << group_name << ":\n";

    int max_count = 0;
    int total_count = 0;
    for (const auto &[_object_size, count] : object_size_histogram) {
      total_count += count;
      max_count = std::max(max_count, count);
    }
    std::size_t max_object_size = object_size_histogram.rbegin()->first;

    int max_digits_in_legend = static_cast<int>(
        std::ceil(std::log10(static_cast<double>(max_object_size + 1))));
    int legend_length = max_digits_in_legend + 9;
    int maximum_bar_length = options.maximum_line_length - legend_length;
    double bar_scale_factor = max_count > maximum_bar_length
                                  ? static_cast<double>(maximum_bar_length) /
                                        static_cast<double>(max_count)
                                  : 1.0;

    std::size_t next_object_size = 0;
    for (auto &[object_size, count] : object_size_histogram) {
      QLJS_ASSERT(count != 0);

      QLJS_ASSERT(options.max_adjacent_empty_rows > 0);
      if (object_size - next_object_size >
          narrow_cast<std::size_t>(options.max_adjacent_empty_rows)) {
        out << "...\n";
      } else {
        for (std::size_t i = next_object_size; i < object_size; ++i) {
          out << std::setw(max_digits_in_legend) << i << "  ( 0%)\n";
        }
      }

      out << std::setw(max_digits_in_legend) << object_size << "  (";
      if (count == total_count) {
        out << "ALL";
      } else {
        double count_fraction =
            static_cast<double>(count) / static_cast<double>(total_count);
        out << std::setw(2) << std::round(count_fraction * 100) << "%";
      }
      out << ')';

      int bar_width =
          std::max(1, static_cast<int>(std::floor(static_cast<double>(count) *
                                                  bar_scale_factor)));
      out << "  ";
      for (int i = 0; i < bar_width; ++i) {
        out << '*';
      }
      out << '\n';

      next_object_size = object_size + 1;
    }
  }
}

std::map<std::string, vector_instrumentation::capacity_change_histogram>
vector_instrumentation::capacity_change_histogram_by_owner() const {
  std::map<std::string, capacity_change_histogram> histogram;
  struct vector_info {
    std::uintptr_t data_pointer;
    std::size_t size;
  };
  std::map<std::uintptr_t, vector_info> objects;
  for (const vector_instrumentation::entry &entry : this->entries_) {
    capacity_change_histogram &h = histogram[entry.owner];
    if (entry.event == event::append) {
      auto old_object_it = objects.find(entry.object_id);
      QLJS_ASSERT(old_object_it != objects.end());
      vector_info &old_object = old_object_it->second;
      if (old_object.data_pointer == entry.data_pointer) {
        h.appends_reusing_capacity += 1;
      } else if (old_object.size == 0) {
        h.appends_initial_capacity += 1;
      } else {
        h.appends_growing_capacity += 1;
      }
    }
    objects[entry.object_id] = vector_info{
        .data_pointer = entry.data_pointer,
        .size = entry.size,
    };
  }
  return histogram;
}

void vector_instrumentation::dump_capacity_change_histogram(
    const std::map<std::string, capacity_change_histogram> &histogram,
    std::ostream &out, const dump_capacity_change_options &options) {
  out << R"(vector capacity changes:
(C=copied; z=initial alloc; -=used internal capacity)
)";

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

    out << owner << ":\n";

    // Example:
    //  5C  0z 15_ |CCCCC_______________|
    out << std::setw(count_width) << h.appends_growing_capacity << "C "
        << std::setw(count_width) << h.appends_initial_capacity << "z "
        << std::setw(count_width) << h.appends_reusing_capacity << "_ |";
    int graph_columns =
        options.maximum_line_length -
        (count_width * 3 + narrow_cast<int>(std::strlen("C z _ ||")));
    int columns_growing_capacity =
        narrow_cast<int>(narrow_cast<std::uintmax_t>(graph_columns) *
                         h.appends_growing_capacity / append_count);  // 'C'
    for (int i = 0; i < columns_growing_capacity; ++i) {
      out << 'C';
    }
    int columns_initial_capacity =
        narrow_cast<int>(narrow_cast<std::uintmax_t>(graph_columns) *
                         h.appends_initial_capacity / append_count);  // 'z'
    for (int i = 0; i < columns_initial_capacity; ++i) {
      out << 'z';
    }
    int columns_reusing_capacity =
        graph_columns -
        (columns_growing_capacity + columns_initial_capacity);  // '_'
    for (int i = 0; i < columns_reusing_capacity; ++i) {
      out << '_';
    }
    out << "|\n";
  }
}

void vector_instrumentation::add_entry(std::uintptr_t object_id,
                                       const char *owner,
                                       vector_instrumentation::event event,
                                       std::uintptr_t data_pointer,
                                       std::size_t size, std::size_t capacity) {
  this->entries_.emplace_back(entry{
      .object_id = object_id,
      .owner = owner,
      .event = event,
      .data_pointer = data_pointer,
      .size = size,
      .capacity = capacity,
  });
}

void vector_instrumentation::register_dump_on_exit_if_requested() {
#if QLJS_FEATURE_VECTOR_PROFILING
  const char *dump_vectors_value = std::getenv("QLJS_DUMP_VECTORS");
  bool should_dump_on_exit = dump_vectors_value && *dump_vectors_value != '\0';
  if (should_dump_on_exit) {
    std::atexit([]() -> void {
      instance.dump_max_size_histogram(instance.max_size_histogram_by_owner(),
                                       std::cerr,
                                       dump_options{
                                           .maximum_line_length = 80,
                                           .max_adjacent_empty_rows = 5,
                                       });
      std::cerr << '\n';
      instance.dump_capacity_change_histogram(
          instance.capacity_change_histogram_by_owner(), std::cerr,
          dump_capacity_change_options{
              .maximum_line_length = 80,
          });
    });
  }
#endif
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
