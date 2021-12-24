// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <iomanip>
#include <limits>
#include <map>
#include <ostream>
#include <quick-lint-js/assert.h>
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

      for (std::size_t i = next_object_size; i < object_size; ++i) {
        out << std::setw(max_digits_in_legend) << i << "  ( 0%)\n";
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

void vector_instrumentation::add_entry(std::uintptr_t object_id,
                                       const char *owner,
                                       vector_instrumentation::event event,
                                       std::size_t size, std::size_t capacity) {
  this->entries_.emplace_back(entry{
      .object_id = object_id,
      .owner = owner,
      .event = event,
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
