// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/typescript-test.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/ascii.h>
#include <utility>
#include <vector>

namespace quick_lint_js {
namespace {
struct typescript_test_metadata_directive {
  std::size_t start_index;
  std::size_t end_index;
  string8_view metadata_name;
  string8_view metadata_value;

  bool is_filename_metadata() {
    return ranges_equal(metadata_name, u8"filename"_sv, [](char8 x, char8 y) {
      QLJS_ASSERT(y == tolower(y));
      return tolower(x) == y;
    });
  }
};

// Parse a metadata directive from a TypeScript test's source code.
//
// Example directives:
//
// // @filename: a.ts
// //@target : ES6
//
// Reference:
// https://github.com/microsoft/TypeScript/blob/e83d61398ea0e4231e882121dd6c6bcfe4fdc9e4/src/harness/harnessIO.ts#L1171
// /^[\/]{2}\s*@(\w+)\s*:\s*([^\r\n]*)/gm
std::optional<typescript_test_metadata_directive>
find_typescript_test_metadata_directive(string8_view sv,
                                        std::size_t search_start_index) {
  std::size_t comment_index = sv.find(u8"//"_sv, search_start_index);
  if (comment_index == string8_view::npos) {
    return std::nullopt;
  }
  bool comment_at_beginning_of_line = comment_index == 0 ||
                                      sv[comment_index - 1] == u8'\n' ||
                                      sv[comment_index - 1] == u8'\r';
  if (!comment_at_beginning_of_line) {
    return std::nullopt;
  }

  std::size_t at_index = sv.find_first_not_of(u8" \t"_sv, comment_index + 2);
  if (at_index == string8_view::npos) {
    return std::nullopt;
  }
  if (sv[at_index] != u8'@') {
    return std::nullopt;
  }

  std::size_t metadata_name_begin_index = at_index + 1;
  std::size_t metadata_name_end_index = metadata_name_begin_index;
  while (metadata_name_end_index < sv.size() &&
         is_ascii_alpha(sv[metadata_name_end_index])) {
    metadata_name_end_index += 1;
  }
  string8_view metadata_name =
      sv.substr(metadata_name_begin_index,
                metadata_name_end_index - metadata_name_begin_index);

  std::size_t colon_index =
      sv.find_first_not_of(u8" \t"_sv, metadata_name_end_index);
  if (colon_index == string8_view::npos) {
    return std::nullopt;
  }
  if (sv[colon_index] != u8':') {
    return std::nullopt;
  }

  std::size_t metadata_value_begin_index = colon_index + 1;
  std::size_t directive_terminator_index =
      sv.find_first_of(u8"\n\r"_sv, metadata_value_begin_index);
  if (directive_terminator_index == string8_view::npos) {
    directive_terminator_index = sv.size();
  }
  string8_view metadata_value =
      trim(sv.substr(metadata_value_begin_index,
                     directive_terminator_index - metadata_value_begin_index),
           u8" \t"_sv);
  std::size_t end_index =
      sv.find_first_not_of(u8"\n\r"_sv, directive_terminator_index + 1);
  if (end_index == string8_view::npos) {
    end_index = sv.size();
  }

  return typescript_test_metadata_directive{
      .start_index = comment_index,
      .end_index = end_index,
      .metadata_name = metadata_name,
      .metadata_value = metadata_value,
  };
}

std::optional<typescript_test_metadata_directive>
find_typescript_test_filename_metadata_directive(
    string8_view sv, std::size_t search_start_index) {
  for (;;) {
    std::optional<typescript_test_metadata_directive> directive =
        find_typescript_test_metadata_directive(sv, search_start_index);
    if (!directive.has_value()) {
      return std::nullopt;
    }
    if (directive->is_filename_metadata()) {
      return directive;
    }
    search_start_index = directive->end_index;
  }
}
}

std::optional<linter_options> typescript_test_unit::get_linter_options() const
    noexcept {
  if (ends_with(string8_view(this->name), u8".json"_sv)) {
    return std::nullopt;
  }
  if (ends_with(string8_view(this->name), u8".ts"_sv)) {
    return linter_options{.jsx = false, .typescript = true};
  }
  if (ends_with(string8_view(this->name), u8".tsx"_sv)) {
    return linter_options{.jsx = true, .typescript = true};
  }
  // FIXME(strager): Should we reject unknown file extensions?
  return linter_options{.jsx = false, .typescript = true};
}

typescript_test_units extract_units_from_typescript_test(
    padded_string&& file, string8_view test_file_name) {
  typescript_test_units units;

  string8_view sv = file.string_view();
  string8_view next_file_name = test_file_name;
  for (;;) {
    std::optional<typescript_test_metadata_directive> filename_directive =
        find_typescript_test_filename_metadata_directive(sv, 0);
    if (!filename_directive.has_value()) {
      break;
    }
    if (filename_directive->start_index != 0) {
      units.push_back(typescript_test_unit{
          .data = padded_string(sv.substr(0, filename_directive->start_index)),
          .name = string8(next_file_name),
      });
      next_file_name = filename_directive->metadata_value;
    }
    sv = sv.substr(filename_directive->end_index);
  }

  bool found_filename_directive = sv.data() != file.data();
  if (found_filename_directive) {
    if (!sv.empty()) {
      units.push_back(typescript_test_unit{
          .data = padded_string(sv),
          .name = string8(next_file_name),
      });
    }
  } else {
    // Don't copy the input string. Just move it.
    units.push_back(typescript_test_unit{
        .data = std::move(file),
        .name = string8(next_file_name),
    });
  }
  return units;
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
