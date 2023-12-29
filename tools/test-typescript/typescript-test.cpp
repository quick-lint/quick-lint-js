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
  String8_View metadata_name;
  String8_View metadata_value;

  bool is_filename_metadata() {
    return ranges_equal(metadata_name, u8"filename"_sv, [](Char8 x, Char8 y) {
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
find_typescript_test_metadata_directive(String8_View sv,
                                        std::size_t search_start_index) {
  std::size_t comment_index = sv.find(u8"//"_sv, search_start_index);
  if (comment_index == String8_View::npos) {
    return std::nullopt;
  }

  auto find_next_metadata =
      [&]() -> std::optional<typescript_test_metadata_directive> {
    return find_typescript_test_metadata_directive(sv, comment_index + 2);
  };

  bool comment_at_beginning_of_line = comment_index == 0 ||
                                      sv[comment_index - 1] == u8'\n' ||
                                      sv[comment_index - 1] == u8'\r';
  if (!comment_at_beginning_of_line) {
    return find_next_metadata();
  }

  std::size_t at_index = sv.find_first_not_of(u8" \t"_sv, comment_index + 2);
  if (at_index == String8_View::npos) {
    return find_next_metadata();
  }
  if (sv[at_index] != u8'@') {
    return find_next_metadata();
  }

  std::size_t metadata_name_begin_index = at_index + 1;
  std::size_t metadata_name_end_index = metadata_name_begin_index;
  while (metadata_name_end_index < sv.size() &&
         is_ascii_alpha(sv[metadata_name_end_index])) {
    metadata_name_end_index += 1;
  }
  String8_View metadata_name =
      sv.substr(metadata_name_begin_index,
                metadata_name_end_index - metadata_name_begin_index);

  std::size_t colon_index =
      sv.find_first_not_of(u8" \t"_sv, metadata_name_end_index);
  if (colon_index == String8_View::npos) {
    return find_next_metadata();
  }
  if (sv[colon_index] != u8':') {
    return find_next_metadata();
  }

  std::size_t metadata_value_begin_index = colon_index + 1;
  std::size_t directive_terminator_index =
      sv.find_first_of(u8"\n\r"_sv, metadata_value_begin_index);
  if (directive_terminator_index == String8_View::npos) {
    directive_terminator_index = sv.size();
  }
  String8_View metadata_value =
      trim(sv.substr(metadata_value_begin_index,
                     directive_terminator_index - metadata_value_begin_index),
           u8" \t"_sv);
  std::size_t end_index =
      sv.find_first_not_of(u8"\n\r"_sv, directive_terminator_index + 1);
  if (end_index == String8_View::npos) {
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
    String8_View sv, std::size_t search_start_index) {
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

std::optional<Linter_Options> TypeScript_Test_Unit::get_linter_options() const {
  if (starts_with(String8_View(this->name), u8"/node_modules/"_sv)) {
    return std::nullopt;
  }
  if (ends_with(String8_View(this->name), u8".json"_sv)) {
    return std::nullopt;
  }
  if (contains(String8_View(this->name), u8".d."_sv)) {
    return Linter_Options{
        .jsx = false, .typescript = true, .typescript_definition = true};
  }
  if (ends_with(String8_View(this->name), u8".ts"_sv)) {
    return Linter_Options{.jsx = false, .typescript = true};
  }
  if (ends_with(String8_View(this->name), u8".tsx"_sv)) {
    return Linter_Options{.jsx = true, .typescript = true};
  }
  if (ends_with(String8_View(this->name), u8".js"_sv)) {
    return Linter_Options{.jsx = true, .typescript = false};
  }
  if (ends_with(String8_View(this->name), u8".jsx"_sv)) {
    return Linter_Options{.jsx = true, .typescript = false};
  }
  // Don't lint unknown file extensions. See
  // TypeScript/tests/cases/conformance/moduleResolution/bundler/bundlerImportTsExtensions.ts
  // for an example.
  return std::nullopt;
}

TypeScript_Test_Units extract_units_from_typescript_test(
    Padded_String&& file, String8_View test_file_name) {
  TypeScript_Test_Units units;

  String8_View sv = file.string_view();
  String8_View next_file_name = test_file_name;
  for (;;) {
    std::optional<typescript_test_metadata_directive> filename_directive =
        find_typescript_test_filename_metadata_directive(sv, 0);
    if (!filename_directive.has_value()) {
      break;
    }
    if (filename_directive->start_index != 0) {
      units.push_back(TypeScript_Test_Unit{
          .data = Padded_String(sv.substr(0, filename_directive->start_index)),
          .name = String8(next_file_name),
      });
    }
    next_file_name = filename_directive->metadata_value;
    sv = sv.substr(filename_directive->end_index);
  }

  bool found_filename_directive = sv.data() != file.data();
  if (found_filename_directive) {
    if (!sv.empty()) {
      units.push_back(TypeScript_Test_Unit{
          .data = Padded_String(sv),
          .name = String8(next_file_name),
      });
    }
  } else {
    // Don't copy the input string. Just move it.
    units.push_back(TypeScript_Test_Unit{
        .data = std::move(file),
        .name = String8(next_file_name),
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
