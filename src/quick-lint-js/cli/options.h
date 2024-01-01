// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <optional>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/diag/diag-code-list.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/util/cast.h>

namespace quick_lint_js {
class Output_Stream;

enum class Output_Format {
  default_format,
  gnu_like,
  vim_qflist_json,
  emacs_lisp,
};

enum class Raw_Input_File_Language : unsigned char {
  // Explicit (--language=default) or implicit (no --language).
  default_,

  javascript,
  javascript_jsx,
  typescript,
  typescript_definition,
  typescript_jsx,
};

enum class Resolved_Input_File_Language : unsigned char {
  javascript = enum_to_int_cast(Raw_Input_File_Language::javascript),
  javascript_jsx = enum_to_int_cast(Raw_Input_File_Language::javascript_jsx),
  typescript = enum_to_int_cast(Raw_Input_File_Language::typescript),
  typescript_definition =
      enum_to_int_cast(Raw_Input_File_Language::typescript_definition),
  typescript_jsx = enum_to_int_cast(Raw_Input_File_Language::typescript_jsx),
};

enum class Option_When { auto_, always, never };

struct File_To_Lint {
  const char *path;
  const char *config_file = nullptr;
  const char *path_for_config_search = nullptr;

  Raw_Input_File_Language language = Raw_Input_File_Language::default_;

  bool is_stdin = false;
  std::optional<int> vim_bufnr;
};

struct Options {
  bool help = false;
  bool list_debug_apps = false;
  bool version = false;
  bool print_parser_visits = false;
  bool lsp_server = false;
  bool snarky = false;
  Output_Format output_format = Output_Format::default_format;
  Option_When diagnostic_hyperlinks = Option_When::auto_;
  Span<const File_To_Lint> files_to_lint;
  Compiled_Diag_Code_List exit_fail_on;
  const char *path_for_stdin = nullptr;

  Span<const char *const> error_unrecognized_options;
  Span<const char *const> warning_vim_bufnr_without_file;
  Span<const char *const> warning_language_without_file;
  bool has_multiple_stdin = false;
  bool has_config_file = false;
  bool has_language = false;
  bool has_vim_file_bufnr = false;

  bool has_stdin() const;

  bool dump_errors(Output_Stream &) const;
};

Resolved_Input_File_Language get_language(const File_To_Lint &file,
                                          const Options &);
Resolved_Input_File_Language get_language(const char *file,
                                          Raw_Input_File_Language language);

// Returns portions of argv and memory allocated by allocator.
Options parse_options(int argc, char **argv, Monotonic_Allocator *allocator);
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
