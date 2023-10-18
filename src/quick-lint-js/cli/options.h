// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <optional>
#include <quick-lint-js/diag/diag-code-list.h>
#include <vector>

namespace quick_lint_js {
class Output_Stream;

enum class Output_Format {
  default_format,
  gnu_like,
  vim_qflist_json,
  emacs_lisp,
};

enum class Raw_Input_File_Language : unsigned char {
  default_,  // Explicit (--language=default) or implicit (no --language).
  javascript,
  javascript_jsx,
  typescript,
  typescript_definition,
  typescript_jsx,
};

enum class Resolved_Input_File_Language : unsigned char {
  javascript = static_cast<unsigned char>(Raw_Input_File_Language::javascript),
  javascript_jsx =
      static_cast<unsigned char>(Raw_Input_File_Language::javascript_jsx),
  typescript = static_cast<unsigned char>(Raw_Input_File_Language::typescript),
  typescript_definition = static_cast<unsigned char>(
      Raw_Input_File_Language::typescript_definition),
  typescript_jsx =
      static_cast<unsigned char>(Raw_Input_File_Language::typescript_jsx),
};

enum class Option_When { auto_, always, never };

struct File_To_Lint {
  const char *path;
  const char *config_file = nullptr;
  const char *path_for_config_search = nullptr;

  Raw_Input_File_Language language = Raw_Input_File_Language::default_;

  bool is_stdin = false;
  std::optional<int> vim_bufnr;

  Resolved_Input_File_Language get_language() const;
};

Resolved_Input_File_Language get_language(const char *file,
                                          Raw_Input_File_Language language);

struct Options {
  bool help = false;
  bool list_debug_apps = false;
  bool version = false;
  bool print_parser_visits = false;
  bool lsp_server = false;
  bool snarky = false;
  Output_Format output_format = Output_Format::default_format;
  Option_When diagnostic_hyperlinks = Option_When::auto_;
  std::vector<File_To_Lint> files_to_lint;
  Compiled_Diag_Code_List exit_fail_on;

  std::vector<const char *> error_unrecognized_options;
  std::vector<const char *> warning_vim_bufnr_without_file;
  std::vector<const char *> warning_language_without_file;
  bool has_multiple_stdin = false;
  bool has_config_file = false;
  bool has_language = false;
  bool has_vim_file_bufnr = false;

  bool dump_errors(Output_Stream &) const;
};

Options parse_options(int argc, char **argv);
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
