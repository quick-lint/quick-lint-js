// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <ostream>
#include <quick-lint-js/arg-parser.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/options.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/warning.h>
#include <string_view>
#include <vector>

QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
QLJS_WARNING_IGNORE_GCC("-Wshadow=local")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
options parse_options(int argc, char** argv) {
  options o;

  std::optional<int> next_vim_file_bufnr;
  const char* next_path_for_config_search = nullptr;
  const char* active_config_file = nullptr;
  bool has_stdin = false;

  arg_parser parser(argc, argv);
  while (!parser.done()) {
    if (const char* argument = parser.match_argument()) {
      if (argument == "-"sv) {
        if (has_stdin) {
          o.has_multiple_stdin = true;
          continue;
        }
        file_to_lint file{.path = "<stdin>",
                          .config_file = active_config_file,
                          .path_for_config_search = next_path_for_config_search,
                          .is_stdin = true,
                          .vim_bufnr = next_vim_file_bufnr};
        has_stdin = true;
        o.files_to_lint.emplace_back(file);
      } else {
        file_to_lint file{.path = argument,
                          .config_file = active_config_file,
                          .path_for_config_search = next_path_for_config_search,
                          .is_stdin = false,
                          .vim_bufnr = next_vim_file_bufnr};
        o.files_to_lint.emplace_back(file);
      }

      next_path_for_config_search = nullptr;
      next_vim_file_bufnr = std::nullopt;
    } else if (parser.match_flag_option("--debug-parser-visits"sv,
                                        "--debug-p"sv)) {
      o.print_parser_visits = true;
    } else if (const char* arg_value =
                   parser.match_option_with_value("--output-format"sv)) {
      if (arg_value == "gnu-like"sv) {
        o.output_format = quick_lint_js::output_format::gnu_like;
      } else if (arg_value == "vim-qflist-json"sv) {
        o.output_format = quick_lint_js::output_format::vim_qflist_json;
      } else if (arg_value == "emacs-lisp"sv) {
        o.output_format = quick_lint_js::output_format::emacs_lisp;
      } else {
        o.error_unrecognized_options.emplace_back(arg_value);
      }
    } else if (const char* arg_value =
                   parser.match_option_with_value("--escape-errors"sv)) {
      if (arg_value == "auto"sv) {
        o.escape_errors = quick_lint_js::escape_errors::auto_;
      } else if (arg_value == "always"sv) {
        o.escape_errors = quick_lint_js::escape_errors::always;
      } else if (arg_value == "never"sv) {
        o.escape_errors = quick_lint_js::escape_errors::never;
      } else {
        o.error_unrecognized_options.emplace_back(arg_value);
      }
    } else if (const char* arg_value =
                   parser.match_option_with_value("--config-file"sv)) {
      active_config_file = arg_value;
      o.has_config_file = true;
    } else if (const char* arg_value = parser.match_option_with_value(
                   "--path-for-config-search"sv)) {
      next_path_for_config_search = arg_value;
    } else if (const char* arg_value =
                   parser.match_option_with_value("--vim-file-bufnr"sv)) {
      int bufnr;
      from_chars_result result =
          from_chars(&arg_value[0], &arg_value[std::strlen(arg_value)], bufnr);
      if (*result.ptr != '\0' || result.ec != std::errc{}) {
        o.error_unrecognized_options.emplace_back(arg_value);
        continue;
      }
      next_vim_file_bufnr = bufnr;
    } else if (const char* arg_value =
                   parser.match_option_with_value("--exit-fail-on"sv)) {
      o.exit_fail_on.add(parse_error_list(arg_value));
    } else if (parser.match_flag_option("--help"sv, "--h"sv) ||
               parser.match_flag_shorthand('h')) {
      o.help = true;
    } else if (parser.match_flag_option("--version"sv, "--v"sv) ||
               parser.match_flag_shorthand('v')) {
      o.version = true;
    } else if (parser.match_flag_option("--lsp-server"sv, "--lsp"sv)) {
      o.lsp_server = true;
    } else if (parser.match_flag_option("--stdin"sv, ""sv)) {
      if (has_stdin) {
        o.has_multiple_stdin = true;
        continue;
      }
      file_to_lint file{.path = "<stdin>",
                        .config_file = active_config_file,
                        .path_for_config_search = next_path_for_config_search,
                        .is_stdin = true,
                        .vim_bufnr = next_vim_file_bufnr};
      o.files_to_lint.emplace_back(file);
      has_stdin = true;
      next_path_for_config_search = nullptr;
      next_vim_file_bufnr = std::nullopt;
    } else {
      const char* unrecognized = parser.match_anything();
      o.error_unrecognized_options.emplace_back(unrecognized);
      goto done_parsing_options;
    }
  }
done_parsing_options:

  return o;
}

bool options::dump_errors(std::ostream& out) const {
  bool have_errors = false;
  if (this->lsp_server) {
    if (this->exit_fail_on.is_user_provided()) {
      out << "warning: --exit-fail-on ignored with --lsp-server\n";
    }
    if (this->output_format != output_format::default_format) {
      out << "warning: --output-format ignored with --lsp-server\n";
    }
    if (!this->files_to_lint.empty()) {
      out << "warning: ignoring files given on command line in "
             "--lsp-server mode\n";
    }
    if (this->has_config_file) {
      out << "warning: --config-file ignored in --lsp-server mode\n";
    }
  }
  if (this->has_multiple_stdin) {
    out << "warning: multiple standard input given on command line\n";
  }
  for (const auto& option : this->error_unrecognized_options) {
    out << "error: unrecognized option: " << option << '\n';
    have_errors = true;
  }
  for (const std::string& error :
       this->exit_fail_on.parse_errors("--exit-fail-on")) {
    out << "error: " << error << '\n';
    have_errors = true;
  }
  for (const std::string& warning : this->exit_fail_on.parse_warnings()) {
    out << "warning: " << warning << '\n';
  }
  return have_errors;
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
