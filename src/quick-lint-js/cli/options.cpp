// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/arg-parser.h>
#include <quick-lint-js/cli/options.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/integer.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string_view>
#include <vector>

QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
QLJS_WARNING_IGNORE_GCC("-Wshadow=local")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
options parse_options(int argc, char** argv) {
  options o;

  struct {
    std::optional<int> number;
    const char* arg_var;
  } next_vim_file_bufnr;

  const char* next_path_for_config_search = nullptr;
  const char* active_config_file = nullptr;
  std::optional<input_file_language> language;
  const char* unused_language_option = nullptr;
  bool has_stdin = false;

  auto add_file = [&](const char* path, bool is_stdin) {
    if (is_stdin && has_stdin) {
      o.has_multiple_stdin = true;
    } else {
      file_to_lint file{.path = path,
                        .config_file = active_config_file,
                        .path_for_config_search = next_path_for_config_search,
                        .language = language,
                        .is_stdin = is_stdin,
                        .vim_bufnr = next_vim_file_bufnr.number};
      o.files_to_lint.emplace_back(file);
    }
    if (is_stdin) {
      has_stdin = true;
    }

    unused_language_option = nullptr;
    next_path_for_config_search = nullptr;
    next_vim_file_bufnr.number = std::nullopt;
  };

  auto add_stdin_file = [&]() { add_file("<stdin>", /*is_stdin=*/true); };

  auto add_regular_file = [&](const char* path) {
    add_file(path, /*is_stdin=*/false);
  };

  arg_parser parser(argc, argv);
  while (!parser.done()) {
    if (const char* argument = parser.match_argument()) {
      if (argument == "-"sv) {
        add_stdin_file();
      } else {
        add_regular_file(argument);
      }
    } else if (parser.match_flag_option("--debug-parser-visits"sv,
                                        "--debug-p"sv)) {
      o.print_parser_visits = true;
    } else if (const char* arg_value =
                   parser.match_option_with_value("--output-format"sv)) {
      if (arg_value == "gnu-like"sv) {
        o.output_format = output_format::gnu_like;
      } else if (arg_value == "vim-qflist-json"sv) {
        o.output_format = output_format::vim_qflist_json;
      } else if (arg_value == "emacs-lisp"sv) {
        o.output_format = output_format::emacs_lisp;
      } else {
        o.error_unrecognized_options.emplace_back(arg_value);
      }
    } else if (const char* arg_value = parser.match_option_with_value(
                   "--diagnostic-hyperlinks"sv)) {
      if (arg_value == "auto"sv) {
        o.diagnostic_hyperlinks = option_when::auto_;
      } else if (arg_value == "always"sv) {
        o.diagnostic_hyperlinks = option_when::always;
      } else if (arg_value == "never"sv) {
        o.diagnostic_hyperlinks = option_when::never;
      } else {
        o.error_unrecognized_options.emplace_back(arg_value);
      }
    } else if (const char* arg_value =
                   parser.match_option_with_value("--config-file"sv)) {
      active_config_file = arg_value;
      o.has_config_file = true;
    } else if (const char* arg_value =
                   parser.match_option_with_value("--language"sv)) {
      o.has_language = true;
      if (unused_language_option) {
        o.warning_language_without_file.emplace_back(unused_language_option);
      }
      unused_language_option = arg_value;
      if (arg_value == "default"sv) {
        language = std::nullopt;
      } else if (arg_value == "javascript"sv) {
        language = input_file_language::javascript;
      } else if (arg_value == "javascript-jsx"sv) {
        language = input_file_language::javascript_jsx;
      } else if (arg_value == "experimental-typescript"sv) {
        language = input_file_language::typescript;
      } else if (arg_value == "experimental-typescript-jsx"sv) {
        language = input_file_language::typescript_jsx;
      } else {
        o.error_unrecognized_options.emplace_back(arg_value);
      }
    } else if (const char* arg_value = parser.match_option_with_value(
                   "--path-for-config-search"sv)) {
      next_path_for_config_search = arg_value;
    } else if (const char* arg_value =
                   parser.match_option_with_value("--vim-file-bufnr"sv)) {
      o.has_vim_file_bufnr = true;
      int bufnr;
      from_chars_result result =
          from_chars(&arg_value[0], &arg_value[std::strlen(arg_value)], bufnr);
      if (*result.ptr != '\0' || result.ec != std::errc{}) {
        o.error_unrecognized_options.emplace_back(arg_value);
        continue;
      }
      if (next_vim_file_bufnr.number != std::nullopt) {
        o.warning_vim_bufnr_without_file.emplace_back(
            next_vim_file_bufnr.arg_var);
      }
      next_vim_file_bufnr.number = bufnr;
      next_vim_file_bufnr.arg_var = arg_value;
    } else if (const char* arg_value =
                   parser.match_option_with_value("--exit-fail-on"sv)) {
      o.exit_fail_on.add(parse_diag_code_list(arg_value));
    } else if (parser.match_flag_option("--help"sv, "--h"sv) ||
               parser.match_flag_shorthand('h')) {
      o.help = true;
    } else if (parser.match_flag_option("--version"sv, "--v"sv) ||
               parser.match_flag_shorthand('v')) {
      o.version = true;
    } else if (parser.match_flag_option("--lsp-server"sv, "--lsp"sv)) {
      o.lsp_server = true;
    } else if (parser.match_flag_option("--snarky"sv, "--snarky"sv)) {
      o.snarky = true;
    } else if (parser.match_flag_option("--stdin"sv, ""sv)) {
      add_stdin_file();
    } else {
      const char* unrecognized = parser.match_anything();
      o.error_unrecognized_options.emplace_back(unrecognized);
      goto done_parsing_options;
    }
  }
done_parsing_options:

  if (unused_language_option) {
    o.warning_language_without_file.emplace_back(unused_language_option);
  }
  if (next_vim_file_bufnr.number != std::nullopt) {
    o.warning_vim_bufnr_without_file.emplace_back(next_vim_file_bufnr.arg_var);
  }

  return o;
}

bool options::dump_errors(output_stream& out) const {
  bool have_errors = false;
  if (this->lsp_server) {
    if (this->exit_fail_on.is_user_provided()) {
      out.append_copy(
          u8"warning: --exit-fail-on ignored with --lsp-server\n"sv);
    }
    if (this->output_format != output_format::default_format) {
      out.append_copy(
          u8"warning: --output-format ignored with --lsp-server\n"sv);
    }
    if (!this->files_to_lint.empty()) {
      out.append_copy(
          u8"warning: ignoring files given on command line in --lsp-server mode\n"sv);
    }
    if (this->has_config_file) {
      out.append_copy(
          u8"warning: --config-file ignored in --lsp-server mode\n"sv);
    }
  }
  if (this->has_multiple_stdin) {
    out.append_copy(
        u8"warning: multiple standard input given on command line\n"sv);
  }

  if (this->has_vim_file_bufnr && this->lsp_server) {
    out.append_copy(
        u8"warning: ignoring --vim-file-bufnr in --lsp-server mode\n"sv);
  } else if (this->has_vim_file_bufnr &&
             this->output_format != output_format::vim_qflist_json) {
    out.append_copy(
        u8"warning: --output-format selected which doesn't use --vim-file-bufnr\n"sv);
  } else {
    for (const auto& argument : this->warning_vim_bufnr_without_file) {
      QLJS_ASSERT(argument != nullptr);
      out.append_copy(u8"warning: flag: '--vim-file-bufnr="sv);
      out.append_copy(to_string8_view(argument));
      out.append_copy(
          u8"' should be followed by an input file name or --stdin\n"sv);
    }
  }

  if (this->has_language && this->lsp_server) {
    out.append_copy(u8"warning: ignoring --language in --lsp-server mode\n"sv);
  } else {
    for (const char* argument : this->warning_language_without_file) {
      QLJS_ASSERT(argument != nullptr);
      out.append_copy(u8"warning: flag '--language="sv);
      out.append_copy(to_string8_view(argument));
      out.append_copy(
          u8"' should be followed by an input file name or --stdin\n"sv);
    }
  }

  for (const auto& option : this->error_unrecognized_options) {
    out.append_copy(u8"error: unrecognized option: "sv);
    out.append_copy(to_string8_view(option));
    out.append_copy(u8'\n');
    have_errors = true;
  }
  for (const std::string& error :
       this->exit_fail_on.parse_errors("--exit-fail-on")) {
    out.append_copy(u8"error: "sv);
    out.append_copy(to_string8_view(error));
    out.append_copy(u8'\n');
    have_errors = true;
  }
  for (const std::string& warning : this->exit_fail_on.parse_warnings()) {
    out.append_copy(u8"warning: "sv);
    out.append_copy(to_string8_view(warning));
    out.append_copy(u8'\n');
  }

  // Make sure the errors and warnings get displayed before anything else
  // happens.
  out.flush();

  return have_errors;
}

input_file_language file_to_lint::get_language() const noexcept {
  return quick_lint_js::get_language(this->path, this->language);
}

input_file_language get_language(
    const char* config_file,
    const std::optional<input_file_language>& language) noexcept {
  static_cast<void>(config_file);  // Unused for now.
  if (language.has_value()) {
    return *language;
  } else {
    return input_file_language::javascript_jsx;
  }
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
