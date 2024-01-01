// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cli/arg-parser.h>
#include <quick-lint-js/cli/options.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/classify-path.h>
#include <quick-lint-js/util/integer.h>
#include <string_view>
#include <vector>

QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
QLJS_WARNING_IGNORE_GCC("-Wshadow=compatible-local")
QLJS_WARNING_IGNORE_GCC("-Wshadow=local")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
Options parse_options(int argc, char** argv, Monotonic_Allocator* allocator) {
  Options o;

  Monotonic_Allocator temporary_allocator("parse_options");

  struct Next_Vim_File_Bufnr {
    int number;
    const char* arg_var;
  };
  std::optional<Next_Vim_File_Bufnr> next_vim_file_bufnr;

  Vector<File_To_Lint> files_to_lint("files_to_lint", allocator);
  Vector<const char*> error_unrecognized_options("error_unrecognized_options",
                                                 allocator);
  Vector<const char*> warning_vim_bufnr_without_file(
      "warning_vim_bufnr_without_file", allocator);
  Vector<const char*> warning_language_without_file(
      "warning_language_without_file", allocator);

  const char* next_path_for_config_search = nullptr;
  const char* active_config_file = nullptr;
  Raw_Input_File_Language language = Raw_Input_File_Language::default_;
  const char* unused_language_option = nullptr;
  bool has_stdin = false;

  auto add_file = [&](const char* path, bool is_stdin) {
    if (is_stdin && has_stdin) {
      o.has_multiple_stdin = true;
    } else {
      File_To_Lint file{
          .path = path,
          .config_file = active_config_file,
          .path_for_config_search = next_path_for_config_search,
          .language = language,
          .is_stdin = is_stdin,
          .vim_bufnr = next_vim_file_bufnr.has_value()
                           ? std::optional<int>(next_vim_file_bufnr->number)
                           : std::nullopt,
      };
      files_to_lint.emplace_back(file);
    }
    if (is_stdin) {
      has_stdin = true;
    }

    unused_language_option = nullptr;
    next_path_for_config_search = nullptr;
    next_vim_file_bufnr = std::nullopt;
  };

  auto add_stdin_file = [&]() { add_file("<stdin>", /*is_stdin=*/true); };

  auto add_regular_file = [&](const char* path) {
    add_file(path, /*is_stdin=*/false);
  };

  Arg_Parser parser(argc, argv);
  QLJS_ARG_PARSER_LOOP(parser) {
    QLJS_ARGUMENT(const char* argument) {
      if (argument == "-"sv) {
        add_stdin_file();
      } else {
        add_regular_file(argument);
      }
    }

    QLJS_FLAG("--debug-parser-visits"sv, "--debug-p"sv) {
      o.print_parser_visits = true;
    }

    QLJS_OPTION(const char* arg_value, "--output-format"sv) {
      if (arg_value == "gnu-like"sv) {
        o.output_format = Output_Format::gnu_like;
      } else if (arg_value == "vim-qflist-json"sv) {
        o.output_format = Output_Format::vim_qflist_json;
      } else if (arg_value == "emacs-lisp"sv) {
        o.output_format = Output_Format::emacs_lisp;
      } else {
        error_unrecognized_options.emplace_back(arg_value);
      }
    }

    QLJS_OPTION(const char* arg_value, "--diagnostic-hyperlinks"sv) {
      if (arg_value == "auto"sv) {
        o.diagnostic_hyperlinks = Option_When::auto_;
      } else if (arg_value == "always"sv) {
        o.diagnostic_hyperlinks = Option_When::always;
      } else if (arg_value == "never"sv) {
        o.diagnostic_hyperlinks = Option_When::never;
      } else {
        error_unrecognized_options.emplace_back(arg_value);
      }
    }

    QLJS_OPTION(const char* arg_value, "--config-file"sv) {
      active_config_file = arg_value;
      o.has_config_file = true;
    }

    QLJS_OPTION(const char* arg_value, "--language"sv) {
      o.has_language = true;
      if (unused_language_option) {
        warning_language_without_file.emplace_back(unused_language_option);
      }
      unused_language_option = arg_value;
      if (arg_value == "default"sv) {
        language = Raw_Input_File_Language::default_;
      } else if (arg_value == "javascript"sv) {
        language = Raw_Input_File_Language::javascript;
      } else if (arg_value == "javascript-jsx"sv) {
        language = Raw_Input_File_Language::javascript_jsx;
      } else if (arg_value == "typescript"sv) {
        language = Raw_Input_File_Language::typescript;
      } else if (arg_value == "typescript-definition"sv) {
        language = Raw_Input_File_Language::typescript_definition;
      } else if (arg_value == "typescript-jsx"sv) {
        language = Raw_Input_File_Language::typescript_jsx;
      } else {
        error_unrecognized_options.emplace_back(arg_value);
      }
    }

    QLJS_OPTION(const char* arg_value, "--path-for-config-search"sv) {
      next_path_for_config_search = arg_value;
    }

    QLJS_OPTION(const char* arg_value, "--stdin-path"sv) {
      o.path_for_stdin = arg_value;
    }

    QLJS_OPTION(const char* arg_value, "--vim-file-bufnr"sv) {
      o.has_vim_file_bufnr = true;
      int bufnr;
      if (parse_integer_exact(std::string_view(arg_value), bufnr) !=
          Parse_Integer_Exact_Error::ok) {
        error_unrecognized_options.emplace_back(arg_value);
        continue;
      }
      if (next_vim_file_bufnr.has_value()) {
        warning_vim_bufnr_without_file.emplace_back(
            next_vim_file_bufnr->arg_var);
      }
      next_vim_file_bufnr = Next_Vim_File_Bufnr{
          .number = bufnr,
          .arg_var = arg_value,
      };
    }

    QLJS_OPTION(const char* arg_value, "--exit-fail-on"sv) {
      o.exit_fail_on.add(parse_diag_code_list(arg_value, &temporary_allocator));
    }

    QLJS_FLAG('h', "--help"sv, "--h"sv) { o.help = true; }
    QLJS_FLAG("--debug-apps"sv, "--debug-apps"sv) { o.list_debug_apps = true; }
    QLJS_FLAG('v', "--version"sv, "--v"sv) { o.version = true; }
    QLJS_FLAG("--lsp-server"sv, "--lsp"sv) { o.lsp_server = true; }
    QLJS_FLAG("--snarky"sv, "--snarky"sv) { o.snarky = true; }
    QLJS_FLAG("--stdin"sv, ""sv) { add_stdin_file(); }

    QLJS_UNRECOGNIZED_OPTION(const char* unrecognized) {
      error_unrecognized_options.emplace_back(unrecognized);
      goto done_parsing_options;
    }
  }
done_parsing_options:

  if (unused_language_option) {
    warning_language_without_file.emplace_back(unused_language_option);
  }
  if (next_vim_file_bufnr.has_value()) {
    warning_vim_bufnr_without_file.emplace_back(next_vim_file_bufnr->arg_var);
  }
  if (o.path_for_stdin != nullptr) {
    for (File_To_Lint& file : files_to_lint) {
      if (file.path_for_config_search == nullptr) {
        file.path_for_config_search = o.path_for_stdin;
      }
    }
  }

  o.files_to_lint = files_to_lint.release_to_span();
  o.error_unrecognized_options = error_unrecognized_options.release_to_span();
  o.warning_vim_bufnr_without_file =
      warning_vim_bufnr_without_file.release_to_span();
  o.warning_language_without_file =
      warning_language_without_file.release_to_span();
  return o;
}

bool Options::has_stdin() const {
  for (const File_To_Lint& file : this->files_to_lint) {
    if (file.is_stdin) {
      return true;
    }
  }
  return false;
}

bool Options::dump_errors(Output_Stream& out) const {
  Monotonic_Allocator temporary_allocator("dump_errors");

  bool have_errors = false;
  if (this->lsp_server) {
    if (this->exit_fail_on.is_user_provided()) {
      out.append_copy(
          u8"warning: --exit-fail-on ignored with --lsp-server\n"_sv);
    }
    if (this->output_format != Output_Format::default_format) {
      out.append_copy(
          u8"warning: --output-format ignored with --lsp-server\n"_sv);
    }
    if (!this->files_to_lint.empty()) {
      out.append_copy(
          u8"warning: ignoring files given on command line in --lsp-server mode\n"_sv);
    }
    if (this->has_config_file) {
      out.append_copy(
          u8"warning: --config-file ignored in --lsp-server mode\n"_sv);
    }
  }
  if (this->has_multiple_stdin) {
    out.append_copy(
        u8"warning: multiple standard input given on command line\n"_sv);
  }

  if (this->has_vim_file_bufnr && this->lsp_server) {
    out.append_copy(
        u8"warning: ignoring --vim-file-bufnr in --lsp-server mode\n"_sv);
  } else if (this->has_vim_file_bufnr &&
             this->output_format != Output_Format::vim_qflist_json) {
    out.append_copy(
        u8"warning: --output-format selected which doesn't use --vim-file-bufnr\n"_sv);
  } else {
    for (const auto& argument : this->warning_vim_bufnr_without_file) {
      QLJS_ASSERT(argument != nullptr);
      out.append_copy(u8"warning: flag: '--vim-file-bufnr="_sv);
      out.append_copy(to_string8_view(argument));
      out.append_copy(
          u8"' should be followed by an input file name or --stdin\n"_sv);
    }
  }

  if (this->has_language && this->lsp_server) {
    out.append_copy(u8"warning: ignoring --language in --lsp-server mode\n"_sv);
  } else {
    for (const char* argument : this->warning_language_without_file) {
      QLJS_ASSERT(argument != nullptr);
      out.append_copy(u8"warning: flag '--language="_sv);
      out.append_copy(to_string8_view(argument));
      out.append_copy(
          u8"' should be followed by an input file name or --stdin\n"_sv);
    }
  }

  if (this->path_for_stdin != nullptr && !this->has_stdin()) {
    out.append_copy(
        u8"warning: '--stdin-path' has no effect without --stdin\n"_sv);
  }

  for (const auto& option : this->error_unrecognized_options) {
    out.append_copy(u8"error: unrecognized option: "_sv);
    out.append_copy(to_string8_view(option));
    out.append_copy(u8'\n');
    have_errors = true;
  }
  for (std::string_view error : this->exit_fail_on.parse_errors(
           "--exit-fail-on", &temporary_allocator)) {
    out.append_copy(u8"error: "_sv);
    out.append_copy(to_string8_view(error));
    out.append_copy(u8'\n');
    have_errors = true;
  }
  for (std::string_view warning :
       this->exit_fail_on.parse_warnings(&temporary_allocator)) {
    out.append_copy(u8"warning: "_sv);
    out.append_copy(to_string8_view(warning));
    out.append_copy(u8'\n');
  }

  // Make sure the errors and warnings get displayed before anything else
  // happens.
  out.flush();

  return have_errors;
}

Resolved_Input_File_Language get_language(const File_To_Lint& file,
                                          const Options& options) {
  const char* path = file.is_stdin && options.path_for_stdin != nullptr
                         ? options.path_for_stdin
                         : file.path;
  return get_language(path, file.language);
}

Resolved_Input_File_Language get_language(const char* file,
                                          Raw_Input_File_Language language) {
  if (language == Raw_Input_File_Language::default_) {
    Path_Classification classification = classify_path(file);
    if (classification.typescript_jsx) {
      return Resolved_Input_File_Language::typescript_jsx;
    } else if (classification.typescript) {
      if (classification.typescript_definition) {
        return Resolved_Input_File_Language::typescript_definition;
      }
      return Resolved_Input_File_Language::typescript;
    } else {
      return Resolved_Input_File_Language::javascript_jsx;
    }
  } else {
    return int_to_enum_cast<Resolved_Input_File_Language>(
        enum_to_int_cast(language));
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
