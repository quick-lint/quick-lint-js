// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <getopt.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/options.h>
#include <string_view>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
options parse_options(int argc, char **argv) {
  options o;

  enum option_value {
    no_option = -1,
    flag_option = 0,
    non_option = 1,
    unknown_option = '?',
    option_missing_argument = ':',

    debug_parser_visits = 0x80,
    output_format,
    vim_file_bufnr,
  };

  static const struct ::option getopt_long_options[] = {
      {"debug-parser-visits", no_argument, nullptr, debug_parser_visits},
      {"output-format", required_argument, nullptr, output_format},
      {"vim-file-bufnr", required_argument, nullptr, vim_file_bufnr},
      {nullptr, 0, nullptr, 0},
  };
  static const char getopt_short_options[] = "-";

  std::optional<int> next_vim_file_bufnr;

  ::optind = 1;
  for (;;) {
    int option_index;
    int c = ::getopt_long(argc, argv, getopt_short_options, getopt_long_options,
                          &option_index);
    switch (c) {
      case debug_parser_visits:
        o.print_parser_visits = true;
        break;

      case output_format:
        if (optarg == "gnu-like"sv) {
          o.output_format = quick_lint_js::output_format::gnu_like;
        } else if (optarg == "vim-qflist-json"sv) {
          o.output_format = quick_lint_js::output_format::vim_qflist_json;
        } else {
          o.error_unrecognized_options.emplace_back(optarg);
        }
        break;

      case vim_file_bufnr: {
        int bufnr;
        from_chars_result result =
            from_chars(::optarg, &::optarg[std::strlen(::optarg)], bufnr);
        if (*result.ptr != '\0' || result.ec != std::errc{}) {
          o.error_unrecognized_options.emplace_back(::optarg);
        } else {
          next_vim_file_bufnr = bufnr;
        }
        break;
      }

      case non_option: {
        file_to_lint file{.path = argv[::optind - 1],
                          .vim_bufnr = next_vim_file_bufnr};
        o.files_to_lint.emplace_back(file);
        next_vim_file_bufnr = std::nullopt;
        break;
      }

      case no_option:
        assert(::optind == argc);
        goto done_parsing_options;

      case unknown_option:
        o.error_unrecognized_options.emplace_back(argv[::optind - 1]);
        goto done_parsing_options;

      case option_missing_argument:
        std::abort();
        break;

      case flag_option:
        assert(false);
        break;

      default:
        assert(false);
        break;
    }
  }
done_parsing_options:

  return o;
}
}  // namespace quick_lint_js
