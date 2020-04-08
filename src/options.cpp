#include <cassert>
#include <cstdlib>
#include <getopt.h>
#include <quicklint-js/options.h>
#include <vector>

namespace quicklint_js {
options parse_options(int argc, char **argv) {
  options o;

  enum option_value {
    no_option = -1,
    flag_option = 0,
    unknown_option = '?',
    option_missing_argument = ':',

    debug_parser_visits = 0x80,
  };

  static const struct ::option getopt_long_options[] = {
      {"debug-parser-visits", no_argument, nullptr, debug_parser_visits},
      {nullptr, 0, nullptr, 0},
  };
  static const char getopt_short_options[] = "";

  ::optind = 1;
  for (;;) {
    int option_index;
    int c = ::getopt_long(argc, argv, getopt_short_options, getopt_long_options,
                          &option_index);
    switch (c) {
      case debug_parser_visits:
        o.print_parser_visits = true;
        break;

      case no_option:
        for (int i = ::optind; i < argc; ++i) {
          o.files_to_lint.emplace_back(argv[i]);
        }
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
}  // namespace quicklint_js
