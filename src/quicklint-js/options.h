#ifndef QUICKLINT_JS_OPTIONS_H
#define QUICKLINT_JS_OPTIONS_H

#include <vector>

namespace quicklint_js {
struct options {
  bool print_parser_visits = false;
  std::vector<const char *> files_to_lint;

  std::vector<const char *> error_unrecognized_options;
};

options parse_options(int argc, char **argv);
}  // namespace quicklint_js

#endif
