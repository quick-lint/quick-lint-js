#include <cstdlib>
#include <iostream>
#include <quicklint-js/lex.h>
#include <quicklint-js/parse.h>

namespace quicklint_js {
void parser::crash_on_unimplemented_token(const char *qljs_file_name,
                                          int qljs_line,
                                          const char *qljs_function_name) {
  std::cerr << qljs_file_name << ":" << qljs_line
            << ": fatal: token not implemented in " << qljs_function_name
            << ": " << this->peek().type << '\n';
  std::abort();
}
}  // namespace quicklint_js
