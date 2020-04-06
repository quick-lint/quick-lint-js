#include <cerrno>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <quicklint-js/error.h>
#include <quicklint-js/lex.h>
#include <quicklint-js/location.h>
#include <quicklint-js/parse.h>
#include <string>

namespace quicklint_js {
namespace {
void process_file(const char *path);
std::string read_file(const char *path);
}  // namespace
}  // namespace quicklint_js

int main(int argc, char **argv) {
  if (argc < 2) {
    std::cerr << "error: expected file name\n";
  }

  for (int i = 1; i < argc; ++i) {
    quicklint_js::process_file(argv[i]);
  }

  return 0;
}

namespace quicklint_js {
namespace {
class debug_error_reporter : public error_reporter {
 public:
  void report_error_invalid_binding_in_let_statement(
      source_code_span) override {
    std::cerr << "error: invalid binding in let statement\n";
  }

  void report_error_let_with_no_bindings(source_code_span) override {
    std::cerr << "error: let with no bindings\n";
  }

  void report_error_missing_oprand_for_operator(source_code_span) override {
    std::cerr << "error: missing oprand for operator\n";
  }

  void report_error_stray_comma_in_let_statement(source_code_span) override {
    std::cerr << "error: stray comma in let statement\n";
  }

  void report_error_unclosed_block_comment(source_code_span) override {
    std::cerr << "error: unclosed block comment\n";
  }

  void report_error_unclosed_string_literal(source_code_span) override {
    std::cerr << "error: unclosed string literal\n";
  }

  void report_error_unexpected_identifier(source_code_span) override {
    std::cerr << "error: unexpected identifier\n";
  }

  void report_error_unmatched_parenthesis(source_code_span) override {
    std::cerr << "error: unmatched parenthesis\n";
  }

  void report_error_variable_used_before_declaration(identifier) override {
    std::cerr << "error: variable used before declaration\n";
  }
};

class debug_visitor {
 public:
  void visit_enter_function_scope() { std::cerr << "entered function scope\n"; }

  void visit_exit_function_scope() { std::cerr << "exited function scope\n"; }

  void visit_variable_declaration(identifier name) {
    std::cerr << "variable declaration: " << name.string_view() << '\n';
  }

  void visit_variable_use(identifier name) {
    std::cerr << "variable use: " << name.string_view() << '\n';
  }
};

void process_file(const char *path) {
  std::string source = read_file(path);
  debug_error_reporter error_reporter;
  parser p(source.c_str(), &error_reporter);
  debug_visitor visitor;
  p.parse_module(visitor);
}

std::string read_file(const char *path) {
  FILE *file = std::fopen(path, "rb");
  if (!file) {
    std::cerr << "error: failed to open " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  if (std::fseek(file, 0, SEEK_END) == -1) {
    std::cerr << "error: failed to seek to end of " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  long file_size = std::ftell(file);
  if (file_size == -1) {
    std::cerr << "error: failed to get size of " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  if (std::fseek(file, 0, SEEK_SET) == -1) {
    std::cerr << "error: failed to seek to beginning of " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  std::string contents;
  contents.resize(file_size);
  std::size_t read_size = std::fread(contents.data(), 1, file_size, file);
  contents.resize(read_size);

  if (std::fclose(file) == -1) {
    std::cerr << "error: failed to close " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  return contents;
}
}  // namespace
}  // namespace quicklint_js
