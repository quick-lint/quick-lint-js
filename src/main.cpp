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

#include <cerrno>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/options.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/text-error-reporter.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/vim-qflist-json-error-reporter.h>
#include <string>
#include <variant>

namespace quick_lint_js {
namespace {
class any_error_reporter {
 public:
  static any_error_reporter make(output_format format) {
    switch (format) {
      case output_format::gnu_like:
        return any_error_reporter(text_error_reporter(std::cerr));
      case output_format::vim_qflist_json:
        return any_error_reporter(vim_qflist_json_error_reporter(std::cout));
    }
    QLJS_UNREACHABLE();
  }

  void set_source(padded_string_view input, const file_to_lint &file) {
    std::visit(
        [&](auto &r) {
          using reporter_type = std::decay_t<decltype(r)>;
          if constexpr (std::is_base_of_v<vim_qflist_json_error_reporter,
                                          reporter_type>) {
            r.set_source(input, file.path, file.vim_bufnr);
          } else {
            r.set_source(input, file.path);
          }
        },
        this->reporter_);
  }

  error_reporter *get() noexcept {
    return std::visit([](error_reporter &r) { return &r; }, this->reporter_);
  }

  void finish() {
    std::visit(
        [&](auto &r) {
          using reporter_type = std::decay_t<decltype(r)>;
          if constexpr (std::is_base_of_v<vim_qflist_json_error_reporter,
                                          reporter_type>) {
            r.finish();
          }
        },
        this->reporter_);
  }

 private:
  using reporter_variant =
      std::variant<text_error_reporter, vim_qflist_json_error_reporter>;

  explicit any_error_reporter(reporter_variant &&reporter)
      : reporter_(std::move(reporter)) {}

  reporter_variant reporter_;
};

void process_file(padded_string_view input, error_reporter *,
                  bool print_parser_visits);
}
}

int main(int argc, char **argv) {
  quick_lint_js::vector_instrumentation::register_dump_on_exit_if_requested();

  quick_lint_js::options o = quick_lint_js::parse_options(argc, argv);
  if (!o.error_unrecognized_options.empty()) {
    for (const auto &option : o.error_unrecognized_options) {
      std::cerr << "error: unrecognized option: " << option << '\n';
    }
    return 1;
  }
  if (o.files_to_lint.empty()) {
    std::cerr << "error: expected file name\n";
    return 1;
  }

  quick_lint_js::any_error_reporter reporter =
      quick_lint_js::any_error_reporter::make(o.output_format);
  for (const quick_lint_js::file_to_lint &file : o.files_to_lint) {
    quick_lint_js::read_file_result source =
        quick_lint_js::read_file(file.path);
    source.exit_if_not_ok();
    reporter.set_source(&source.content, file);
    quick_lint_js::process_file(&source.content, reporter.get(),
                                o.print_parser_visits);
  }
  reporter.finish();

  return 0;
}

namespace quick_lint_js {
namespace {
class debug_visitor {
 public:
  void visit_end_of_module() { std::cerr << "end of module\n"; }

  void visit_enter_block_scope() { std::cerr << "entered block scope\n"; }

  void visit_enter_class_scope() { std::cerr << "entered class scope\n"; }

  void visit_enter_for_scope() { std::cerr << "entered for scope\n"; }

  void visit_enter_function_scope() { std::cerr << "entered function scope\n"; }

  void visit_enter_function_scope_body() {
    std::cerr << "entered function scope body\n";
  }

  void visit_enter_named_function_scope(identifier) {
    std::cerr << "entered named function scope\n";
  }

  void visit_exit_block_scope() { std::cerr << "exited block scope\n"; }

  void visit_exit_class_scope() { std::cerr << "exited class scope\n"; }

  void visit_exit_for_scope() { std::cerr << "exited for scope\n"; }

  void visit_exit_function_scope() { std::cerr << "exited function scope\n"; }

  void visit_property_declaration(identifier name) {
    std::cerr << "property declaration: " << out_string8(name.string_view())
              << '\n';
  }

  void visit_variable_assignment(identifier name) {
    std::cerr << "variable assignment: " << out_string8(name.string_view())
              << '\n';
  }

  void visit_variable_declaration(identifier name, variable_kind) {
    std::cerr << "variable declaration: " << out_string8(name.string_view())
              << '\n';
  }

  void visit_variable_typeof_use(identifier name) {
    std::cerr << "variable typeof use: " << out_string8(name.string_view())
              << '\n';
  }

  void visit_variable_use(identifier name) {
    std::cerr << "variable use: " << out_string8(name.string_view()) << '\n';
  }
};

template <QLJS_PARSE_VISITOR Visitor1, QLJS_PARSE_VISITOR Visitor2>
class multi_visitor {
 public:
  explicit multi_visitor(Visitor1 *visitor_1, Visitor2 *visitor_2) noexcept
      : visitor_1_(visitor_1), visitor_2_(visitor_2) {}

  void visit_end_of_module() {
    this->visitor_1_->visit_end_of_module();
    this->visitor_2_->visit_end_of_module();
  }

  void visit_enter_block_scope() {
    this->visitor_1_->visit_enter_block_scope();
    this->visitor_2_->visit_enter_block_scope();
  }

  void visit_enter_class_scope() {
    this->visitor_1_->visit_enter_class_scope();
    this->visitor_2_->visit_enter_class_scope();
  }

  void visit_enter_for_scope() {
    this->visitor_1_->visit_enter_for_scope();
    this->visitor_2_->visit_enter_for_scope();
  }

  void visit_enter_function_scope() {
    this->visitor_1_->visit_enter_function_scope();
    this->visitor_2_->visit_enter_function_scope();
  }

  void visit_enter_function_scope_body() {
    this->visitor_1_->visit_enter_function_scope_body();
    this->visitor_2_->visit_enter_function_scope_body();
  }

  void visit_enter_named_function_scope(identifier name) {
    this->visitor_1_->visit_enter_named_function_scope(name);
    this->visitor_2_->visit_enter_named_function_scope(name);
  }

  void visit_exit_block_scope() {
    this->visitor_1_->visit_exit_block_scope();
    this->visitor_2_->visit_exit_block_scope();
  }

  void visit_exit_class_scope() {
    this->visitor_1_->visit_exit_class_scope();
    this->visitor_2_->visit_exit_class_scope();
  }

  void visit_exit_for_scope() {
    this->visitor_1_->visit_exit_for_scope();
    this->visitor_2_->visit_exit_for_scope();
  }

  void visit_exit_function_scope() {
    this->visitor_1_->visit_exit_function_scope();
    this->visitor_2_->visit_exit_function_scope();
  }

  void visit_property_declaration(identifier name) {
    this->visitor_1_->visit_property_declaration(name);
    this->visitor_2_->visit_property_declaration(name);
  }

  void visit_variable_assignment(identifier name) {
    this->visitor_1_->visit_variable_assignment(name);
    this->visitor_2_->visit_variable_assignment(name);
  }

  void visit_variable_declaration(identifier name, variable_kind kind) {
    this->visitor_1_->visit_variable_declaration(name, kind);
    this->visitor_2_->visit_variable_declaration(name, kind);
  }

  void visit_variable_typeof_use(identifier name) {
    this->visitor_1_->visit_variable_typeof_use(name);
    this->visitor_2_->visit_variable_typeof_use(name);
  }

  void visit_variable_use(identifier name) {
    this->visitor_1_->visit_variable_use(name);
    this->visitor_2_->visit_variable_use(name);
  }

 private:
  Visitor1 *visitor_1_;
  Visitor2 *visitor_2_;
};

void process_file(padded_string_view input, error_reporter *error_reporter,
                  bool print_parser_visits) {
  parser p(input, error_reporter);
  linter l(error_reporter);
  if (print_parser_visits) {
    debug_visitor logger;
    multi_visitor visitor(&logger, &l);
    p.parse_and_visit_module(visitor);
  } else {
    p.parse_and_visit_module(l);
  }
}
}
}
