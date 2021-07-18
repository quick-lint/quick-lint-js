// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdio>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <optional>
#include <quick-lint-js/change-detecting-filesystem.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/emacs-lisp-error-reporter.h>
#include <quick-lint-js/emacs-location.h>
#include <quick-lint-js/error-list.h>
#include <quick-lint-js/error-tape.h>
#include <quick-lint-js/event-loop.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/lsp-pipe-writer.h>
#include <quick-lint-js/lsp-server.h>
#include <quick-lint-js/options.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/pipe-writer.h>
#include <quick-lint-js/text-error-reporter.h>
#include <quick-lint-js/translation.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/utf-16.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/version.h>
#include <quick-lint-js/vim-qflist-json-error-reporter.h>
#include <string>
#include <tuple>
#include <unordered_map>
#include <variant>

#if QLJS_HAVE_KQUEUE
#include <sys/event.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace quick_lint_js {
namespace {
class any_error_reporter {
 public:
  static any_error_reporter make(output_format format,
                                 compiled_error_list *exit_fail_on) {
    switch (format) {
    case output_format::default_format:
    case output_format::gnu_like:
      return any_error_reporter(error_tape<text_error_reporter>(
          text_error_reporter(std::cerr), exit_fail_on));
    case output_format::vim_qflist_json:
      return any_error_reporter(error_tape<vim_qflist_json_error_reporter>(
          vim_qflist_json_error_reporter(std::cout), exit_fail_on));
    case output_format::emacs_lisp:
      return any_error_reporter(error_tape<emacs_lisp_error_reporter>(
          emacs_lisp_error_reporter(std::cout), exit_fail_on));
    }
    QLJS_UNREACHABLE();
  }

  void set_source(padded_string_view input, const file_to_lint &file) {
    std::visit(
        [&](auto &r) {
          using reporter_type = std::decay_t<decltype(r)>;
          if constexpr (std::is_base_of_v<
                            error_tape<vim_qflist_json_error_reporter>,
                            reporter_type>) {
            r.get_reporter()->set_source(input, file.path, file.vim_bufnr);
          } else if constexpr (std::is_base_of_v<
                                   error_tape<emacs_lisp_error_reporter>,
                                   reporter_type>) {
            r.get_reporter()->set_source(input);
          } else {
            r.get_reporter()->set_source(input, file.path);
          }
        },
        this->tape_);
  }

  error_reporter *get() noexcept {
    return std::visit([](error_reporter &r) { return &r; }, this->tape_);
  }

  bool get_error() noexcept {
    return std::visit([](auto &r) { return r.found_matching_error(); },
                      this->tape_);
  }

  void finish() {
    std::visit(
        [&](auto &r) {
          using reporter_type = std::decay_t<decltype(r)>;
          if constexpr (std::is_base_of_v<
                            error_tape<vim_qflist_json_error_reporter>,
                            reporter_type>) {
            r.get_reporter()->finish();
          } else if constexpr (std::is_base_of_v<
                                   error_tape<emacs_lisp_error_reporter>,
                                   reporter_type>) {
            r.get_reporter()->finish();
          }
        },
        this->tape_);
  }

 private:
  using tape_variant = std::variant<error_tape<text_error_reporter>,
                                    error_tape<vim_qflist_json_error_reporter>,
                                    error_tape<emacs_lisp_error_reporter>>;

  explicit any_error_reporter(tape_variant &&tape) : tape_(tape) {}

  tape_variant tape_;
};

[[noreturn]] void handle_options(quick_lint_js::options o);

void process_file(padded_string_view input, configuration &, error_reporter *,
                  bool print_parser_visits);

void run_lsp_server();

void print_help_message();
void print_version_information();
}
}

#if defined(_WIN32)
int wmain(int argc, wchar_t **wargv) {
  quick_lint_js::vector_instrumentation::register_dump_on_exit_if_requested();
  quick_lint_js::initialize_translations_from_environment();

  quick_lint_js::mbargv m(argc, wargv);
  quick_lint_js::options o = quick_lint_js::parse_options(m.size(), m.data());
  quick_lint_js::handle_options(o);
}
#else
int main(int argc, char **argv) {
  quick_lint_js::vector_instrumentation::register_dump_on_exit_if_requested();
  quick_lint_js::initialize_translations_from_environment();

  quick_lint_js::options o = quick_lint_js::parse_options(argc, argv);
  quick_lint_js::handle_options(o);
}
#endif

namespace quick_lint_js {
namespace {
void handle_options(quick_lint_js::options o) {
  if (o.help) {
    quick_lint_js::print_help_message();
    std::exit(EXIT_SUCCESS);
  }
  if (o.version) {
    quick_lint_js::print_version_information();
    std::exit(EXIT_SUCCESS);
  }
  if (o.dump_errors(std::cerr)) {
    std::exit(EXIT_FAILURE);
  }
  if (o.lsp_server) {
    quick_lint_js::run_lsp_server();
    std::exit(EXIT_SUCCESS);
  }
  if (o.files_to_lint.empty()) {
    std::cerr << "error: expected file name\n";
    std::exit(EXIT_FAILURE);
  }

  quick_lint_js::any_error_reporter reporter =
      quick_lint_js::any_error_reporter::make(o.output_format, &o.exit_fail_on);

  configuration_loader config_loader(
      basic_configuration_filesystem::instance());
  for (const file_to_lint &file : o.files_to_lint) {
    auto config_result = config_loader.load_for_file(file);
    if (!config_result.ok()) {
      std::fprintf(stderr, "error: %s\n",
                   config_result.error_to_string().c_str());
      std::exit(1);
    }
    loaded_config_file *config_file = *config_result;
    if (config_file && !config_file->config.errors_were_reported()) {
      if (const std::optional<canonical_path> &config_path =
              config_file->config.config_file_path()) {
        reporter.set_source(&config_file->file_content,
                            file_to_lint{
                                .path = config_path->c_str(),
                                .config_file = nullptr,
                                .is_stdin = false,
                                .vim_bufnr = std::nullopt,
                            });
        config_file->config.report_errors(reporter.get());
      }
    }
  }

  if (!reporter.get_error()) {
    for (const quick_lint_js::file_to_lint &file : o.files_to_lint) {
      auto config_result = config_loader.load_for_file(file);
      QLJS_ASSERT(config_result.ok());
      configuration *config = *config_result
                                  ? &(*config_result)->config
                                  : config_loader.get_default_config();
      result<padded_string, read_file_io_error> source =
          file.is_stdin ? quick_lint_js::read_stdin()
                        : quick_lint_js::read_file(file.path);
      if (!source.ok()) {
        source.error().print_and_exit();
      }
      reporter.set_source(&*source, file);
      quick_lint_js::process_file(&*source, *config, reporter.get(),
                                  o.print_parser_visits);
    }
  }
  reporter.finish();

  if (reporter.get_error() == true &&
      o.output_format != output_format::emacs_lisp) {
    std::exit(EXIT_FAILURE);
  }

  std::exit(EXIT_SUCCESS);
}

class debug_visitor {
 public:
  void visit_end_of_module() { std::cerr << "end of module\n"; }

  void visit_enter_block_scope() { std::cerr << "entered block scope\n"; }

  void visit_enter_with_scope() { std::cerr << "entered with scope\n"; }

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

  void visit_exit_with_scope() { std::cerr << "exited with scope\n"; }

  void visit_exit_class_scope() { std::cerr << "exited class scope\n"; }

  void visit_exit_for_scope() { std::cerr << "exited for scope\n"; }

  void visit_exit_function_scope() { std::cerr << "exited function scope\n"; }

  void visit_property_declaration(std::optional<identifier> name) {
    std::cerr << "property declaration";
    if (name.has_value()) {
      std::cerr << ": " << out_string8(name->normalized_name());
    }
    std::cerr << '\n';
  }

  void visit_variable_assignment(identifier name) {
    std::cerr << "variable assignment: " << out_string8(name.normalized_name())
              << '\n';
  }

  void visit_variable_declaration(identifier name, variable_kind) {
    std::cerr << "variable declaration: " << out_string8(name.normalized_name())
              << '\n';
  }

  void visit_variable_export_use(identifier name) {
    std::cerr << "variable export use: " << out_string8(name.normalized_name())
              << '\n';
  }

  void visit_variable_typeof_use(identifier name) {
    std::cerr << "variable typeof use: " << out_string8(name.normalized_name())
              << '\n';
  }

  void visit_variable_use(identifier name) {
    std::cerr << "variable use: " << out_string8(name.normalized_name())
              << '\n';
  }
};
QLJS_STATIC_ASSERT_IS_PARSE_VISITOR(debug_visitor);

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

  void visit_enter_with_scope() {
    this->visitor_1_->visit_enter_with_scope();
    this->visitor_2_->visit_enter_with_scope();
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

  void visit_exit_with_scope() {
    this->visitor_1_->visit_exit_with_scope();
    this->visitor_2_->visit_exit_with_scope();
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

  void visit_property_declaration(std::optional<identifier> name) {
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

  void visit_variable_export_use(identifier name) {
    this->visitor_1_->visit_variable_export_use(name);
    this->visitor_2_->visit_variable_export_use(name);
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
QLJS_STATIC_ASSERT_IS_PARSE_VISITOR(
    multi_visitor<debug_visitor, debug_visitor>);

void process_file(padded_string_view input, configuration &config,
                  error_reporter *error_reporter, bool print_parser_visits) {
  parser p(input, error_reporter);
  linter l(error_reporter, &config.globals());
  // TODO(strager): Use parse_and_visit_module_catching_unimplemented instead of
  // parse_and_visit_module to avoid crashing on QLJS_PARSER_UNIMPLEMENTED.
  if (print_parser_visits) {
    buffering_visitor v(p.buffering_visitor_memory());
    p.parse_and_visit_module(v);

    debug_visitor logger;
    multi_visitor visitor(&logger, &l);
    v.move_into(visitor);
  } else {
    p.parse_and_visit_module(l);
  }
}

void run_lsp_server() {
#if defined(_WIN32)
  windows_handle_file_ref input_pipe(::GetStdHandle(STD_INPUT_HANDLE));
  windows_handle_file_ref output_pipe(::GetStdHandle(STD_OUTPUT_HANDLE));
#else
  posix_fd_file_ref input_pipe(STDIN_FILENO);
  posix_fd_file_ref output_pipe(STDOUT_FILENO);
#endif
  basic_configuration_filesystem fs;

  class lsp_event_loop : public event_loop<lsp_event_loop> {
   public:
    explicit lsp_event_loop(platform_file_ref input_pipe,
                            platform_file_ref output_pipe)
        :
#if QLJS_HAVE_KQUEUE
          fs_(this->kqueue_fd(),
              reinterpret_cast<void *>(event_udata_fs_changed)),
#elif QLJS_HAVE_INOTIFY
          fs_(),
#elif defined(_WIN32)
          fs_(this->io_completion_port(), completion_key_fs_changed),
#else
#error "Unsupported platform"
#endif
          input_pipe_(input_pipe),
          endpoint_(std::forward_as_tuple(&this->fs_),
                    std::forward_as_tuple(output_pipe)) {
    }

    platform_file_ref get_readable_pipe() const { return this->input_pipe_; }

    void append(string8_view data) { this->endpoint_.append(data); }

#if QLJS_HAVE_KQUEUE || QLJS_HAVE_POLL
    std::optional<posix_fd_file_ref> get_pipe_write_fd() {
      return this->endpoint_.remote().get_event_fd();
    }
#endif

#if QLJS_HAVE_KQUEUE
    void on_pipe_write_event(const struct ::kevent &event) {
      this->endpoint_.remote().on_poll_event(event);
    }
#elif QLJS_HAVE_POLL
    void on_pipe_write_event(const ::pollfd &event) {
      this->endpoint_.remote().on_poll_event(event);
    }
#endif

#if QLJS_HAVE_KQUEUE
    void on_fs_changed_kevents() { this->endpoint_.filesystem_changed(); }
#endif

#if QLJS_HAVE_INOTIFY
    std::optional<posix_fd_file_ref> get_inotify_fd() {
      return this->fs_.get_inotify_fd();
    }

    void on_fs_changed_event(const ::pollfd &event) {
      this->fs_.handle_poll_event(event);
      this->endpoint_.filesystem_changed();
    }
#endif

#if defined(_WIN32)
    void on_fs_changed_event(::OVERLAPPED *overlapped,
                             ::DWORD number_of_bytes_transferred,
                             ::DWORD error) {
      bool fs_changed = this->fs_.handle_event(
          overlapped, number_of_bytes_transferred, error);
      if (fs_changed) {
        this->endpoint_.filesystem_changed();
      }
    }
#endif

   private:
#if QLJS_HAVE_KQUEUE
    change_detecting_filesystem_kqueue fs_;
#elif QLJS_HAVE_INOTIFY
    change_detecting_filesystem_inotify fs_;
#elif defined(_WIN32)
    change_detecting_filesystem_win32 fs_;
#else
#error "Unsupported platform"
#endif

    platform_file_ref input_pipe_;
    lsp_endpoint<linting_lsp_server_handler<lsp_javascript_linter>,
                 lsp_pipe_writer>
        endpoint_;
  };

#if QLJS_EVENT_LOOP_READ_PIPE_NON_BLOCKING
  input_pipe.set_pipe_non_blocking();
#endif
#if !QLJS_PIPE_WRITER_SEPARATE_THREAD
  output_pipe.set_pipe_non_blocking();
#endif
  lsp_event_loop server(input_pipe, output_pipe);
  server.run();
}

void print_help_message() {
  int max_width = 36;

  auto print_option = [&](const char *abbr, const char *message) {
    std::cout << std::setw(max_width) << std::left << abbr << message << '\n';
  };

  std::cout << "Usage: quick-lint-js [OPTIONS]... [FILE]...\n\n"
            << "OPTIONS\n";
  print_option("--config-file=[FILE]",
               "Read configuration from a JSON file for later input files");
  print_option("--exit-fail-on=[CODES]",
               "Fail with a non-zero exit code if any of these");
  print_option("", "errors are found (default: \"all\")");
  print_option("--stdin", "Read standard input as a JavaScript file");
  print_option("--lsp, --lsp-server",
               "Run in Language Server mode (for LSP-aware editors)");
  print_option("--output-format=[FORMAT]",
               "Format to print feedback where FORMAT is one of:");
  print_option("",
               "gnu-like (default if omitted), vim-qflist-json, emacs-lisp");
  print_option("-v, --version", "Print version information");
  print_option("--vim-file-bufnr=[NUMBER]",
               "Select a vim buffer for outputting feedback");
  print_option("-h, --help", "Print help message");

  bool mention_man_page = false;
#if defined(_POSIX2_VERSION)
  mention_man_page = true;
#endif
  if (mention_man_page) {
    std::cout << "\nFor more information, run 'man quick-lint-js' or visit\n"
                 "https://quick-lint-js.com/cli/\n";
  } else {
    std::cout
        << "\nFor more information, visit https://quick-lint-js.com/cli/\n";
  }
}

void print_version_information() {
  std::cout << "quick-lint-js version " << QUICK_LINT_JS_VERSION_STRING << '\n';
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
