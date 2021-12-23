// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdio>
#include <cstring>
#include <iomanip>
#include <optional>
#include <quick-lint-js/basic-configuration-filesystem.h>
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
#include <quick-lint-js/variant.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/version.h>
#include <quick-lint-js/vim-qflist-json-error-reporter.h>
#include <string>
#include <tuple>
#include <unordered_map>

#if QLJS_HAVE_KQUEUE
#include <sys/event.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace quick_lint_js {
namespace {
bool stderr_supports_terminal_escapes() {
#if defined(_WIN32)
  return false;
#else
  return isatty(STDERR_FILENO);
#endif
}

bool get_escape_errors(option_when escape_errors) {
  switch (escape_errors) {
  case option_when::auto_:
    return stderr_supports_terminal_escapes();
  case option_when::always:
    return true;
  case option_when::never:
    return false;
  }
  QLJS_UNREACHABLE();
}

class any_error_reporter {
 public:
  static any_error_reporter make(output_format format,
                                 option_when escape_errors,
                                 compiled_error_list *exit_fail_on) {
    switch (format) {
    case output_format::default_format:
    case output_format::gnu_like:
      return any_error_reporter(error_tape<text_error_reporter>(
          text_error_reporter(
              file_output_stream::get_stderr(),
              /*escape_errors=*/get_escape_errors(escape_errors)),
          exit_fail_on));
    case output_format::vim_qflist_json:
      return any_error_reporter(error_tape<vim_qflist_json_error_reporter>(
          vim_qflist_json_error_reporter(file_output_stream::get_stdout()),
          exit_fail_on));
    case output_format::emacs_lisp:
      return any_error_reporter(error_tape<emacs_lisp_error_reporter>(
          emacs_lisp_error_reporter(file_output_stream::get_stdout()),
          exit_fail_on));
    }
    QLJS_UNREACHABLE();
  }

  void set_source(padded_string_view input, const file_to_lint &file) {
    visit(
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
    return visit([](error_reporter &r) { return &r; }, this->tape_);
  }

  bool get_error() noexcept {
    return visit([](auto &r) { return r.found_matching_error(); }, this->tape_);
  }

  void finish() {
    visit(
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
  using tape_variant = variant<error_tape<text_error_reporter>,
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
  if (o.dump_errors(*file_output_stream::get_stderr())) {
    std::exit(EXIT_FAILURE);
  }
  if (o.lsp_server) {
    quick_lint_js::run_lsp_server();
    std::exit(EXIT_SUCCESS);
  }
  if (o.files_to_lint.empty()) {
    std::fprintf(stderr, "error: expected file name\n");
    std::exit(EXIT_FAILURE);
  }

  quick_lint_js::any_error_reporter reporter =
      quick_lint_js::any_error_reporter::make(
          o.output_format, o.diagnostic_hyperlinks, &o.exit_fail_on);

  configuration default_config;
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
    if (config_file && !config_file->config.errors_were_reported) {
      reporter.set_source(&config_file->file_content,
                          file_to_lint{
                              .path = config_file->config_path->c_str(),
                              .config_file = nullptr,
                              .is_stdin = false,
                              .vim_bufnr = std::nullopt,
                          });
      config_file->errors.copy_into(reporter.get());
      config_file->config.errors_were_reported = true;
    }
  }

  if (!reporter.get_error()) {
    for (const quick_lint_js::file_to_lint &file : o.files_to_lint) {
      auto config_result = config_loader.load_for_file(file);
      QLJS_ASSERT(config_result.ok());
      configuration *config =
          *config_result ? &(*config_result)->config : &default_config;
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
  void visit_end_of_module() {
    this->output_->append_copy(u8"end of module\n"sv);
    this->output_->flush();
  }

  void visit_enter_block_scope() {
    this->output_->append_copy(u8"entered block scope\n"sv);
    this->output_->flush();
  }

  void visit_enter_with_scope() {
    this->output_->append_copy(u8"entered with scope\n"sv);
    this->output_->flush();
  }

  void visit_enter_class_scope() {
    this->output_->append_copy(u8"entered class scope\n"sv);
    this->output_->flush();
  }

  void visit_enter_for_scope() {
    this->output_->append_copy(u8"entered for scope\n"sv);
    this->output_->flush();
  }

  void visit_enter_function_scope() {
    this->output_->append_copy(u8"entered function scope\n"sv);
    this->output_->flush();
  }

  void visit_enter_function_scope_body() {
    this->output_->append_copy(u8"entered function scope body\n"sv);
    this->output_->flush();
  }

  void visit_enter_named_function_scope(identifier) {
    this->output_->append_copy(u8"entered named function scope\n"sv);
    this->output_->flush();
  }

  void visit_exit_block_scope() {
    this->output_->append_copy(u8"exited block scope\n"sv);
    this->output_->flush();
  }

  void visit_exit_with_scope() {
    this->output_->append_copy(u8"exited with scope\n"sv);
    this->output_->flush();
  }

  void visit_exit_class_scope() {
    this->output_->append_copy(u8"exited class scope\n"sv);
    this->output_->flush();
  }

  void visit_exit_for_scope() {
    this->output_->append_copy(u8"exited for scope\n"sv);
    this->output_->flush();
  }

  void visit_exit_function_scope() {
    this->output_->append_copy(u8"exited function scope\n"sv);
    this->output_->flush();
  }

  void visit_keyword_variable_use(identifier name) {
    this->output_->append_copy(u8"keyword variable use: "sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_property_declaration(std::optional<identifier> name) {
    this->output_->append_copy(u8"property declaration"sv);
    if (name.has_value()) {
      this->output_->append_copy(u8": "sv);
      this->output_->append_copy(name->normalized_name());
    }
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_assignment(identifier name) {
    this->output_->append_copy(u8"variable assignment: "sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_declaration(identifier name, variable_kind) {
    this->output_->append_copy(u8"variable declaration: "sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_delete_use(
      identifier name, [[maybe_unused]] source_code_span delete_keyword) {
    this->output_->append_copy(u8"variable delete use: "sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_export_use(identifier name) {
    this->output_->append_copy(u8"variable export use: "sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_typeof_use(identifier name) {
    this->output_->append_copy(u8"variable typeof use: "sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  void visit_variable_use(identifier name) {
    this->output_->append_copy(u8"variable use: "sv);
    this->output_->append_copy(name.normalized_name());
    this->output_->append_copy(u8'\n');
    this->output_->flush();
  }

  file_output_stream *output_ = file_output_stream::get_stderr();
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

  void visit_keyword_variable_use(identifier name) {
    this->visitor_1_->visit_keyword_variable_use(name);
    this->visitor_2_->visit_keyword_variable_use(name);
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

  void visit_variable_delete_use(identifier name,
                                 source_code_span delete_keyword) {
    this->visitor_1_->visit_variable_delete_use(name, delete_keyword);
    this->visitor_2_->visit_variable_delete_use(name, delete_keyword);
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
  // TODO(strager): Use parse_and_visit_module_catching_fatal_parse_errors
  // instead of parse_and_visit_module to avoid crashing on
  // QLJS_PARSER_UNIMPLEMENTED.
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
      this->report_pending_watch_io_errors();
    }

    platform_file_ref get_readable_pipe() const { return this->input_pipe_; }

    void append(string8_view data) {
      this->endpoint_.append(data);
      // TODO(strager): Only call report_pending_watch_io_errors after
      // processing a full message.
      this->report_pending_watch_io_errors();
    }

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
    void on_fs_changed_kevent(const struct ::kevent &event) {
      this->fs_.handle_kqueue_event(event);
    }

    void on_fs_changed_kevents() { this->filesystem_changed(); }
#endif

#if QLJS_HAVE_INOTIFY
    std::optional<posix_fd_file_ref> get_inotify_fd() {
      return this->fs_.get_inotify_fd();
    }

    void on_fs_changed_event(const ::pollfd &event) {
      this->fs_.handle_poll_event(event);
      this->filesystem_changed();
    }
#endif

#if defined(_WIN32)
    void on_fs_changed_event(::OVERLAPPED *overlapped,
                             ::DWORD number_of_bytes_transferred,
                             ::DWORD error) {
      bool fs_changed = this->fs_.handle_event(
          overlapped, number_of_bytes_transferred, error);
      if (fs_changed) {
        this->filesystem_changed();
      }
    }
#endif

    void filesystem_changed() {
      this->endpoint_.handler().filesystem_changed();
      this->endpoint_.flush_pending_notifications();
    }

    void report_pending_watch_io_errors() {
      this->endpoint_.handler().add_watch_io_errors(
          this->fs_.take_watch_errors());
      this->endpoint_.flush_pending_notifications();
    }

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
    std::printf("%-*s%s\n", max_width, abbr, message);
  };

  std::printf(
      "Usage: quick-lint-js [OPTIONS]... FILE [FILE...]\n"
      "       quick-lint-js --lsp-server\n\n"
      "OPTIONS\n");
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
  print_option("", "gnu-like (default)");
  print_option("", "vim-qflist-json");
  print_option("", "emacs-lisp");
  print_option(
      "--diagnostic-hyperlinks=[WHEN]",
      "Control whether to hyperlink error codes or not. WHEN is one of:");
  print_option("", "auto (default)");
  print_option("", "always");
  print_option("", "never");
  print_option("-v, --version", "Print version information");
  print_option("--vim-file-bufnr=[NUMBER]",
               "Select a vim buffer for outputting feedback");
  print_option("-h, --help", "Print help message");

  bool mention_man_page = false;
#if defined(_POSIX2_VERSION)
  mention_man_page = true;
#endif
  if (mention_man_page) {
    std::printf(
        "\nFor more information, run 'man quick-lint-js' or visit\n"
        "https://quick-lint-js.com/cli/\n");
  } else {
    std::printf(
        "\nFor more information, visit https://quick-lint-js.com/cli/\n");
  }
}

void print_version_information() {
  std::puts("quick-lint-js version " QUICK_LINT_JS_VERSION_STRING);
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
