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
#include <quick-lint-js/debug-parse-visitor.h>
#include <quick-lint-js/diag-code-list.h>
#include <quick-lint-js/emacs-lisp-diag-reporter.h>
#include <quick-lint-js/emacs-location.h>
#include <quick-lint-js/event-loop.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/lsp-pipe-writer.h>
#include <quick-lint-js/lsp-server.h>
#include <quick-lint-js/multi-parse-visitor.h>
#include <quick-lint-js/options.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/pipe-writer.h>
#include <quick-lint-js/reported-diag-statistics.h>
#include <quick-lint-js/text-diag-reporter.h>
#include <quick-lint-js/trace-flusher.h>
#include <quick-lint-js/translation.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/utf-16.h>
#include <quick-lint-js/variant.h>
#include <quick-lint-js/vector-profiler.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/version.h>
#include <quick-lint-js/vim-qflist-json-diag-reporter.h>
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

class any_diag_reporter {
 public:
  static any_diag_reporter make(output_format format, option_when escape_errors,
                                compiled_diag_code_list *exit_fail_on) {
    switch (format) {
    case output_format::default_format:
    case output_format::gnu_like:
      return any_diag_reporter(reported_diag_statistics<text_diag_reporter>(
          text_diag_reporter(
              qljs_messages, file_output_stream::get_stderr(),
              /*escape_errors=*/get_escape_errors(escape_errors)),
          exit_fail_on));
    case output_format::vim_qflist_json:
      return any_diag_reporter(
          reported_diag_statistics<vim_qflist_json_diag_reporter>(
              vim_qflist_json_diag_reporter(qljs_messages,
                                            file_output_stream::get_stdout()),
              exit_fail_on));
    case output_format::emacs_lisp:
      return any_diag_reporter(
          reported_diag_statistics<emacs_lisp_diag_reporter>(
              emacs_lisp_diag_reporter(qljs_messages,
                                       file_output_stream::get_stdout()),
              exit_fail_on));
    }
    QLJS_UNREACHABLE();
  }

  void set_source(padded_string_view input, const file_to_lint &file) {
    visit(
        [&](auto &r) {
          using reporter_type = std::decay_t<decltype(r)>;
          if constexpr (std::is_base_of_v<reported_diag_statistics<
                                              vim_qflist_json_diag_reporter>,
                                          reporter_type>) {
            r.get_reporter()->set_source(input, file.path, file.vim_bufnr);
          } else if constexpr (std::is_base_of_v<reported_diag_statistics<
                                                     emacs_lisp_diag_reporter>,
                                                 reporter_type>) {
            r.get_reporter()->set_source(input);
          } else {
            r.get_reporter()->set_source(input, file.path);
          }
        },
        this->tape_);
  }

  diag_reporter *get() noexcept {
    return visit([](diag_reporter &r) { return &r; }, this->tape_);
  }

  bool get_error() noexcept {
    return visit([](auto &r) { return r.found_matching_diag(); }, this->tape_);
  }

  void finish() {
    visit(
        [&](auto &r) {
          using reporter_type = std::decay_t<decltype(r)>;
          if constexpr (std::is_base_of_v<reported_diag_statistics<
                                              vim_qflist_json_diag_reporter>,
                                          reporter_type>) {
            r.get_reporter()->finish();
          } else if constexpr (std::is_base_of_v<reported_diag_statistics<
                                                     emacs_lisp_diag_reporter>,
                                                 reporter_type>) {
            r.get_reporter()->finish();
          }
        },
        this->tape_);
  }

 private:
  using tape_variant =
      variant<reported_diag_statistics<text_diag_reporter>,
              reported_diag_statistics<vim_qflist_json_diag_reporter>,
              reported_diag_statistics<emacs_lisp_diag_reporter>>;

  explicit any_diag_reporter(tape_variant &&tape) : tape_(tape) {}

  tape_variant tape_;
};

void init();
[[noreturn]] void run(int argc, char **argv);
[[noreturn]] void run(options o);

void process_file(padded_string_view input, configuration &, diag_reporter *,
                  bool print_parser_visits);

void run_lsp_server();

void print_help_message();
void print_version_information();
}
}

#if defined(_WIN32)
int wmain(int argc, wchar_t **wargv) {
  quick_lint_js::init();
  quick_lint_js::mbargv m(argc, wargv);
  quick_lint_js::run(m.size(), m.data());
}
#else
int main(int argc, char **argv) {
  quick_lint_js::init();
  quick_lint_js::run(argc, argv);
}
#endif

namespace quick_lint_js {
namespace {
void init() {
#if QLJS_FEATURE_VECTOR_PROFILING
  vector_instrumentation::register_dump_on_exit_if_requested();
#endif
  initialize_translations_from_environment();
}

void run(int argc, char **argv) {
  options o = parse_options(argc, argv);
  run(o);
}

void run(options o) {
  if (o.snarky) {
    initialize_translations_from_locale("en_US@snarky");
  }
  if (o.help) {
    print_help_message();
    std::exit(EXIT_SUCCESS);
  }
  if (o.version) {
    print_version_information();
    std::exit(EXIT_SUCCESS);
  }
  if (o.dump_errors(*file_output_stream::get_stderr())) {
    std::exit(EXIT_FAILURE);
  }
  if (o.lsp_server) {
    run_lsp_server();
    std::exit(EXIT_SUCCESS);
  }
  if (o.files_to_lint.empty()) {
    std::fprintf(stderr, "error: expected file name\n");
    std::exit(EXIT_FAILURE);
  }

  any_diag_reporter reporter = any_diag_reporter::make(
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
    for (const file_to_lint &file : o.files_to_lint) {
      auto config_result = config_loader.load_for_file(file);
      QLJS_ASSERT(config_result.ok());
      configuration *config =
          *config_result ? &(*config_result)->config : &default_config;
      result<padded_string, read_file_io_error> source =
          file.is_stdin ? read_stdin() : read_file(file.path);
      if (!source.ok()) {
        source.error().print_and_exit();
      }
      reporter.set_source(&*source, file);
      process_file(&*source, *config, reporter.get(), o.print_parser_visits);
    }
  }
  reporter.finish();

  if (reporter.get_error() == true &&
      o.output_format != output_format::emacs_lisp) {
    std::exit(EXIT_FAILURE);
  }

  std::exit(EXIT_SUCCESS);
}

void process_file(padded_string_view input, configuration &config,
                  diag_reporter *diag_reporter, bool print_parser_visits) {
  parser_options p_options;
  p_options.jsx = true;
  parser p(input, diag_reporter, p_options);
  linter l(diag_reporter, &config.globals());

  auto run_parser = [&p](auto &visitor) -> void {
#if QLJS_HAVE_SETJMP
    p.parse_and_visit_module_catching_fatal_parse_errors(visitor);
#else
    p.parse_and_visit_module(visitor);
#endif
  };

  if (print_parser_visits) {
    debug_parse_visitor logger;
    multi_parse_visitor visitor(&logger, &l);
    run_parser(visitor);
  } else {
    run_parser(l);
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
          handler_(&this->fs_, &this->linter_, &this->tracer_),
          writer_(output_pipe),
          endpoint_(&this->handler_, &this->writer_, &this->tracer_) {
      this->report_pending_watch_io_errors();

      this->tracer_.register_current_thread();
      this->tracer_.flush_sync();
      this->tracer_.start_flushing_thread();
    }

    platform_file_ref get_readable_pipe() const { return this->input_pipe_; }

    void append(string8_view data) {
      this->endpoint_.append(data);
      this->handler_.flush_pending_notifications(this->writer_);
      // TODO(strager): Only call report_pending_watch_io_errors after
      // processing a full message.
      this->report_pending_watch_io_errors();

      this->tracer_.flush_async();
    }

#if QLJS_HAVE_KQUEUE || QLJS_HAVE_POLL
    std::optional<posix_fd_file_ref> get_pipe_write_fd() {
      return this->writer_.get_event_fd();
    }
#endif

#if QLJS_HAVE_KQUEUE
    void on_pipe_write_event(const struct ::kevent &event) {
      this->writer_.on_poll_event(event);
    }
#elif QLJS_HAVE_POLL
    void on_pipe_write_event(const ::pollfd &event) {
      this->writer_.on_poll_event(event);
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
      this->handler_.filesystem_changed();
      this->handler_.flush_pending_notifications(this->writer_);

      this->tracer_.flush_async();
    }

    void report_pending_watch_io_errors() {
      this->handler_.add_watch_io_errors(this->fs_.take_watch_errors());
      this->handler_.flush_pending_notifications(this->writer_);
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

    trace_flusher tracer_;
    platform_file_ref input_pipe_;
    lsp_javascript_linter linter_;
    linting_lsp_server_handler handler_;
    lsp_pipe_writer writer_;
    lsp_endpoint endpoint_;
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
  print_option("--snarky", "Add spice to your failures");

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
