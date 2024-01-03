// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdio>
#include <cstring>
#include <iomanip>
#include <memory>
#include <optional>
#include <quick-lint-js/cli/emacs-lisp-diag-reporter.h>
#include <quick-lint-js/cli/emacs-location.h>
#include <quick-lint-js/cli/options.h>
#include <quick-lint-js/cli/text-diag-reporter.h>
#include <quick-lint-js/cli/vim-qflist-json-diag-reporter.h>
#include <quick-lint-js/configuration/basic-configuration-filesystem.h>
#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/hash-set.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/vector-profiler.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/debug/find-debug-server.h>
#include <quick-lint-js/diag/diag-code-list.h>
#include <quick-lint-js/diag/reported-diag-statistics.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/i18n/translation.h>
#include <quick-lint-js/io/event-loop.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/pipe-writer.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/lsp/lsp-json-rpc-message-parser.h>
#include <quick-lint-js/lsp/lsp-pipe-writer.h>
#include <quick-lint-js/lsp/lsp-server.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/utf-16.h>
#include <quick-lint-js/version.h>
#include <string>
#include <tuple>

#if QLJS_FEATURE_DEBUG_SERVER
#include <quick-lint-js/debug/debug-server.h>
#endif

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
  const char *term = std::getenv("TERM");
  return isatty(STDERR_FILENO) && term && term != "dumb"sv;
#endif
}

bool get_escape_errors(Option_When escape_errors) {
  switch (escape_errors) {
  case Option_When::auto_:
    return stderr_supports_terminal_escapes();
  case Option_When::always:
    return true;
  case Option_When::never:
    return false;
  }
  QLJS_UNREACHABLE();
}

class Main_Diag_Reporter {
 private:
  class Emacs_Lisp_Main_Diag_Reporter;
  class GNU_Like_Main_Diag_Reporter;
  class Vim_QFList_JSON_Main_Diag_Reporter;

 public:
  static std::unique_ptr<Main_Diag_Reporter> make(
      Output_Format format, Option_When escape_errors,
      Compiled_Diag_Code_List *exit_fail_on);

  virtual ~Main_Diag_Reporter() = default;

  virtual void set_source(Padded_String_View input,
                          const File_To_Lint &file) = 0;
  virtual Diag_Reporter *get() = 0;
  virtual bool get_error() = 0;
  virtual void finish() = 0;
};

class Main_Diag_Reporter::Emacs_Lisp_Main_Diag_Reporter final
    : public Main_Diag_Reporter {
 public:
  explicit Emacs_Lisp_Main_Diag_Reporter(Compiled_Diag_Code_List *exit_fail_on)
      : reporter_(Emacs_Lisp_Diag_Reporter(qljs_messages,
                                           File_Output_Stream::get_stdout()),
                  exit_fail_on) {}

  void set_source(Padded_String_View input,
                  [[maybe_unused]] const File_To_Lint &file) override {
    this->reporter_.get_reporter()->set_source(input);
  }

  Diag_Reporter *get() override { return &this->reporter_; }

  bool get_error() override { return this->reporter_.found_matching_diag(); }

  void finish() override { this->reporter_.get_reporter()->finish(); }

 private:
  Reported_Diag_Statistics<Emacs_Lisp_Diag_Reporter> reporter_;
};

class Main_Diag_Reporter::GNU_Like_Main_Diag_Reporter final
    : public Main_Diag_Reporter {
 public:
  explicit GNU_Like_Main_Diag_Reporter(Option_When escape_errors,
                                       Compiled_Diag_Code_List *exit_fail_on)
      : reporter_(Text_Diag_Reporter(
                      qljs_messages, File_Output_Stream::get_stderr(),
                      /*escape_errors=*/get_escape_errors(escape_errors)),
                  exit_fail_on) {}

  void set_source(Padded_String_View input, const File_To_Lint &file) override {
    this->reporter_.get_reporter()->set_source(input, file.path);
  }

  Diag_Reporter *get() override { return &this->reporter_; }

  bool get_error() override { return this->reporter_.found_matching_diag(); }

  void finish() override {
    // Do nothing.
  }

 private:
  Reported_Diag_Statistics<Text_Diag_Reporter> reporter_;
};

class Main_Diag_Reporter::Vim_QFList_JSON_Main_Diag_Reporter final
    : public Main_Diag_Reporter {
 public:
  explicit Vim_QFList_JSON_Main_Diag_Reporter(
      Compiled_Diag_Code_List *exit_fail_on)
      : reporter_(Vim_QFList_JSON_Diag_Reporter(
                      qljs_messages, File_Output_Stream::get_stdout()),
                  exit_fail_on) {}

  void set_source(Padded_String_View input, const File_To_Lint &file) override {
    this->reporter_.get_reporter()->set_source(input, file.path,
                                               file.vim_bufnr);
  }

  Diag_Reporter *get() override { return &this->reporter_; }

  bool get_error() override { return this->reporter_.found_matching_diag(); }

  void finish() override { this->reporter_.get_reporter()->finish(); }

 private:
  Reported_Diag_Statistics<Vim_QFList_JSON_Diag_Reporter> reporter_;
};

std::unique_ptr<Main_Diag_Reporter> Main_Diag_Reporter::make(
    Output_Format format, Option_When escape_errors,
    Compiled_Diag_Code_List *exit_fail_on) {
  switch (format) {
  case Output_Format::default_format:
  case Output_Format::gnu_like:
    return std::unique_ptr<Main_Diag_Reporter>(
        new GNU_Like_Main_Diag_Reporter(escape_errors, exit_fail_on));
  case Output_Format::vim_qflist_json:
    return std::unique_ptr<Main_Diag_Reporter>(
        new Vim_QFList_JSON_Main_Diag_Reporter(exit_fail_on));
  case Output_Format::emacs_lisp:
    return std::unique_ptr<Main_Diag_Reporter>(
        new Emacs_Lisp_Main_Diag_Reporter(exit_fail_on));
  }
  QLJS_UNREACHABLE();
}

void init();
[[noreturn]] void run(int argc, char **argv);
[[noreturn]] void run(Options o);

Linter_Options get_linter_options_from_language(Resolved_Input_File_Language);

void list_debug_apps();
void run_lsp_server();

void print_help_message();
void print_version_information();
}
}

#if defined(_WIN32)
int wmain(int argc, wchar_t **wargv) {
  quick_lint_js::init();
  quick_lint_js::MBArgv m(argc, wargv);
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
  Vector_Instrumentation::register_dump_on_exit_if_requested();
#endif
  initialize_translations_from_environment();
}

void run(int argc, char **argv) {
  Monotonic_Allocator options_allocator("run options_allocator");
  Options o = parse_options(argc, argv, &options_allocator);
  run(std::move(o));
}

void run(Options o) {
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
  if (o.dump_errors(*File_Output_Stream::get_stderr())) {
    std::exit(EXIT_FAILURE);
  }
  if (o.list_debug_apps) {
    list_debug_apps();
    std::exit(EXIT_SUCCESS);
  }
  if (o.lsp_server) {
    run_lsp_server();
    std::exit(EXIT_SUCCESS);
  }
  if (o.files_to_lint.empty()) {
    std::fprintf(stderr, "error: expected file name\n");
    std::exit(EXIT_FAILURE);
  }

  std::unique_ptr<Main_Diag_Reporter> reporter = Main_Diag_Reporter::make(
      o.output_format, o.diagnostic_hyperlinks, &o.exit_fail_on);

  Configuration default_config;
  Configuration_Loader config_loader(
      Basic_Configuration_Filesystem::instance());
  Hash_Set<Loaded_Config_File *> loaded_config_files;
  for (const File_To_Lint &file : o.files_to_lint) {
    auto config_result = config_loader.load_for_file(file);
    if (!config_result.ok()) {
      std::fprintf(stderr, "error: %s\n",
                   config_result.error_to_string().c_str());
      std::exit(1);
    }
    Loaded_Config_File *config_file = *config_result;
    if (config_file && !loaded_config_files.contains(config_file)) {
      reporter->set_source(&config_file->file_content,
                           File_To_Lint{
                               .path = config_file->config_path->c_str(),
                               .config_file = nullptr,
                               .language = Raw_Input_File_Language::default_,
                               .is_stdin = false,
                               .vim_bufnr = std::nullopt,
                           });
      reporter->get()->report(config_file->errors);
      // To avoid repeating errors for a given config file, remember that we
      // already reported errors for this config file.
      loaded_config_files.insert(config_file);
    }
  }

  if (!reporter->get_error()) {
    for (const File_To_Lint &file : o.files_to_lint) {
      auto config_result = config_loader.load_for_file(file);
      QLJS_ASSERT(config_result.ok());
      Configuration *config =
          *config_result ? &(*config_result)->config : &default_config;
      Result<Padded_String, Read_File_IO_Error> source =
          file.is_stdin ? read_stdin() : read_file(file.path);
      if (!source.ok()) {
        source.error().print_and_exit();
      }
      Linter_Options lint_options =
          get_linter_options_from_language(get_language(file, o));
      lint_options.print_parser_visits = o.print_parser_visits;
      reporter->set_source(&*source, file);
      parse_and_lint(&*source, *reporter->get(), config->globals(),
                     lint_options);
    }
  }
  reporter->finish();

  if (reporter->get_error() == true &&
      o.output_format != Output_Format::emacs_lisp) {
    std::exit(EXIT_FAILURE);
  }

  std::exit(EXIT_SUCCESS);
}

Linter_Options get_linter_options_from_language(
    Resolved_Input_File_Language language) {
  Linter_Options o;
  switch (language) {
  case Resolved_Input_File_Language::javascript:
    o.jsx = false;
    o.typescript = false;
    break;
  case Resolved_Input_File_Language::javascript_jsx:
    o.jsx = true;
    o.typescript = false;
    break;
  case Resolved_Input_File_Language::typescript:
    o.jsx = false;
    o.typescript = true;
    break;
  case Resolved_Input_File_Language::typescript_definition:
    o.jsx = false;
    o.typescript = true;
    o.typescript_definition = true;
    break;
  case Resolved_Input_File_Language::typescript_jsx:
    o.jsx = true;
    o.typescript = true;
    break;
  }
  return o;
}

void list_debug_apps() {
  Monotonic_Allocator temporary_allocator("list_debug_apps");

  struct Table_Row {
    std::string process_id;
    std::string server_url;
  };
  std::vector<Table_Row> table;
  for (const Found_Debug_Server &s : find_debug_servers(&temporary_allocator)) {
    table.push_back(Table_Row{
        .process_id = std::to_string(s.process_id),
        .server_url =
            concat("http://localhost:"sv,
                   std::to_string(narrow_cast<int>(s.port_number)), "/"sv),
    });
  }

  const char *process_id_column_header = "PROCESS ID";
  int process_id_column_width =
      narrow_cast<int>(std::strlen(process_id_column_header));
  for (const Table_Row &row : table) {
    process_id_column_width = std::max(process_id_column_width,
                                       narrow_cast<int>(row.process_id.size()));
  }

  std::printf("%-*s  SERVER URL\n", process_id_column_width,
              process_id_column_header);
  for (const Table_Row &row : table) {
    std::printf("%-*s  %s\n", process_id_column_width, row.process_id.c_str(),
                row.server_url.c_str());
  }
}

void run_lsp_server() {
#if defined(_WIN32)
  Windows_Handle_File_Ref input_pipe(::GetStdHandle(STD_INPUT_HANDLE));
  Windows_Handle_File_Ref output_pipe(::GetStdHandle(STD_OUTPUT_HANDLE));
#else
  POSIX_FD_File_Ref input_pipe(STDIN_FILENO);
  POSIX_FD_File_Ref output_pipe(STDOUT_FILENO);
#endif
  Basic_Configuration_Filesystem fs;

  class LSP_Event_Loop :
#if QLJS_HAVE_KQUEUE
      public Event_Loop_Custom_Kqueue_Delegate,
#elif QLJS_HAVE_INOTIFY
      public Event_Loop_Custom_Poll_Delegate,
#elif defined(_WIN32)
      public Event_Loop_Custom_Windows_IO_Completion_Delegate,
#endif
#if QLJS_EVENT_LOOP2_PIPE_WRITE
      public Event_Loop_Pipe_Write_Delegate,
#endif
      public Event_Loop_Pipe_Read_Delegate {
   public:
    explicit LSP_Event_Loop(Platform_File_Ref input_pipe,
                            Platform_File_Ref output_pipe)
        :
#if QLJS_HAVE_KQUEUE
          kqueue_udata_(this->event_loop_.register_custom_kqueue(this)),
          fs_(this->event_loop_.kqueue_fd(), this->kqueue_udata_),
#elif QLJS_HAVE_INOTIFY
          fs_(),
#elif defined(_WIN32)
          windows_completion_key_(
              this->event_loop_.register_custom_windows_io_completion(this)),
          fs_(this->event_loop_.windows_io_completion_port(),
              this->windows_completion_key_),
#else
#error "Unsupported platform"
#endif
#if QLJS_FEATURE_DEBUG_SERVER
          debugger_(Debug_Server::create()),
#endif
          input_pipe_(input_pipe),
          handler_(&this->fs_, &this->linter_),
          writer_(output_pipe),
          endpoint_(&this->handler_) {
      Trace_Flusher *tracer = Trace_Flusher::instance();
      tracer->register_current_thread();
      tracer->flush_sync();
      tracer->start_flushing_thread();

#if QLJS_FEATURE_DEBUG_SERVER
      // NOTE(strager): The debug server will run on a random port. You can
      // query the port using the CLI: 'quick-lint-js --debug-apps'
      this->debugger_->start_server_thread();
      Result<void, Debug_Server_IO_Error> start_result =
          this->debugger_->wait_for_server_start();
      // TODO(strager): Print this over the LSP connection instead.
      // TODO(strager): Allow the LSP client to customize the debug server port.
      if (start_result.ok()) {
        std::fprintf(stderr, "note: quick-lint-js debug server started at %s\n",
                     this->debugger_->url().c_str());
      } else {
        std::fprintf(stderr,
                     "error: quick-lint-js debug server failed to start: %s\n",
                     start_result.error_to_string().c_str());
      }
#endif

      this->event_loop_.keep_alive();
#if !QLJS_HAVE_KQUEUE && QLJS_HAVE_INOTIFY
      if (std::optional<Platform_File_Ref> inotify_fd =
              this->fs_.get_inotify_fd()) {
        this->event_loop_.register_custom_poll(*inotify_fd, POLLIN, this);
      }
#endif
      this->event_loop_.register_pipe_read(this->input_pipe_, this);
#if QLJS_EVENT_LOOP2_PIPE_WRITE
      this->event_loop_.register_pipe_write(this->writer_.get_pipe_fd(), this);
      this->enable_or_disable_writer_events_as_needed();
#endif

      this->report_pending_watch_io_errors();
    }

    // Callable on any one thread once.
    void run() { this->event_loop_.run(); }

    void on_pipe_read_data(Event_Loop_Base *, Platform_File_Ref,
                           String8_View data) override {
      this->endpoint_.append(data);
      this->endpoint_.flush_error_responses(this->writer_);
      this->handler_.flush_pending_notifications(this->writer_);
      // TODO(strager): Only call report_pending_watch_io_errors after
      // processing a full message.
      this->report_pending_watch_io_errors();

#if QLJS_EVENT_LOOP2_PIPE_WRITE
      this->enable_or_disable_writer_events_as_needed();
#endif

      Trace_Flusher::instance()->flush_async();
    }

    void on_pipe_read_end(Event_Loop_Base *event_loop,
                          Platform_File_Ref) override {
      event_loop->un_keep_alive();
    }

    QLJS_WARNING_PUSH
    QLJS_WARNING_IGNORE_CLANG("-Wmissing-noreturn")
    QLJS_WARNING_IGNORE_GCC("-Wsuggest-attribute=noreturn")

    void on_pipe_read_error(Event_Loop_Base *, Platform_File_Ref,
                            Platform_File_IO_Error) override {
      QLJS_UNIMPLEMENTED();
    }

    QLJS_WARNING_POP

#if QLJS_EVENT_LOOP2_PIPE_WRITE
    void on_pipe_write_ready(Event_Loop_Base *, Platform_File_Ref) override {
      this->writer_.on_pipe_write_ready();
      this->enable_or_disable_writer_events_as_needed();
    }
    void on_pipe_write_end(Event_Loop_Base *, Platform_File_Ref) override {
      this->writer_.on_pipe_write_end();
    }

    // HACK[Non_Blocking_Pipe_Writer-enable-disable]: Synchronize
    // Non_Blocking_Pipe_Writer with Event_Loop. Ideally this would be
    // coordinated automatically between the two classes.
    //
    // If this->writer_ has no pending data to write to the pipe (e.g. if
    // on_pipe_read_data flushed the output buffers without blocking), and if
    // the event loop has the write events enabled, then we'll *always* receive
    // events (which is wasteful (100% CPU)).
    //
    // If this->writer_ has pending data to write to the pipe (e.g. if
    // on_pipe_read_data wrote a big message or if the client is slow at reading
    // notifications), then if the event loop has write events disabled, then
    // the client won't receive any data.
    void enable_or_disable_writer_events_as_needed() {
      bool should_be_enabled = this->writer_.has_pending_data();
      if (should_be_enabled) {
        this->event_loop_.enable_pipe_write(this->writer_.get_pipe_fd());
      } else {
        this->event_loop_.disable_pipe_write(this->writer_.get_pipe_fd());
      }
    }
#endif

#if QLJS_HAVE_KQUEUE
    void on_custom_kqueue_events(Event_Loop_Base *,
                                 Span<struct ::kevent> events) override {
      for (const struct ::kevent &event : events) {
        this->fs_.handle_kqueue_event(event);
      }
      this->filesystem_changed();
    }
#endif

#if QLJS_HAVE_INOTIFY
    void on_custom_poll_event(Event_Loop_Base *, POSIX_FD_File_Ref,
                              short revents) override {
      this->fs_.handle_poll_event(revents);

      this->filesystem_changed();
    }
#endif

#if defined(_WIN32)
    void on_custom_windows_io_completion(Event_Loop_Base *, ::ULONG_PTR,
                                         ::DWORD number_of_bytes_transferred,
                                         ::OVERLAPPED *overlapped) override {
      this->on_filesystem_io_completion(overlapped, number_of_bytes_transferred,
                                        /*error=*/0);
    }

    void on_custom_windows_io_completion_error(
        Event_Loop_Base *, ::ULONG_PTR, ::DWORD error,
        ::DWORD number_of_bytes_transferred,
        ::OVERLAPPED *overlapped) override {
      this->on_filesystem_io_completion(overlapped, number_of_bytes_transferred,
                                        error);
    }

    void on_filesystem_io_completion(::OVERLAPPED *overlapped,
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
#if QLJS_EVENT_LOOP2_PIPE_WRITE
      this->enable_or_disable_writer_events_as_needed();
#endif

      Trace_Flusher::instance()->flush_async();
    }

    void report_pending_watch_io_errors() {
      this->handler_.add_watch_io_errors(
          Span<const Watch_IO_Error>(this->fs_.take_watch_errors()));
      this->handler_.flush_pending_notifications(this->writer_);
#if QLJS_EVENT_LOOP2_PIPE_WRITE
      this->enable_or_disable_writer_events_as_needed();
#endif
    }

   private:
    Event_Loop event_loop_;

#if QLJS_HAVE_KQUEUE
    Event_Loop::Kqueue_Udata kqueue_udata_;
    Change_Detecting_Filesystem_Kqueue fs_;
#elif QLJS_HAVE_INOTIFY
    Change_Detecting_Filesystem_Inotify fs_;
#elif defined(_WIN32)
    Event_Loop::Windows_Completion_Key windows_completion_key_;
    Change_Detecting_Filesystem_Win32 fs_;
#else
#error "Unsupported platform"
#endif

#if QLJS_FEATURE_DEBUG_SERVER
    std::shared_ptr<Debug_Server> debugger_;
#endif

    Platform_File_Ref input_pipe_;
    LSP_JavaScript_Linter linter_;
    Linting_LSP_Server_Handler handler_;
    LSP_Pipe_Writer writer_;
    LSP_JSON_RPC_Message_Parser endpoint_;
  };

#if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING
  input_pipe.set_pipe_non_blocking();
#endif
#if !QLJS_PIPE_WRITER_SEPARATE_THREAD
  output_pipe.set_pipe_non_blocking();
#endif
  LSP_Event_Loop server(input_pipe, output_pipe);
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
  print_option("--language=[LANG]", "Interpret input files as if they were");
  print_option("", "written in LANG. LANG is one of:");
  print_option("", "default (default): use file extension");
  print_option("", "javascript");
  print_option("", "javascript-jsx");
  print_option("", "experimental-typescript");
  print_option("", "experimental-typescript-jsx");
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
