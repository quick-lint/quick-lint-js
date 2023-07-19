// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <optional>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/port/child-process.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/port/windows-error.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/utf-16.h>
#include <string>
#include <string_view>
#include <vector>

#if QLJS_HAVE_CRT_EXTERNS_H
#include <crt_externs.h>
#endif

#if QLJS_HAVE_POSIX_SPAWN
#include <spawn.h>
#endif

#if QLJS_HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#endif

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
#if QLJS_HAVE_UNISTD_H
char** get_posix_environment();
#endif

#if QLJS_HAVE_POSIX_SPAWN || QLJS_HAVE_WINDOWS_H
void handle_process_io(const Run_Program_Options& options,
                       Pipe_FDs& program_input, Pipe_FDs& program_output,
                       Run_Program_Result& out);
#endif
}

Run_Program_Result run_program(std::initializer_list<std::string> command) {
  return run_program(Span<const std::string>(command.begin(), command.end()));
}

Run_Program_Result run_program(std::initializer_list<const char*> command) {
  return run_program(Span<const char* const>(command.begin(), command.end()));
}

Run_Program_Result run_program(std::initializer_list<std::string> command,
                               Run_Program_Options options) {
  return run_program(Span<const std::string>(command.begin(), command.end()),
                     options);
}

Run_Program_Result run_program(std::initializer_list<const char*> command,
                               Run_Program_Options options) {
  return run_program(Span<const char* const>(command.begin(), command.end()),
                     options);
}

Run_Program_Result run_program(Span<const std::string> command) {
  return run_program(command, Run_Program_Options());
}

Run_Program_Result run_program(Span<const char* const> command) {
  return run_program(command, Run_Program_Options());
}

Run_Program_Result run_program(Span<const std::string> command,
                               Run_Program_Options options) {
  std::vector<const char*> command_raw;
  for (const std::string& arg : command) {
    command_raw.push_back(arg.c_str());
  }
  return run_program(Span<const char* const>(command_raw), options);
}

#if QLJS_HAVE_POSIX_SPAWN
Run_Program_Result run_program(Span<const char* const> command,
                               Run_Program_Options options) {
  QLJS_ASSERT(command.size() >= 1 && "expected at least path to .exe");

  Pipe_FDs program_input = make_pipe();
  Pipe_FDs program_output = make_pipe();
  ::posix_spawn_file_actions_t file_actions;
  ::posix_spawn_file_actions_init(&file_actions);
  ::posix_spawn_file_actions_adddup2(&file_actions, program_input.reader.get(),
                                     STDIN_FILENO);
  ::posix_spawn_file_actions_adddup2(&file_actions, program_output.writer.get(),
                                     STDOUT_FILENO);
  ::posix_spawn_file_actions_adddup2(&file_actions, program_output.writer.get(),
                                     STDERR_FILENO);

  Result<std::string, Platform_File_IO_Error> old_cwd;
  if (options.current_directory != nullptr) {
    old_cwd = get_current_working_directory();
    if (!old_cwd.ok()) {
      std::fprintf(stderr, "error: failed to save current directory: %s\n",
                   old_cwd.error_to_string().c_str());
      std::exit(1);
    }

    set_current_working_directory_or_exit(options.current_directory);
  }

  std::vector<char*> argv;
  for (const char* arg : command) {
    argv.push_back(const_cast<char*>(arg));
  }
  argv.push_back(nullptr);
  const char* exe_file = command[0];

  ::pid_t pid;
  int rc = ::posix_spawnp(/*pid=*/&pid, /*file=*/exe_file,
                          /*file_actions=*/&file_actions,
                          /*attrp=*/nullptr,
                          /*argv=*/argv.data(),
                          /*envp=*/get_posix_environment());
  if (rc != 0) {
    std::fprintf(stderr, "error: failed to spawn %s: %s\n", exe_file,
                 std::strerror(errno));
    std::exit(1);
  }

  if (options.current_directory != nullptr) {
    set_current_working_directory_or_exit(old_cwd->c_str());
  }

  ::posix_spawn_file_actions_destroy(&file_actions);

  Run_Program_Result result;
  handle_process_io(options, program_input, program_output, result);
  result.exit_status = wait_for_process_exit(pid);

  return result;
}
#endif

#if QLJS_HAVE_SYS_WAIT_H
std::uint32_t wait_for_process_exit(::pid_t pid) {
retry:
  int status;
  ::pid_t rc = ::waitpid(pid, &status, /*options=*/0);
  if (rc == -1) {
    if (errno == EINTR) {
      goto retry;
    }
    std::fprintf(stderr, "error: failed to wait for process %lld: %s\n",
                 narrow_cast<long long>(pid), std::strerror(errno));
    std::exit(1);
  }

  if (WIFEXITED(status)) {
    return static_cast<std::uint32_t>(WEXITSTATUS(status));
  }
  if (WIFSIGNALED(status)) {
    QLJS_ASSERT(WTERMSIG(status) > 0);
    return std::uint32_t(0) - WTERMSIG(status);  // Arbitrary.
  }
  QLJS_ASSERT(false);
  return std::uint32_t(-1);  // Arbitrary.
}
#endif

#if QLJS_HAVE_WINDOWS_H
namespace {
class Windows_Thread_Attribute_List {
 public:
  // thread_attribute_list must have been allocated with std::malloc (or
  // something compatible).
  explicit Windows_Thread_Attribute_List(
      ::LPPROC_THREAD_ATTRIBUTE_LIST thread_attribute_list)
      : thread_attribute_list_(thread_attribute_list) {}

  Windows_Thread_Attribute_List(const Windows_Thread_Attribute_List&) = delete;
  Windows_Thread_Attribute_List& operator=(
      const Windows_Thread_Attribute_List&) = delete;

  ~Windows_Thread_Attribute_List() {
    ::DeleteProcThreadAttributeList(this->thread_attribute_list_);
    std::free(this->thread_attribute_list_);
  }

  ::LPPROC_THREAD_ATTRIBUTE_LIST get() const {
    return this->thread_attribute_list_;
  }

  static Windows_Thread_Attribute_List make_for_inheriting_handles(
      Span<const ::HANDLE> handles) {
    ::SIZE_T thread_attributes_size = 0;
    (void)::InitializeProcThreadAttributeList(
        nullptr, /*dwAttributeCount=*/1,
        /*dwFlags=*/0,
        /*lpSize=*/&thread_attributes_size);
    ::LPPROC_THREAD_ATTRIBUTE_LIST thread_attribute_list =
        reinterpret_cast<::LPPROC_THREAD_ATTRIBUTE_LIST>(
            std::malloc(thread_attributes_size));
    if (!::InitializeProcThreadAttributeList(
            thread_attribute_list, /*dwAttributeCount=*/1,
            /*dwFlags=*/0,
            /*lpSize=*/&thread_attributes_size)) {
      QLJS_UNIMPLEMENTED();
    }
    if (!::UpdateProcThreadAttribute(
            thread_attribute_list,
            /*dwFlags=*/0,
            /*Attribute=*/PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
            /*lpValue=*/const_cast<::HANDLE*>(handles.data()),
            /*cbSize=*/handles.byte_size(),
            /*lpPreviousValue=*/nullptr,
            /*lpReturnSize=*/nullptr)) {
      QLJS_UNIMPLEMENTED();
    }
    return Windows_Thread_Attribute_List(thread_attribute_list);
  }

 private:
  ::LPPROC_THREAD_ATTRIBUTE_LIST thread_attribute_list_;
};

struct Windows_Command_Line_Builder {
  std::wstring command_line;

  wchar_t* data() { return this->command_line.data(); }

  void add_argument(const char* argument) {
    std::optional<std::wstring> wide_argument = mbstring_to_wstring(argument);
    if (!wide_argument.has_value()) {
      QLJS_UNIMPLEMENTED();
    }
    this->add_argument(*wide_argument);
  }

  void add_argument(std::wstring_view argument) {
    if (argument.find_first_of(L'"') != std::wstring_view::npos) {
      // TODO(strager): Escape quotes.
      QLJS_UNIMPLEMENTED();
    }
    if (!this->command_line.empty()) {
      this->command_line += L' ';
    }
    this->command_line += L'"';
    this->command_line += argument;
    this->command_line += L'"';
  }
};
}

Run_Program_Result run_program(Span<const char* const> command,
                               Run_Program_Options options) {
  QLJS_ASSERT(command.size() >= 1 && "expected at least path to .exe");

  std::optional<std::wstring> application_name =
      mbstring_to_wstring(command[0]);
  if (!application_name.has_value()) {
    QLJS_UNIMPLEMENTED();
  }

  Windows_Command_Line_Builder command_line;
  command_line.add_argument(*application_name);
  for (Span_Size i = 1; i < command.size(); ++i) {
    command_line.add_argument(command[i]);
  }

  std::optional<std::wstring> current_directory = std::nullopt;
  if (options.current_directory != nullptr) {
    current_directory = mbstring_to_wstring(options.current_directory);
    if (!current_directory.has_value()) {
      QLJS_UNIMPLEMENTED();
    }
  }

  Pipe_FDs program_input = make_pipe();
  Pipe_FDs program_output = make_pipe();

  ::HANDLE handles_to_inherit[] = {
      program_input.reader.get(),
      program_output.writer.get(),
  };
  Windows_Thread_Attribute_List thread_attribute_list =
      Windows_Thread_Attribute_List::make_for_inheriting_handles(
          Span<const ::HANDLE>(handles_to_inherit));

  ::STARTUPINFOEXW startup_info = {};
  startup_info.StartupInfo.cb = sizeof(startup_info);
  startup_info.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  startup_info.StartupInfo.hStdInput = program_input.reader.get();
  startup_info.StartupInfo.hStdOutput = program_output.writer.get();
  startup_info.StartupInfo.hStdError = program_output.writer.get();
  startup_info.lpAttributeList = thread_attribute_list.get();

  ::PROCESS_INFORMATION process_info;
  if (!::CreateProcessW(
          /*lpApplicationName=*/application_name->c_str(),
          /*lpCommandLine=*/command_line.data(),
          /*lpProcessAttributes=*/nullptr,
          /*lpThreadAttributes=*/nullptr,
          /*bInheritHandles=*/true,
          /*dwCreationFlags=*/CREATE_UNICODE_ENVIRONMENT |
              EXTENDED_STARTUPINFO_PRESENT,
          /*lpEnvironment=*/nullptr,
          /*lpCurrentDirectory=*/current_directory.has_value()
              ? current_directory->c_str()
              : nullptr,
          /*lpStartupInfo=*/reinterpret_cast<::STARTUPINFOW*>(&startup_info),
          /*lpProcessInformation=*/&process_info)) {
    std::fprintf(stderr, "error: failed to create process: %s\n",
                 windows_last_error_message().c_str());
    QLJS_UNIMPLEMENTED();
  }
  ::CloseHandle(process_info.hThread);
  Windows_Handle_File process_handle(process_info.hProcess);

  Run_Program_Result result;
  handle_process_io(options, program_input, program_output, result);

  ::DWORD rc = ::WaitForSingleObject(process_handle.get(), INFINITE);
  if (rc != WAIT_OBJECT_0) {
    std::fprintf(stderr, "fatal: WaitForSingleObject failed: %s\n",
                 windows_last_error_message().c_str());
    std::exit(1);
  }
  ::DWORD exit_code;
  if (!::GetExitCodeProcess(process_handle.get(), &exit_code)) {
    std::fprintf(stderr, "fatal: GetExitCodeProcess failed: %s\n",
                 windows_last_error_message().c_str());
    std::exit(1);
  }
  result.exit_status = exit_code;

  return result;
}
#endif

namespace {
#if QLJS_HAVE_UNISTD_H
#if !QLJS_HAVE_NS_GET_ENVIRON
extern "C" char** environ;
#endif

char** get_posix_environment() {
#if QLJS_HAVE_NS_GET_ENVIRON
  return *::_NSGetEnviron();
#else
  return environ;
#endif
}
#endif

#if QLJS_HAVE_POSIX_SPAWN || QLJS_HAVE_WINDOWS_H
void handle_process_io(const Run_Program_Options& options,
                       Pipe_FDs& program_input, Pipe_FDs& program_output,
                       Run_Program_Result& out) {
  program_input.reader.close();
  program_output.writer.close();

  Thread input_writer_thread;
  if (options.input.empty()) {
    program_input.writer.close();
  } else {
    input_writer_thread = Thread([&]() -> void {
      program_input.writer.write_full(options.input.data(),
                                      options.input.size());
      program_input.writer.close();
    });
  }

  auto output = read_file(program_output.reader.ref());
  if (!output.ok()) {
    std::fprintf(stderr, "error: %s\n", output.error_to_string().c_str());
    std::exit(1);
  }
  out.output = std::move(*output);

  if (!options.input.empty()) {
    input_writer_thread.join();
  }
}
#endif
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
