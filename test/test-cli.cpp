// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/child-process.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/pty.h>

#if QLJS_HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

using ::testing::HasSubstr;

namespace quick_lint_js {
namespace {
const char* get_quick_lint_js_executable_path() {
  const char* variable_name = "QUICK_LINT_JS_EXE";
  const char* path = std::getenv(variable_name);
  if (path == nullptr || std::strlen(path) == 0) {
    std::fprintf(stderr,
                 "fatal: Could not determine path to quick-lint-js. Set the %s "
                 "environment variable to its path.",
                 variable_name);
  }
  return path;
}

int count_occurences(String8_View haystack, String8_View needle) {
  int count = 0;
  for (;;) {
    std::size_t index = haystack.find(needle);
    if (index == String8_View::npos) {
      return count;
    }
    count += 1;
    haystack = haystack.substr(index + needle.size());
  }
}

class Test_CLI : public ::testing::Test, protected Filesystem_Test {};

TEST_F(Test_CLI, no_files_fails) {
  std::string temp_file_path = this->make_temporary_directory() + "/temp.js";
  Run_Program_Result r = run_program({get_quick_lint_js_executable_path()});
  EXPECT_EQ(r.exit_status, 1);
}

TEST_F(Test_CLI, good_file_lints_ok) {
  std::string test_file = this->make_temporary_directory() + "/test.js";
  write_file_or_exit(test_file, u8"console.log('hello world');\n"_sv);

  Run_Program_Result r =
      run_program({get_quick_lint_js_executable_path(), test_file});
  EXPECT_EQ(r.exit_status, 0);
}

TEST_F(Test_CLI, file_with_syntax_errors_fails) {
  std::string test_file = this->make_temporary_directory() + "/test.js";
  write_file_or_exit(
      test_file, u8"var parenthesesMissing;\nif parenthesesMissing { }\n"_sv);

  Run_Program_Result r =
      run_program({get_quick_lint_js_executable_path(), test_file});
  EXPECT_EQ(r.exit_status, 1);
  EXPECT_THAT(to_string(r.output.string_view()), HasSubstr("E0017"));
}

TEST_F(Test_CLI, file_with_escaped_syntax_errors_fails) {
  std::string test_file = this->make_temporary_directory() + "/test.js";
  write_file_or_exit(
      test_file, u8"var parenthesesMissing;\nif parenthesesMissing { }\n"_sv);

  Run_Program_Result r =
      run_program({get_quick_lint_js_executable_path(),
                   "--diagnostic-hyperlinks=always", test_file});
  EXPECT_EQ(r.exit_status, 1);
  EXPECT_THAT(to_string(r.output.string_view()),
              HasSubstr("[\u001b]8;;https://quick-lint-js.com/errors/E0017/"
                        "\u001b\\E0017\u001b]8;;\u001b\\]"));
}

TEST_F(Test_CLI,
       file_with_syntax_errors_with_non_matching_exit_fail_on_does_not_fail) {
  std::string test_file = this->make_temporary_directory() + "/test.js";
  write_file_or_exit(
      test_file, u8"var parenthesesMissing;\nif parenthesesMissing { }\n"_sv);

  Run_Program_Result r = run_program(
      {get_quick_lint_js_executable_path(), "--exit-fail-on=E0057", test_file});
  EXPECT_EQ(r.exit_status, 0);
  EXPECT_THAT(to_string(r.output.string_view()), HasSubstr("E0017"))
      << "Error should be printed";
}

TEST_F(Test_CLI, single_config_file) {
  std::string test_directory = this->make_temporary_directory();

  std::string test_file = test_directory + "/test.js";
  write_file_or_exit(test_file, u8"console.log(myGlobalVariable);"_sv);

  std::string config_file = test_directory + "/config.json";
  write_file_or_exit(config_file,
                     u8R"({"globals":{"myGlobalVariable": true}})"_sv);

  Run_Program_Result r = run_program({get_quick_lint_js_executable_path(),
                                      "--config-file", config_file, test_file});
  EXPECT_EQ(r.output, u8""_sv);
  EXPECT_EQ(r.exit_status, 0);
}

TEST_F(Test_CLI, config_file_for_stdin) {
  std::string config_file = this->make_temporary_directory() + "/config.json";
  write_file_or_exit(config_file,
                     u8R"({"globals":{"myGlobalVariable": true}})"_sv);

  Run_Program_Result r =
      run_program({get_quick_lint_js_executable_path(), "--config-file",
                   config_file, "--stdin"},
                  Run_Program_Options{
                      .input = u8"console.log(myGlobalVariable);"_sv,
                  });
  EXPECT_EQ(r.output, u8""_sv);
  EXPECT_EQ(r.exit_status, 0);
}

TEST_F(Test_CLI, missing_explicit_config_file) {
  std::string test_directory = this->make_temporary_directory();

  std::string test_file = test_directory + "/test.js";
  write_file_or_exit(test_file, u8"console.log(myGlobalVariable);"_sv);

  std::string config_file = test_directory + "/config.json";

  Run_Program_Result r = run_program({get_quick_lint_js_executable_path(),
                                      "--config-file", config_file, test_file});
  EXPECT_THAT(to_string(r.output.string_view()), HasSubstr("config.json"));
  EXPECT_THAT(to_string(r.output.string_view()), HasSubstr("error:"));
  EXPECT_EQ(r.exit_status, 1);
}

TEST_F(Test_CLI, automatically_find_config_file) {
  std::string test_directory = this->make_temporary_directory();

  std::string test_file = test_directory + "/test.js";
  write_file_or_exit(test_file, u8"console.log(myGlobalVariable);"_sv);

  std::string config_file = test_directory + "/quick-lint-js.config";
  write_file_or_exit(config_file,
                     u8R"({"globals":{"myGlobalVariable": true}})"_sv);

  Run_Program_Result r =
      run_program({get_quick_lint_js_executable_path(), test_file});
  EXPECT_EQ(r.output, u8""_sv);
  EXPECT_EQ(r.exit_status, 0);
}

TEST_F(Test_CLI, stdin_does_not_automatically_find_config_file) {
  std::string test_directory = this->make_temporary_directory();

  std::string config_file = test_directory + "/quick-lint-js.config";
  write_file_or_exit(config_file, u8R"({"global-groups":["emscripten"]})"_sv);

  Run_Program_Result r =
      run_program({get_quick_lint_js_executable_path(), "--stdin"},
                  Run_Program_Options{
                      .current_directory = test_directory.c_str(),
                      .input = u8"document"_sv,
                  });
  EXPECT_EQ(r.output, u8""_sv);
  EXPECT_EQ(r.exit_status, 0);
}

TEST_F(Test_CLI, automatically_find_config_file_given_path_for_config_search) {
  std::string test_directory = this->make_temporary_directory();

  std::string test_file = test_directory + "/test.js";
  write_file_or_exit(test_file, u8"console.log(myGlobalVariable);"_sv);

  std::string config_file_dir = test_directory + "/subdir";
  create_directory_or_exit(config_file_dir);
  std::string config_file = config_file_dir + "/quick-lint-js.config";
  write_file_or_exit(config_file,
                     u8R"({"globals":{"myGlobalVariable": true}})"_sv);

  Run_Program_Result r = run_program({
      get_quick_lint_js_executable_path(),
      "--path-for-config-search",
      config_file_dir + "/app.js",
      test_file,
  });
  EXPECT_EQ(r.output, u8""_sv);
  EXPECT_EQ(r.exit_status, 0);
}

TEST_F(Test_CLI, path_for_config_search_affects_stdin_file) {
  std::string test_directory = this->make_temporary_directory();
  std::string config_file = test_directory + "/quick-lint-js.config";
  write_file_or_exit(config_file,
                     u8R"({"globals":{"myGlobalVariable": true}})"_sv);

  Run_Program_Result r = run_program(
      {
          get_quick_lint_js_executable_path(),
          "--path-for-config-search",
          test_directory + "/app.js",
          "--stdin",
      },
      Run_Program_Options{
          .input = u8"console.log(myGlobalVariable);"_sv,
      });
  EXPECT_EQ(r.output, u8""_sv);
  EXPECT_EQ(r.exit_status, 0);
}

TEST_F(Test_CLI, path_for_stdin_affects_stdin_file_config_search) {
  std::string test_directory = this->make_temporary_directory();
  std::string config_file = test_directory + "/quick-lint-js.config";
  write_file_or_exit(config_file,
                     u8R"({"globals":{"myGlobalVariable": true}})"_sv);

  Run_Program_Result r = run_program(
      {
          get_quick_lint_js_executable_path(),
          "--stdin-path",
          test_directory + "/app.js",
          "--stdin",
      },
      Run_Program_Options{
          .input = u8"console.log(myGlobalVariable);"_sv,
      });
  EXPECT_EQ(r.output, u8""_sv);
  EXPECT_EQ(r.exit_status, 0);
}

TEST_F(Test_CLI, config_file_parse_error_prevents_lint) {
  std::string test_directory = this->make_temporary_directory();

  std::string test_file = test_directory + "/test.js";
  write_file_or_exit(test_file, u8"console.log(myGlobalVariable);"_sv);

  std::string config_file = test_directory + "/quick-lint-js.config";
  write_file_or_exit(config_file, u8"INVALID JSON"_sv);

  Run_Program_Result r =
      run_program({get_quick_lint_js_executable_path(), test_file});
  EXPECT_EQ(r.exit_status, 1);

  EXPECT_THAT(to_string(r.output.string_view()),
              ::testing::Not(HasSubstr("myGlobalVariable")))
      << "test.js shouldn't be linted";
  EXPECT_THAT(to_string(r.output.string_view()),
              ::testing::Not(HasSubstr("E0057")))
      << "test.js shouldn't be linted";

  EXPECT_THAT(to_string(r.output.string_view()),
              HasSubstr("quick-lint-js.config"))
      << "quick-lint-js.config should have errors";
  EXPECT_THAT(to_string(r.output.string_view()), HasSubstr("E0164"))
      << "quick-lint-js.config should have errors";
}

TEST_F(Test_CLI, config_error_for_multiple_js_files_is_printed_only_once) {
  std::string test_directory = this->make_temporary_directory();

  std::string test_file_1 = test_directory + "/test1.js";
  write_file_or_exit(test_file_1, u8""_sv);

  std::string test_file_2 = test_directory + "/test2.js";
  write_file_or_exit(test_file_2, u8""_sv);

  std::string config_file = test_directory + "/quick-lint-js.config";
  write_file_or_exit(config_file, u8"INVALID JSON"_sv);

  Run_Program_Result r = run_program(
      {get_quick_lint_js_executable_path(), test_file_1, test_file_2});
  EXPECT_EQ(r.exit_status, 1);
  EXPECT_EQ(count_occurences(r.output.string_view(), u8"E0164"_sv), 1)
      << r.output;
}

TEST_F(Test_CLI, errors_for_all_config_files_are_printed) {
  std::string test_directory = this->make_temporary_directory();

  std::string test_dir_1 = test_directory + "/dir1";
  create_directory_or_exit(test_dir_1);
  std::string test_file_1 = test_dir_1 + "/test.js";
  write_file_or_exit(test_file_1, u8""_sv);
  std::string config_file_1 = test_dir_1 + "/quick-lint-js.config";
  write_file_or_exit(config_file_1, u8"INVALID JSON"_sv);

  std::string test_dir_2 = test_directory + "/dir2";
  create_directory_or_exit(test_dir_2);
  std::string test_file_2 = test_dir_2 + "/test.js";
  write_file_or_exit(test_file_2, u8""_sv);
  std::string config_file_2 = test_dir_2 + "/quick-lint-js.config";
  write_file_or_exit(config_file_2, u8"INVALID JSON"_sv);

  Run_Program_Result r = run_program(
      {get_quick_lint_js_executable_path(), test_file_1, test_file_2});
  EXPECT_EQ(r.exit_status, 1);
  EXPECT_THAT(to_string(r.output.string_view()), HasSubstr("dir1"));
  EXPECT_THAT(to_string(r.output.string_view()), HasSubstr("dir2"));
  EXPECT_EQ(count_occurences(r.output.string_view(), u8"E0164"_sv), 2)
      << r.output;
}

TEST_F(Test_CLI, path_for_stdin_affects_default_language) {
  {
    Run_Program_Result r = run_program({get_quick_lint_js_executable_path(),
                                        "--stdin", "--stdin-path=hello.js"},
                                       Run_Program_Options{
                                           .input = u8"interface I {}"_sv,
                                       });
    EXPECT_EQ(r.exit_status, 1);
    EXPECT_THAT(to_string(r.output.string_view()), HasSubstr("E0213"))
        << "expected \"TypeScript's 'interface' feature is not allowed in "
           "JavaScript code\"\n"
        << r.output;
  }

  {
    Run_Program_Result r = run_program({get_quick_lint_js_executable_path(),
                                        "--stdin", "--stdin-path=hello.ts"},
                                       Run_Program_Options{
                                           .input = u8"interface I {}"_sv,
                                       });
    EXPECT_EQ(r.exit_status, 0);
    EXPECT_THAT(to_string(r.output.string_view()), Not(HasSubstr("E0213")))
        << "expected no diagnostics because file should be interpreted as "
           "TypeScript\n"
        << r.output;
  }
}

TEST_F(Test_CLI, language_overrides_path_for_stdin) {
  Run_Program_Result r =
      run_program({get_quick_lint_js_executable_path(), "--language=typescript",
                   "--stdin", "--stdin-path=hello.js"},
                  Run_Program_Options{
                      .input = u8"interface I {}"_sv,
                  });
  EXPECT_EQ(r.exit_status, 0);
  EXPECT_THAT(to_string(r.output.string_view()), Not(HasSubstr("E0213")))
      << "expected no diagnostics because file should be interpreted as "
         "TypeScript\n"
      << r.output;
}

TEST_F(Test_CLI, language_javascript) {
  Run_Program_Result r = run_program(
      {get_quick_lint_js_executable_path(), "--language=javascript", "--stdin"},
      Run_Program_Options{
          .input = u8"let c = <JSX />;"_sv,
      });
  EXPECT_THAT(to_string(r.output.string_view()), HasSubstr("E0177"));
  EXPECT_EQ(r.exit_status, 1);
}

TEST_F(Test_CLI,
       language_typescript_warns_about_undeclared_variables_despite_eval) {
  Run_Program_Result r = run_program(
      {get_quick_lint_js_executable_path(), "--language=typescript", "--stdin"},
      Run_Program_Options{
          .input = u8"eval('var x = 42;'); console.log(x);"_sv,
      });
  EXPECT_THAT(to_string(r.output.string_view()), HasSubstr("E0057"))
      << "\"use of undeclared variable 'x'\"";
  EXPECT_EQ(r.exit_status, 1);
}

#if QLJS_HAVE_FORKPTY
TEST_F(Test_CLI, auto_hyperlinks_are_not_printed_in_dumb_terminals) {
  std::string test_file = this->make_temporary_directory() + "/test.js";
  write_file_or_exit(
      test_file, u8"var parenthesesMissing;\nif parenthesesMissing { }\n"_sv);

  int tty_fd;
  ::pid_t pid =
      forkpty(&tty_fd, /*name=*/nullptr, /*termp=*/nullptr, /*winp=*/nullptr);
  ASSERT_NE(pid, -1) << std::strerror(errno);
  if (pid == 0) {
    // Child.
    std::string qljs = get_quick_lint_js_executable_path();
    char* argv[] = {qljs.data(), test_file.data(), nullptr};
    // FIXME(strager): We should augment the environment, not
    // overwrite it.
    char* envp[] = {const_cast<char*>("TERM=dumb"), nullptr};
    ::execve(qljs.c_str(), argv, envp);
    std::exit(100);
  } else {
    // Parent.
    auto output = read_file(POSIX_FD_File_Ref(tty_fd));
    if (!output.ok()) {
      ADD_FAILURE() << output.error_to_string();
      return;
    }
    std::uint32_t exit_status = wait_for_process_exit(pid);

    EXPECT_EQ(exit_status, 1);
    EXPECT_THAT(to_string(output->string_view()),
                ::testing::Not(HasSubstr("\x1b")));
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
