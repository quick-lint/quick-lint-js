// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <initializer_list>
#include <iostream>
#include <quick-lint-js/cli/options.h>
#include <quick-lint-js/diag/diag-code-list.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string_view>
#include <vector>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
options parse_options(std::initializer_list<const char *> arguments) {
  std::vector<char *> argv;
  argv.emplace_back(const_cast<char *>("(program)"));
  for (const char *argument : arguments) {
    argv.emplace_back(const_cast<char *>(argument));
  }
  return quick_lint_js::parse_options(narrow_cast<int>(argv.size()),
                                      argv.data());
}

options parse_options_no_errors(std::initializer_list<const char *> arguments) {
  options o = parse_options(arguments);
  EXPECT_THAT(o.error_unrecognized_options, IsEmpty());
  EXPECT_THAT(o.warning_language_without_file, IsEmpty());
  EXPECT_THAT(o.warning_vim_bufnr_without_file, IsEmpty());
  return o;
}

struct dumped_errors {
  bool have_errors;
  string8 output;
};

dumped_errors dump_errors(const options &o) {
  memory_output_stream output;
  bool have_errors = o.dump_errors(output);
  output.flush();
  return dumped_errors{
      .have_errors = have_errors,
      .output = output.get_flushed_string8(),
  };
}

TEST(test_options, default_options_with_no_files) {
  options o = parse_options_no_errors({});
  EXPECT_FALSE(o.print_parser_visits);
  EXPECT_FALSE(o.help);
  EXPECT_FALSE(o.list_debug_apps);
  EXPECT_FALSE(o.version);
  EXPECT_FALSE(o.lsp_server);
  EXPECT_EQ(o.output_format, output_format::default_format);
  EXPECT_THAT(o.files_to_lint, IsEmpty());
}

TEST(test_options, default_options_with_files) {
  options o = parse_options_no_errors({"foo.js"});
  EXPECT_FALSE(o.print_parser_visits);
  EXPECT_FALSE(o.snarky);
  ASSERT_EQ(o.files_to_lint.size(), 1);
  EXPECT_EQ(o.files_to_lint[0].path, "foo.js"sv);
}

TEST(test_options, hyphen_hyphen_treats_remaining_arguments_as_files) {
  {
    options o = parse_options_no_errors({"--", "foo.js"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].path, "foo.js"sv);
  }

  {
    options o = parse_options_no_errors(
        {"--", "--debug-parser-visits", "foo.js", "-bar"});
    EXPECT_FALSE(o.print_parser_visits);
    ASSERT_EQ(o.files_to_lint.size(), 3);
    EXPECT_EQ(o.files_to_lint[0].path, "--debug-parser-visits"sv);
    EXPECT_EQ(o.files_to_lint[1].path, "foo.js"sv);
    EXPECT_EQ(o.files_to_lint[2].path, "-bar"sv);
  }
}

TEST(test_options, debug_parser_visits) {
  options o = parse_options_no_errors({"--debug-parser-visits", "foo.js"});
  EXPECT_TRUE(o.print_parser_visits);
  ASSERT_EQ(o.files_to_lint.size(), 1);
  EXPECT_EQ(o.files_to_lint[0].path, "foo.js"sv);
}

TEST(test_options, snarky) {
  options o = parse_options_no_errors({"--snarky", "foo.js"});
  EXPECT_TRUE(o.snarky);
  ASSERT_EQ(o.files_to_lint.size(), 1);
  EXPECT_EQ(o.files_to_lint[0].path, "foo.js"sv);
}

TEST(test_options, debug_parser_visits_shorthand) {
  {
    options o = parse_options_no_errors({"--debug-p", "foo.js"});
    EXPECT_TRUE(o.print_parser_visits);
  }

  {
    options o = parse_options_no_errors({"--debug-parser-vis", "foo.js"});
    EXPECT_TRUE(o.print_parser_visits);
  }
}

TEST(test_options, output_format) {
  {
    options o = parse_options_no_errors({});
    EXPECT_EQ(o.output_format, output_format::default_format);
  }

  {
    options o = parse_options_no_errors({"--output-format=gnu-like"});
    EXPECT_EQ(o.output_format, output_format::gnu_like);
  }

  {
    options o = parse_options_no_errors({"--output-format=vim-qflist-json"});
    EXPECT_EQ(o.output_format, output_format::vim_qflist_json);
  }

  {
    options o = parse_options_no_errors({"--output-format=emacs-lisp"});
    EXPECT_EQ(o.output_format, output_format::emacs_lisp);
  }
}

TEST(test_options, invalid_output_format) {
  {
    options o = parse_options({"--output-format=unknown-garbage"});
    EXPECT_THAT(o.error_unrecognized_options,
                ElementsAreArray({"unknown-garbage"sv}));
    EXPECT_EQ(o.output_format, output_format::default_format)
        << "output_format should remain the default";
  }

  {
    options o = parse_options({"--output-format"});
    EXPECT_THAT(o.error_unrecognized_options,
                ElementsAreArray({"--output-format"sv}));
  }
}

TEST(test_options, vim_file_bufnr) {
  {
    options o = parse_options_no_errors({"one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, std::nullopt);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, std::nullopt);
  }

  {
    options o = parse_options_no_errors({"--output-format", "vim-qflist-json",
                                         "--vim-file-bufnr", "3", "file.js"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].path, "file.js"sv);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, 3);
  }

  {
    options o =
        parse_options_no_errors({"--vim-file-bufnr", "3", "one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, 3);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, std::nullopt);
  }

  {
    options o =
        parse_options_no_errors({"one.js", "--vim-file-bufnr=10", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, std::nullopt);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, 10);
  }

  {
    options o = parse_options_no_errors(
        {"--vim-file-bufnr=1", "one.js", "--vim-file-bufnr=2", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, 1);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, 2);
  }

  {
    options o = parse_options_no_errors({"--vim-file-bufnr=42", "-"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, 42);
  }

  {
    options o =
        parse_options_no_errors({"one.js", "--vim-file-bufnr=42", "--stdin"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, 42);
  }

  {
    options o = parse_options_no_errors(
        {"--vim-file-bufnr=1", "--", "one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, 1);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, std::nullopt);
  }
}

TEST(test_options, path_for_config_search) {
  {
    options o = parse_options_no_errors({"one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].path_for_config_search, nullptr);
    EXPECT_EQ(o.files_to_lint[1].path_for_config_search, nullptr);
  }

  {
    options o = parse_options_no_errors(
        {"--path-for-config-search", "configme.js", "file.js"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].path, "file.js"sv);
    EXPECT_STREQ(o.files_to_lint[0].path_for_config_search, "configme.js");
  }

  {
    options o = parse_options_no_errors(
        {"--path-for-config-search", "configme.js", "one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_STREQ(o.files_to_lint[0].path_for_config_search, "configme.js");
    EXPECT_EQ(o.files_to_lint[1].path_for_config_search, nullptr);
  }

  {
    options o = parse_options_no_errors(
        {"one.js", "--path-for-config-search=configme.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].path_for_config_search, nullptr);
    EXPECT_STREQ(o.files_to_lint[1].path_for_config_search, "configme.js");
  }

  {
    options o = parse_options_no_errors(
        {"--path-for-config-search=test/one.js", "/tmp/one.js",
         "--path-for-config-search=src/two.js", "/tmp/two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_STREQ(o.files_to_lint[0].path_for_config_search, "test/one.js");
    EXPECT_STREQ(o.files_to_lint[1].path_for_config_search, "src/two.js");
  }

  {
    options o =
        parse_options_no_errors({"--path-for-config-search=configme.js", "-"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_STREQ(o.files_to_lint[0].path_for_config_search, "configme.js");
  }

  {
    options o = parse_options_no_errors(
        {"one.js", "--path-for-config-search=configme.js", "--stdin"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_STREQ(o.files_to_lint[1].path_for_config_search, "configme.js");
  }

  {
    options o = parse_options_no_errors(
        {"--path-for-config-search=configme.js", "--", "one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_STREQ(o.files_to_lint[0].path_for_config_search, "configme.js");
    EXPECT_EQ(o.files_to_lint[1].path_for_config_search, nullptr);
  }

  {
    options o = parse_options_no_errors(
        {"--path-for-config-search=configme.js", "--stdin", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_STREQ(o.files_to_lint[0].path_for_config_search, "configme.js");
    EXPECT_EQ(o.files_to_lint[1].path_for_config_search, nullptr);
  }
}

TEST(test_options, config_file) {
  {
    options o = parse_options_no_errors({"one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].config_file, nullptr);
    EXPECT_EQ(o.files_to_lint[1].config_file, nullptr);
    EXPECT_FALSE(o.has_config_file);
  }

  {
    options o =
        parse_options_no_errors({"--config-file", "config.json", "file.js"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].path, "file.js"sv);
    EXPECT_EQ(o.files_to_lint[0].config_file, "config.json"sv);
    EXPECT_TRUE(o.has_config_file);
  }

  {
    options o = parse_options_no_errors(
        {"--config-file", "config.json", "one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].config_file, "config.json"sv);
    EXPECT_EQ(o.files_to_lint[1].config_file, "config.json"sv);
  }

  {
    options o = parse_options_no_errors(
        {"one.js", "--config-file=config.json", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].config_file, nullptr);
    EXPECT_EQ(o.files_to_lint[1].config_file, "config.json"sv);
  }

  {
    options o = parse_options_no_errors({"--config-file=one.config", "one.js",
                                         "--config-file=two.config", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].config_file, "one.config"sv);
    EXPECT_EQ(o.files_to_lint[1].config_file, "two.config"sv);
  }

  {
    options o = parse_options_no_errors({"--config-file=config.json", "-"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].config_file, "config.json"sv);
  }

  {
    options o = parse_options_no_errors(
        {"one.js", "--config-file=config.json", "--stdin"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[1].config_file, "config.json"sv);
  }

  {
    options o = parse_options_no_errors(
        {"--config-file=config.json", "--", "one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].config_file, "config.json"sv);
    EXPECT_EQ(o.files_to_lint[1].config_file, "config.json"sv);
  }
}

TEST(test_options, language) {
  {
    options o =
        parse_options_no_errors({"one.js", "two.ts", "three.txt", "--stdin"});
    ASSERT_EQ(o.files_to_lint.size(), 4);
    EXPECT_EQ(o.files_to_lint[0].language, std::nullopt) << "one.js";
    EXPECT_EQ(o.files_to_lint[1].language, std::nullopt) << "two.ts";
    EXPECT_EQ(o.files_to_lint[2].language, std::nullopt) << "three.txt";
    EXPECT_EQ(o.files_to_lint[3].language, std::nullopt) << "--stdin";
  }

  {
    options o = parse_options_no_errors(
        {"--language=javascript", "one.js", "two.ts", "three.txt"});
    ASSERT_EQ(o.files_to_lint.size(), 3);
    EXPECT_EQ(o.files_to_lint[0].language, input_file_language::javascript);
    EXPECT_EQ(o.files_to_lint[1].language, input_file_language::javascript);
    EXPECT_EQ(o.files_to_lint[2].language, input_file_language::javascript);
  }

  {
    options o =
        parse_options_no_errors({"--language=javascript", "one.js",
                                 "--language=javascript-jsx", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].language, input_file_language::javascript);
    EXPECT_EQ(o.files_to_lint[1].language, input_file_language::javascript_jsx);
  }

  {
    options o = parse_options_no_errors(
        {"one.js", "--language=javascript-jsx", "two.jsx"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].language, std::nullopt);
    EXPECT_EQ(o.files_to_lint[1].language, input_file_language::javascript_jsx);
  }

  {
    options o = parse_options_no_errors(
        {"--language=experimental-typescript", "one.txt"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].language, input_file_language::typescript);
  }

  {
    options o = parse_options_no_errors(
        {"--language=experimental-typescript-jsx", "one.txt"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].language, input_file_language::typescript_jsx);
  }

  {
    options o = parse_options_no_errors({"--language=javascript-jsx", "-"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].language, input_file_language::javascript_jsx);
  }

  {
    options o =
        parse_options_no_errors({"--language=javascript-jsx", "--stdin"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].language, input_file_language::javascript_jsx);
  }

  {
    options o = parse_options({"file.js", "--language=javascript-jsx"});
    EXPECT_THAT(o.warning_language_without_file,
                ElementsAreArray({"javascript-jsx"sv}));

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(
        errors.output,
        u8"warning: flag '--language=javascript-jsx' should be followed by an "
        u8"input file name or --stdin\n");
  }

  {
    options o = parse_options(
        {"--language=javascript", "--language=javascript-jsx", "test.jsx"});
    EXPECT_THAT(o.warning_language_without_file,
                ElementsAreArray({"javascript"sv}));

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(
        errors.output,
        u8"warning: flag '--language=javascript' should be followed by an "
        u8"input file name or --stdin\n");
  }

  {
    options o = parse_options({"--language=badlanguageid", "test.js"});
    EXPECT_THAT(o.warning_language_without_file, IsEmpty());
    // TODO(strager): Highlight the full option, not just the value.
    EXPECT_THAT(o.error_unrecognized_options,
                ElementsAreArray({"badlanguageid"sv}));
  }
}

TEST(test_options, get_language_from_path) {
  constexpr auto javascript_jsx = input_file_language::javascript_jsx;
  EXPECT_EQ(get_language("<stdin>", std::nullopt), javascript_jsx);
  EXPECT_EQ(get_language("hi.js", std::nullopt), javascript_jsx);
  EXPECT_EQ(get_language("hi.jsx", std::nullopt), javascript_jsx);
  EXPECT_EQ(get_language("hi.txt", std::nullopt), javascript_jsx);
}

TEST(test_options, get_language_overwritten) {
  constexpr auto javascript = input_file_language::javascript;
  constexpr auto javascript_jsx = input_file_language::javascript_jsx;

  EXPECT_EQ(get_language("<stdin>", javascript_jsx), javascript_jsx);
  EXPECT_EQ(get_language("hi.js", javascript_jsx), javascript_jsx);
  EXPECT_EQ(get_language("hi.jsx", javascript_jsx), javascript_jsx);
  EXPECT_EQ(get_language("hi.txt", javascript_jsx), javascript_jsx);

  EXPECT_EQ(get_language("<stdin>", javascript), javascript);
  EXPECT_EQ(get_language("hi.js", javascript), javascript);
  EXPECT_EQ(get_language("hi.jsx", javascript), javascript);
  EXPECT_EQ(get_language("hi.txt", javascript), javascript);
}

TEST(test_options, lsp_server) {
  {
    options o = parse_options_no_errors({"--lsp-server"});
    EXPECT_TRUE(o.lsp_server);
  }

  {
    options o = parse_options_no_errors({"--lsp"});
    EXPECT_TRUE(o.lsp_server);
  }
}

TEST(test_options, dash_dash_stdin) {
  {
    options o = parse_options_no_errors({"--stdin", "one.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_TRUE(o.files_to_lint[0].is_stdin);
    EXPECT_FALSE(o.has_multiple_stdin);
  }

  {
    options o = parse_options_no_errors({"one.js", "--stdin"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_TRUE(o.files_to_lint[1].is_stdin);
    EXPECT_FALSE(o.has_multiple_stdin);
  }

  {
    options o = parse_options_no_errors({"-"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_TRUE(o.files_to_lint[0].is_stdin);
    EXPECT_FALSE(o.has_multiple_stdin);
  }
}

TEST(test_options, is_stdin_emplaced_only_once) {
  {
    options o = parse_options_no_errors({"--stdin", "one.js", "-", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 3);
    EXPECT_TRUE(o.has_multiple_stdin);
  }
  {
    options o = parse_options_no_errors({"one.js", "-", "two.js", "-"});
    ASSERT_EQ(o.files_to_lint.size(), 3);
    EXPECT_TRUE(o.has_multiple_stdin);
  }
}

TEST(test_options, single_hyphen_is_argument) {
  {
    options o = parse_options_no_errors({"one.js", "-", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 3);
  }
}

TEST(test_options, print_help) {
  {
    options o = parse_options_no_errors({"--help"});
    EXPECT_TRUE(o.help);
  }

  {
    options o = parse_options_no_errors({"--h"});
    EXPECT_TRUE(o.help);
  }

  {
    options o = parse_options_no_errors({"-h"});
    EXPECT_TRUE(o.help);
  }
}

TEST(test_options, list_debug_apps) {
  options o = parse_options_no_errors({"--debug-apps"});
  EXPECT_TRUE(o.list_debug_apps);
}

TEST(test_options, print_version) {
  {
    options o = parse_options_no_errors({"--version"});
    EXPECT_TRUE(o.version);
  }

  {
    options o = parse_options_no_errors({"--v"});
    EXPECT_TRUE(o.version);
  }

  {
    options o = parse_options_no_errors({"-v"});
    EXPECT_TRUE(o.version);
  }
}

TEST(test_options, exit_fail_on) {
  {
    options o = parse_options_no_errors({"--exit-fail-on=E0003", "file.js"});
    EXPECT_TRUE(
        o.exit_fail_on.is_present(diag_type::diag_assignment_to_const_variable))
        << "E0003 should cause failure";
    EXPECT_FALSE(o.exit_fail_on.is_present(
        diag_type::diag_big_int_literal_contains_decimal_point))
        << "E0005 should not cause failure";
  }
}

TEST(test_options, invalid_vim_file_bufnr) {
  {
    options o = parse_options({"--vim-file-bufnr=garbage", "file.js"});
    EXPECT_THAT(o.error_unrecognized_options, ElementsAreArray({"garbage"sv}));
  }

  {
    options o = parse_options({"--vim-file-bufnr"});
    EXPECT_THAT(o.error_unrecognized_options,
                ElementsAreArray({"--vim-file-bufnr"sv}));
  }
}

TEST(test_options, no_following_filename_vim_file_bufnr) {
  {
    options o = parse_options({"foo.js", "--vim-file-bufnr=1"});
    o.output_format = output_format::vim_qflist_json;

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: flag: '--vim-file-bufnr=1' should be followed by an "
              u8"input file name or --stdin\n");
  }
  {
    options o =
        parse_options({"--vim-file-bufnr=1", "--vim-file-bufnr=2", "foo.js"});
    o.output_format = output_format::vim_qflist_json;

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: flag: '--vim-file-bufnr=1' should be followed by an "
              u8"input file name or --stdin\n");
  }
  {
    options o =
        parse_options({"--vim-file-bufnr=1", "foo.js", "--vim-file-bufnr=2"});
    o.output_format = output_format::vim_qflist_json;

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: flag: '--vim-file-bufnr=2' should be followed by an "
              u8"input file name or --stdin\n");
  }
  {
    options o = parse_options({"--vim-file-bufnr=1", "--vim-file-bufnr=2"});
    o.output_format = output_format::vim_qflist_json;

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: flag: '--vim-file-bufnr=1' should be followed by an "
              u8"input file name or --stdin\n"
              u8"warning: flag: '--vim-file-bufnr=2' should be followed by an "
              u8"input file name or --stdin\n");
  }
  {
    options o = parse_options_no_errors({"--vim-file-bufnr=1",
                                         "foo.js"
                                         "--vim-file-bufnr=2",
                                         "--stdin"});
    o.output_format = output_format::vim_qflist_json;

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output, u8"");
  }
  {
    // test if the right argument gets inserted into the error message
    options o =
        parse_options({"--vim-file-bufnr=11", "--output-format=vim-qflist-json",
                       "--vim-file-bufnr=22", "foo.js"});
    o.output_format = output_format::vim_qflist_json;

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: flag: '--vim-file-bufnr=11' should be followed by an "
              u8"input file name or --stdin\n");
  }
}

TEST(test_options, using_vim_file_bufnr_without_format) {
  {
    for (const auto &format : {
             output_format::default_format,
             output_format::gnu_like,
             output_format::emacs_lisp,
         }) {
      options o = parse_options_no_errors({"--vim-file-bufnr=1", "file.js"});
      o.output_format = format;

      dumped_errors errors = dump_errors(o);
      EXPECT_FALSE(errors.have_errors);
      EXPECT_EQ(errors.output,
                u8"warning: --output-format selected which doesn't use "
                u8"--vim-file-bufnr\n");
    }
  }
  {
    options o = parse_options_no_errors({"--vim-file-bufnr=1", "file.js"});
    o.output_format = output_format::vim_qflist_json;

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output, u8"");
  }
}

TEST(test_options, using_vim_file_bufnr_in_lsp_mode) {
  {
    options o = parse_options({"--lsp-server", "--vim-file-bufnr=1"});

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: ignoring --vim-file-bufnr in --lsp-server mode\n");
  }
  {
    options o = parse_options({"--lsp-server", "--vim-file-bufnr=1", "foo.js"});

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: ignoring files given on command line in --lsp-server "
              u8"mode\n"
              u8"warning: ignoring --vim-file-bufnr in --lsp-server mode\n");
  }
}

TEST(test_options, using_language_in_lsp_mode) {
  {
    options o = parse_options({"--lsp-server", "--language=javascript"});

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: ignoring --language in --lsp-server mode\n");
  }
  {
    options o =
        parse_options({"--lsp-server", "--language=javascript", "foo.js"});

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: ignoring files given on command line in --lsp-server "
              u8"mode\n"
              u8"warning: ignoring --language in --lsp-server mode\n");
  }
}

TEST(test_options, invalid_option) {
  {
    options o = parse_options({"--option-does-not-exist", "foo.js"});
    EXPECT_THAT(o.error_unrecognized_options,
                ElementsAreArray({"--option-does-not-exist"sv}));
    EXPECT_THAT(o.files_to_lint, IsEmpty());
  }

  {
    options o = parse_options({"--debug-parse-vixxx", "foo.js"});
    EXPECT_THAT(o.error_unrecognized_options,
                ElementsAreArray({"--debug-parse-vixxx"sv}));
    EXPECT_THAT(o.files_to_lint, IsEmpty());
  }

  {
    options o = parse_options({"--debug-parse-visits-xxx", "foo.js"});
    EXPECT_THAT(o.error_unrecognized_options,
                ElementsAreArray({"--debug-parse-visits-xxx"sv}));
    EXPECT_THAT(o.files_to_lint, IsEmpty());
  }

  {
    options o = parse_options({"-version", "foo.js"});
    EXPECT_THAT(o.error_unrecognized_options, ElementsAreArray({"-version"sv}));
    EXPECT_THAT(o.files_to_lint, IsEmpty());
  }
}

TEST(test_options, dump_errors) {
  {
    options o;
    o.error_unrecognized_options.clear();

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output, u8"");
  }

  {
    options o;
    o.error_unrecognized_options.push_back("--bad-option");

    dumped_errors errors = dump_errors(o);
    EXPECT_TRUE(errors.have_errors);
    EXPECT_EQ(errors.output, u8"error: unrecognized option: --bad-option\n");
  }

  {
    options o;

    parsed_diag_code_list parsed_errors;
    parsed_errors.included_categories.emplace_back("banana");
    parsed_errors.excluded_codes.emplace_back("E9999");
    o.exit_fail_on.add(parsed_errors);

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: unknown error category: banana\n"
              u8"warning: unknown error code: E9999\n");
  }

  {
    options o;
    o.exit_fail_on.add(parsed_diag_code_list());

    dumped_errors errors = dump_errors(o);
    EXPECT_TRUE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"error: --exit-fail-on must be given at least one category or "
              u8"code\n");
  }

  {
    options o;
    o.lsp_server = true;
    o.output_format = output_format::default_format;

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output, u8"");
  }

  {
    for (const auto &format : {
             /* default_format intentionally left out */
             output_format::gnu_like,
             output_format::vim_qflist_json,
         }) {
      options o;
      o.lsp_server = true;
      o.output_format = format;

      dumped_errors errors = dump_errors(o);
      EXPECT_FALSE(errors.have_errors);
      EXPECT_EQ(errors.output,
                u8"warning: --output-format ignored with --lsp-server\n");
    }
  }

  {
    options o;
    o.lsp_server = true;
    o.has_config_file = true;

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: --config-file ignored in --lsp-server mode\n");
  }

  {
    const file_to_lint file = {
        .path = "file.js",
        .config_file = nullptr,
        .language = std::nullopt,
        .is_stdin = false,
        .vim_bufnr = std::optional<int>(),
    };

    options o;
    o.lsp_server = true;
    o.files_to_lint.emplace_back(file);

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: ignoring files given on command line in "
              u8"--lsp-server mode\n");
  }

  {
    options o;
    o.lsp_server = true;
    o.exit_fail_on.add(parse_diag_code_list("E0001"));

    dumped_errors errors = dump_errors(o);
    EXPECT_FALSE(errors.have_errors);
    EXPECT_EQ(errors.output,
              u8"warning: --exit-fail-on ignored with --lsp-server\n");
  }
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
