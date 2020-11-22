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

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <initializer_list>
#include <iostream>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/options.h>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
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

TEST(test_options, default_options_with_no_files) {
  options o = parse_options({});
  EXPECT_FALSE(o.print_parser_visits);
  EXPECT_FALSE(o.help);
  EXPECT_FALSE(o.version);
  EXPECT_FALSE(o.lsp_server);
  EXPECT_EQ(o.output_format, output_format::gnu_like);
  EXPECT_THAT(o.files_to_lint, IsEmpty());
}

TEST(test_options, default_options_with_files) {
  options o = parse_options({"foo.js"});
  EXPECT_FALSE(o.print_parser_visits);
  ASSERT_EQ(o.files_to_lint.size(), 1);
  EXPECT_EQ(o.files_to_lint[0].path, "foo.js"sv);
}

TEST(test_options, hyphen_hyphen_treats_remaining_arguments_as_files) {
  {
    options o = parse_options({"--", "foo.js"});
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].path, "foo.js"sv);
  }

  {
    options o =
        parse_options({"--", "--debug-parser-visits", "foo.js", "-bar"});
    EXPECT_FALSE(o.print_parser_visits);
    ASSERT_EQ(o.files_to_lint.size(), 3);
    EXPECT_EQ(o.files_to_lint[0].path, "--debug-parser-visits"sv);
    EXPECT_EQ(o.files_to_lint[1].path, "foo.js"sv);
    EXPECT_EQ(o.files_to_lint[2].path, "-bar"sv);
  }
}

TEST(test_options, debug_parser_visits) {
  options o = parse_options({"--debug-parser-visits", "foo.js"});
  EXPECT_TRUE(o.print_parser_visits);
  ASSERT_EQ(o.files_to_lint.size(), 1);
  EXPECT_EQ(o.files_to_lint[0].path, "foo.js"sv);
}

TEST(test_options, debug_parser_visits_shorthand) {
  {
    options o = parse_options({"--debug-p", "foo.js"});
    EXPECT_TRUE(o.print_parser_visits);
  }

  {
    options o = parse_options({"--debug-parser-vis", "foo.js"});
    EXPECT_TRUE(o.print_parser_visits);
  }
}

TEST(test_options, output_format) {
  {
    options o = parse_options({"--output-format=gnu-like"});
    EXPECT_THAT(o.error_unrecognized_options, IsEmpty());
    EXPECT_EQ(o.output_format, output_format::gnu_like);
  }

  {
    options o = parse_options({"--output-format=vim-qflist-json"});
    EXPECT_THAT(o.error_unrecognized_options, IsEmpty());
    EXPECT_EQ(o.output_format, output_format::vim_qflist_json);
  }
}

TEST(test_options, invalid_output_format) {
  {
    options o = parse_options({"--output-format=unknown-garbage"});
    EXPECT_THAT(o.error_unrecognized_options, ElementsAre("unknown-garbage"sv));
    EXPECT_EQ(o.output_format, output_format::gnu_like)
        << "output_format should remain the default";
  }

  {
    options o = parse_options({"--output-format"});
    EXPECT_THAT(o.error_unrecognized_options, ElementsAre("--output-format"sv));
  }
}

TEST(test_options, vim_file_bufnr) {
  {
    options o = parse_options({"one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, std::nullopt);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, std::nullopt);
  }

  {
    options o = parse_options({"--output-format", "vim-qflist-json",
                               "--vim-file-bufnr", "3", "file.js"});
    EXPECT_THAT(o.error_unrecognized_options, IsEmpty());
    ASSERT_EQ(o.files_to_lint.size(), 1);
    EXPECT_EQ(o.files_to_lint[0].path, "file.js"sv);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, 3);
  }

  {
    options o = parse_options({"--vim-file-bufnr", "3", "one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, 3);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, std::nullopt);
  }

  {
    options o = parse_options({"one.js", "--vim-file-bufnr=10", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, std::nullopt);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, 10);
  }

  {
    options o = parse_options(
        {"--vim-file-bufnr=1", "one.js", "--vim-file-bufnr=2", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, 1);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, 2);
  }

  {
    options o = parse_options({"--vim-file-bufnr=1", "--", "one.js", "two.js"});
    ASSERT_EQ(o.files_to_lint.size(), 2);
    EXPECT_EQ(o.files_to_lint[0].vim_bufnr, 1);
    EXPECT_EQ(o.files_to_lint[1].vim_bufnr, std::nullopt);
  }
}

TEST(test_options, lsp_server) {
  {
    options o = parse_options({"--lsp-server"});
    EXPECT_TRUE(o.lsp_server);
  }

  {
    options o = parse_options({"--lsp"});
    EXPECT_TRUE(o.lsp_server);
  }
}

TEST(test_options, print_help) {
  {
    options o = parse_options({"--help"});
    EXPECT_TRUE(o.help);
  }

  {
    options o = parse_options({"--h"});
    EXPECT_TRUE(o.help);
  }

  {
    options o = parse_options({"-h"});
    EXPECT_TRUE(o.help);
  }
}

TEST(test_options, print_version) {
  {
    options o = parse_options({"--version"});
    EXPECT_TRUE(o.version);
  }

  {
    options o = parse_options({"--v"});
    EXPECT_TRUE(o.version);
  }

  {
    options o = parse_options({"-v"});
    EXPECT_TRUE(o.version);
  }
}

TEST(test_options, invalid_vim_file_bufnr) {
  {
    options o = parse_options({"--vim-file-bufnr=garbage", "file.js"});
    ASSERT_EQ(o.error_unrecognized_options.size(), 1);
    EXPECT_EQ(o.error_unrecognized_options[0], "garbage"sv);
  }

  {
    options o = parse_options({"--vim-file-bufnr"});
    ASSERT_EQ(o.error_unrecognized_options.size(), 1);
    EXPECT_EQ(o.error_unrecognized_options[0], "--vim-file-bufnr"sv);
  }
}

// TODO(strager): Report warning for trailing (ununsed) --vim-file-bufnr.

// TODO(strager): Report warning for using --vim-file-bufnr without
// --output-format.

TEST(test_options, invalid_option) {
  {
    options o = parse_options({"--option-does-not-exist", "foo.js"});
    ASSERT_EQ(o.error_unrecognized_options.size(), 1);
    EXPECT_EQ(o.error_unrecognized_options[0], "--option-does-not-exist"sv);
    EXPECT_THAT(o.files_to_lint, IsEmpty());
  }

  {
    options o = parse_options({"--debug-parse-vixxx", "foo.js"});
    ASSERT_EQ(o.error_unrecognized_options.size(), 1);
    EXPECT_EQ(o.error_unrecognized_options[0], "--debug-parse-vixxx"sv);
    EXPECT_THAT(o.files_to_lint, IsEmpty());
  }

  {
    options o = parse_options({"--debug-parse-visits-xxx", "foo.js"});
    ASSERT_EQ(o.error_unrecognized_options.size(), 1);
    EXPECT_EQ(o.error_unrecognized_options[0], "--debug-parse-visits-xxx"sv);
    EXPECT_THAT(o.files_to_lint, IsEmpty());
  }

  {
    options o = parse_options({"-version", "foo.js"});
    ASSERT_EQ(o.error_unrecognized_options.size(), 1);
    EXPECT_EQ(o.error_unrecognized_options[0], "-version"sv);
    EXPECT_THAT(o.files_to_lint, IsEmpty());
  }
}
}
}
