// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/lsp/lsp-language.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
TEST(Test_LSP_Language, primary_languages) {
  {
    const LSP_Language* language = LSP_Language::find("javascript"sv);
    ASSERT_NE(language, nullptr);
    EXPECT_TRUE(language->lint_options.jsx)
        << "JSX support should be enabled for 'javascript'";
    EXPECT_FALSE(language->lint_options.typescript);
  }

  {
    const LSP_Language* language = LSP_Language::find("javascriptreact"sv);
    ASSERT_NE(language, nullptr);
    EXPECT_TRUE(language->lint_options.jsx);
    EXPECT_FALSE(language->lint_options.typescript);
  }

  {
    const LSP_Language* language = LSP_Language::find("typescript"sv);
    ASSERT_NE(language, nullptr);
    EXPECT_FALSE(language->lint_options.jsx);
    EXPECT_TRUE(language->lint_options.typescript);
  }

  {
    const LSP_Language* language = LSP_Language::find("typescriptreact"sv);
    ASSERT_NE(language, nullptr);
    EXPECT_TRUE(language->lint_options.jsx);
    EXPECT_TRUE(language->lint_options.typescript);
  }
}

TEST(Test_LSP_Language, language_aliases) {
  for (std::string_view alias : {"js"sv}) {
    const LSP_Language* main_language = LSP_Language::find("javascript"sv);
    ASSERT_NE(main_language, nullptr);

    SCOPED_TRACE(alias);
    const LSP_Language* alias_language = LSP_Language::find(alias);
    ASSERT_NE(alias_language, nullptr);
    EXPECT_EQ(alias_language->lint_options, main_language->lint_options);
  }

  for (std::string_view alias : {"js-jsx"sv}) {
    const LSP_Language* main_language = LSP_Language::find("javascriptreact"sv);
    ASSERT_NE(main_language, nullptr);

    SCOPED_TRACE(alias);
    const LSP_Language* alias_language = LSP_Language::find(alias);
    ASSERT_NE(alias_language, nullptr);
    EXPECT_EQ(alias_language->lint_options, main_language->lint_options);
  }

  for (std::string_view alias : {"tsx"sv}) {
    const LSP_Language* main_language = LSP_Language::find("typescriptreact"sv);
    ASSERT_NE(main_language, nullptr);

    SCOPED_TRACE(alias);
    const LSP_Language* alias_language = LSP_Language::find(alias);
    ASSERT_NE(alias_language, nullptr);
    EXPECT_EQ(alias_language->lint_options, main_language->lint_options);
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
