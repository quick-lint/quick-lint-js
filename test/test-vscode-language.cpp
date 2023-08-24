// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/vscode/vscode-language.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
constexpr bool allow_typescript = true;

TEST(Test_VSCode_Language, primary_languages) {
  {
    const VSCode_Language* language =
        VSCode_Language::find("javascript"sv, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_TRUE(language->lint_options.jsx)
        << "JSX support should be enabled for 'javascript'";
    EXPECT_FALSE(language->lint_options.typescript);
  }

  {
    const VSCode_Language* language =
        VSCode_Language::find("javascriptreact"sv, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_TRUE(language->lint_options.jsx);
    EXPECT_FALSE(language->lint_options.typescript);
  }

  {
    const VSCode_Language* language =
        VSCode_Language::find("typescript"sv, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_FALSE(language->lint_options.jsx);
    EXPECT_TRUE(language->lint_options.typescript);
  }

  {
    const VSCode_Language* language =
        VSCode_Language::find("typescriptreact"sv, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_TRUE(language->lint_options.jsx);
    EXPECT_TRUE(language->lint_options.typescript);
  }
}

TEST(Test_VSCode_Language, typescript_is_not_detected_if_disabled) {
  constexpr bool disallow_typescript = false;

  EXPECT_EQ(VSCode_Language::find("typescript"sv, disallow_typescript),
            nullptr);
  EXPECT_EQ(VSCode_Language::find("typescriptreact"sv, disallow_typescript),
            nullptr);
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
