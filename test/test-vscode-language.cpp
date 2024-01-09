// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/vscode/vscode-language.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
constexpr String8_View dummy_uri = u8"file:///example.txt"_sv;
constexpr bool allow_typescript = true;

TEST(Test_VSCode_Language, primary_languages) {
  {
    const VSCode_Language* language =
        VSCode_Language::find("javascript"sv, dummy_uri, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_EQ(language->language, File_Language::javascript_jsx)
        << "JSX support should be enabled for 'javascript'";
  }

  {
    const VSCode_Language* language =
        VSCode_Language::find("javascriptreact"sv, dummy_uri, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_EQ(language->language, File_Language::javascript_jsx);
  }

  {
    const VSCode_Language* language =
        VSCode_Language::find("typescript"sv, dummy_uri, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_EQ(language->language, File_Language::typescript);
  }

  {
    const VSCode_Language* language =
        VSCode_Language::find("typescriptreact"sv, dummy_uri, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_EQ(language->language, File_Language::typescript_jsx);
  }
}

TEST(Test_VSCode_Language, typescript_is_not_detected_if_disabled) {
  constexpr bool disallow_typescript = false;

  EXPECT_EQ(
      VSCode_Language::find("typescript"sv, dummy_uri, disallow_typescript),
      nullptr);
  EXPECT_EQ(VSCode_Language::find("typescriptreact"sv, dummy_uri,
                                  disallow_typescript),
            nullptr);
}

TEST(Test_VSCode_Language, typescript_file_without_d_is_source) {
  {
    const VSCode_Language* language = VSCode_Language::find(
        "typescript"sv, u8"file:///test.ts"_sv, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_EQ(language->language, File_Language::typescript);
  }

  {
    const VSCode_Language* language = VSCode_Language::find(
        "typescript"sv, u8"file:///folder.d.ts/test.ts"_sv, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_EQ(language->language, File_Language::typescript)
        << ".d. in containing directory should be ignored";
  }

  // TODO(strager): Query parameters should be ignored.
  // TODO(strager): Fragments should be ignored.
}

TEST(Test_VSCode_Language, typescript_file_with_d_is_definition) {
  for (String8_View uri : {
           u8"file:///test.d.ts"_sv,       //
           u8"file:///test.d.json.ts"_sv,  //
           u8"file:///test.d.mts"_sv,      //
           u8"file:///test.d.cts"_sv,      //
           // TODO(strager): What should % encoding do?
       }) {
    SCOPED_TRACE(out_string8(uri));
    const VSCode_Language* language =
        VSCode_Language::find("typescript"sv, uri, allow_typescript);
    ASSERT_NE(language, nullptr);
    EXPECT_EQ(language->language, File_Language::typescript_definition);
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
