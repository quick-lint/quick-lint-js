// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/lsp/lsp-language.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
constexpr String8_View dummy_uri = u8"file:///example.txt"_sv;

TEST(Test_LSP_Language, primary_languages) {
  {
    const LSP_Language* language =
        LSP_Language::find("javascript"sv, dummy_uri);
    ASSERT_NE(language, nullptr);
    EXPECT_TRUE(language->lint_options.jsx)
        << "JSX support should be enabled for 'javascript'";
    EXPECT_FALSE(language->lint_options.typescript);
  }

  {
    const LSP_Language* language =
        LSP_Language::find("javascriptreact"sv, dummy_uri);
    ASSERT_NE(language, nullptr);
    EXPECT_TRUE(language->lint_options.jsx);
    EXPECT_FALSE(language->lint_options.typescript);
  }

  {
    const LSP_Language* language =
        LSP_Language::find("typescriptsource"sv, dummy_uri);
    ASSERT_NE(language, nullptr);
    EXPECT_FALSE(language->lint_options.jsx);
    EXPECT_TRUE(language->lint_options.typescript);
    EXPECT_FALSE(language->lint_options.typescript_definition);
  }

  {
    const LSP_Language* language =
        LSP_Language::find("typescriptdefinition"sv, dummy_uri);
    ASSERT_NE(language, nullptr);
    EXPECT_FALSE(language->lint_options.jsx);
    EXPECT_TRUE(language->lint_options.typescript);
    EXPECT_TRUE(language->lint_options.typescript_definition);
  }

  {
    const LSP_Language* language =
        LSP_Language::find("typescriptreact"sv, dummy_uri);
    ASSERT_NE(language, nullptr);
    EXPECT_TRUE(language->lint_options.jsx);
    EXPECT_TRUE(language->lint_options.typescript);
  }
}

TEST(Test_LSP_Language, language_aliases) {
  for (std::string_view alias : {"js"sv}) {
    const LSP_Language* main_language =
        LSP_Language::find("javascript"sv, dummy_uri);
    ASSERT_NE(main_language, nullptr);

    SCOPED_TRACE(alias);
    const LSP_Language* alias_language = LSP_Language::find(alias, dummy_uri);
    ASSERT_NE(alias_language, nullptr);
    EXPECT_EQ(alias_language->lint_options, main_language->lint_options);
  }

  for (std::string_view alias : {"js-jsx"sv}) {
    const LSP_Language* main_language =
        LSP_Language::find("javascriptreact"sv, dummy_uri);
    ASSERT_NE(main_language, nullptr);

    SCOPED_TRACE(alias);
    const LSP_Language* alias_language = LSP_Language::find(alias, dummy_uri);
    ASSERT_NE(alias_language, nullptr);
    EXPECT_EQ(alias_language->lint_options, main_language->lint_options);
  }

  for (std::string_view alias : {"tsx"sv}) {
    const LSP_Language* main_language =
        LSP_Language::find("typescriptreact"sv, dummy_uri);
    ASSERT_NE(main_language, nullptr);

    SCOPED_TRACE(alias);
    const LSP_Language* alias_language = LSP_Language::find(alias, dummy_uri);
    ASSERT_NE(alias_language, nullptr);
    EXPECT_EQ(alias_language->lint_options, main_language->lint_options);
  }
}

TEST(Test_LSP_Language, typescript_file_without_d_or_tsx_is_source) {
  {
    const LSP_Language* language =
        LSP_Language::find("typescript"sv, u8"file:///test.ts"_sv);
    ASSERT_NE(language, nullptr);
    EXPECT_FALSE(language->lint_options.typescript_definition);
  }

  {
    const LSP_Language* language =
        LSP_Language::find("typescript"sv, u8"file:///folder.d.ts/test.ts"_sv);
    ASSERT_NE(language, nullptr);
    EXPECT_FALSE(language->lint_options.typescript_definition)
        << ".d. in containing directory should be ignored";
  }

  // TODO(strager): Query parameters should be ignored.
  // TODO(strager): Fragments should be ignored.
}

TEST(Test_LSP_Language, typescript_file_with_d_is_definition) {
  for (String8_View uri : {
           u8"file:///test.d.ts"_sv,       //
           u8"file:///test.d.tsx"_sv,      //
           u8"file:///test.d.json.ts"_sv,  //
           u8"file:///test.d.mts"_sv,      //
           u8"file:///test.d.cts"_sv,      //
           // TODO(strager): What should % encoding do?
       }) {
    SCOPED_TRACE(out_string8(uri));
    const LSP_Language* language = LSP_Language::find("typescript"sv, uri);
    ASSERT_NE(language, nullptr);
    EXPECT_TRUE(language->lint_options.typescript_definition);
    EXPECT_FALSE(language->lint_options.jsx);
  }
}

TEST(Test_LSP_Language, typescript_file_with_tsx_is_typescript_jsx) {
  const LSP_Language* language =
      LSP_Language::find("typescript"sv, u8"file:///test.tsx"_sv);
  ASSERT_NE(language, nullptr);
  EXPECT_TRUE(language->lint_options.jsx);
  EXPECT_TRUE(language->lint_options.typescript);
}

TEST(Test_LSP_Language, typescriptsource_ignores_d_in_uri) {
  const LSP_Language* language =
      LSP_Language::find("typescriptsource"sv, u8"file:///test.d.ts"_sv);
  ASSERT_NE(language, nullptr);
  EXPECT_FALSE(language->lint_options.typescript_definition);
}

TEST(Test_LSP_Language, typescriptsource_ignores_tsx_in_uri) {
  const LSP_Language* language =
      LSP_Language::find("typescriptsource"sv, u8"file:///test.tsx"_sv);
  ASSERT_NE(language, nullptr);
  EXPECT_FALSE(language->lint_options.jsx);
}

TEST(Test_LSP_Language, typescriptdefinition_does_not_require_d_in_uri) {
  const LSP_Language* language =
      LSP_Language::find("typescriptdefinition"sv, u8"file:///test.ts"_sv);
  ASSERT_NE(language, nullptr);
  EXPECT_TRUE(language->lint_options.typescript_definition);
}

TEST(Test_LSP_Language, typescriptreact_does_not_require_tsx_in_uri) {
  const LSP_Language* language =
      LSP_Language::find("typescriptreact"sv, u8"file:///test.ts"_sv);
  ASSERT_NE(language, nullptr);
  EXPECT_TRUE(language->lint_options.jsx);
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
