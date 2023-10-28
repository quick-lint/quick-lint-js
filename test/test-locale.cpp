// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <quick-lint-js/i18n/locale.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>

using ::testing::ElementsAreArray;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
std::vector<std::string> locale_name_combinations(const char* locale_name) {
  std::vector<std::string> locale_names;
  enumerate_locale_name_combinations(
      locale_name, [&](std::string_view current_locale) -> bool {
        locale_names.emplace_back(current_locale);
        return true;
      });
  return locale_names;
}

TEST(Test_Locale, combinations_for_language) {
  EXPECT_THAT(locale_name_combinations("en"), ElementsAreArray({"en"}));
}

TEST(Test_Locale, combinations_for_language_with_territory) {
  EXPECT_THAT(locale_name_combinations("fr_FR"),
              ElementsAreArray({"fr_FR", "fr"}));
}

TEST(Test_Locale, combinations_for_language_with_codeset) {
  EXPECT_THAT(locale_name_combinations("fr.utf8"),
              ElementsAreArray({"fr.utf8", "fr"}));
}

TEST(Test_Locale, combinations_for_language_with_modifier) {
  EXPECT_THAT(locale_name_combinations("fr@bon"),
              ElementsAreArray({"fr@bon", "fr"}));
}

TEST(Test_Locale, combinations_for_language_with_territory_and_modifier) {
  EXPECT_THAT(locale_name_combinations("fr_FR@bon"),
              ElementsAreArray({"fr_FR@bon", "fr@bon", "fr_FR", "fr"}));
}

TEST(Test_Locale, combinations_for_language_with_territory_and_codeset) {
  EXPECT_THAT(locale_name_combinations("fr_FR.utf8"),
              ElementsAreArray({"fr_FR.utf8", "fr_FR", "fr.utf8", "fr"}));
}

TEST(Test_Locale,
     combinations_for_language_with_territory_and_codeset_and_modifier) {
  EXPECT_THAT(
      locale_name_combinations("fr_FR.utf8@bon"),
      ElementsAreArray({"fr_FR.utf8@bon", "fr_FR@bon", "fr.utf8@bon", "fr@bon",
                        "fr_FR.utf8", "fr_FR", "fr.utf8", "fr"}));
}

TEST(Test_Locale, modifier_can_contain_underscores_and_at_signs) {
  EXPECT_THAT(locale_name_combinations("fr@a_b@c"),
              ElementsAreArray({"fr@a_b@c", "fr"}));
}

template <class Func>
void for_each_locale_permutation(std::vector<std::string_view> locales,
                                 Func&& callback) {
  sort(locales);
  do {
    callback(locales);
  } while (std::next_permutation(locales.begin(), locales.end()));
}

std::string make_locales_string(const std::vector<std::string_view>& locales) {
  std::string locales_string;
  for (std::string_view locale : locales) {
    locales_string += locale;
    locales_string += '\0';
  }
  return locales_string;
}

TEST(Test_Locale, exact_match_locale) {
  for_each_locale_permutation(
      {
          "fr_FR"sv,
          "en@slang"sv,
      },
      [](const std::vector<std::string_view>& locales) {
        std::string locales_string = make_locales_string(locales);
        std::optional<int> fr_index =
            find_locale(locales_string.c_str(), "fr_FR");
        EXPECT_EQ(locales.at(narrow_cast<std::size_t>(fr_index.value())),
                  "fr_FR");
        std::optional<int> en_index =
            find_locale(locales_string.c_str(), "en@slang");
        EXPECT_EQ(locales.at(narrow_cast<std::size_t>(en_index.value())),
                  "en@slang");
      });
}

TEST(Test_Locale, no_match) {
  for_each_locale_permutation(
      {
          "fr_FR"sv,
          "en@slang"sv,
      },
      [](const std::vector<std::string_view>& locales) {
        std::string locales_string = make_locales_string(locales);
        EXPECT_EQ(find_locale(locales_string.c_str(), "de_DE@a"), std::nullopt);
      });
}

TEST(Test_Locale, match_subset_of_locale_name) {
  for_each_locale_permutation(
      {
          "fr"sv,
          "en"sv,
      },
      [](const std::vector<std::string_view>& locales) {
        std::string locales_string = make_locales_string(locales);
        std::optional<int> fr_index =
            find_locale(locales_string.c_str(), "fr_FR.utf8@bon");
        EXPECT_EQ(locales.at(narrow_cast<std::size_t>(fr_index.value())), "fr");
      });
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
