// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <quick-lint-js/locale.h>

using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_locale, combinations_for_language) {
  EXPECT_THAT(locale_name_combinations("en"), ElementsAre("en"));
}

TEST(test_locale, combinations_for_language_with_territory) {
  EXPECT_THAT(locale_name_combinations("fr_FR"), ElementsAre("fr_FR", "fr"));
}

TEST(test_locale, combinations_for_language_with_codeset) {
  EXPECT_THAT(locale_name_combinations("fr.utf8"),
              ElementsAre("fr.utf8", "fr"));
}

TEST(test_locale, combinations_for_language_with_modifier) {
  EXPECT_THAT(locale_name_combinations("fr@bon"), ElementsAre("fr@bon", "fr"));
}

TEST(test_locale, combinations_for_language_with_territory_and_modifier) {
  EXPECT_THAT(locale_name_combinations("fr_FR@bon"),
              ElementsAre("fr_FR@bon", "fr@bon", "fr_FR", "fr"));
}

TEST(test_locale, combinations_for_language_with_territory_and_codeset) {
  EXPECT_THAT(locale_name_combinations("fr_FR.utf8"),
              ElementsAre("fr_FR.utf8", "fr_FR", "fr.utf8", "fr"));
}

TEST(test_locale,
     combinations_for_language_with_territory_and_codeset_and_modifier) {
  EXPECT_THAT(locale_name_combinations("fr_FR.utf8@bon"),
              ElementsAre("fr_FR.utf8@bon", "fr_FR@bon", "fr.utf8@bon",
                          "fr@bon", "fr_FR.utf8", "fr_FR", "fr.utf8", "fr"));
}

TEST(test_locale, modifier_can_contain_underscores_and_at_signs) {
  EXPECT_THAT(locale_name_combinations("fr@a_b@c"),
              ElementsAre("fr@a_b@c", "fr"));
}

template <class T, class Func>
void for_each_permutation(const locale_entry<T>* original_files,
                          Func&& callback) {
  auto order = [](const locale_entry<T>& a, const locale_entry<T>& b) {
    return std::strcmp(a.locale, b.locale) < 0;
  };

  std::vector<locale_entry<T>> files;
  for (const locale_entry<T>* file = original_files; file->valid(); ++file) {
    files.emplace_back(*file);
  }
  std::sort(files.begin(), files.end(), order);

  do {
    const locale_entry<T>* current_files = files.data();
    callback(current_files);
  } while (std::next_permutation(files.begin(), files.end(), order));
}

TEST(test_locale, exact_match_locale) {
  const locale_entry<int> all_entries[] = {
      {"fr_FR", 100},
      {"en@slang", 200},
      {},
  };
  for_each_permutation(all_entries, [](const locale_entry<int>* entries) {
    EXPECT_EQ(find_locale_entry(entries, "fr_FR")->data, 100);
    EXPECT_EQ(find_locale_entry(entries, "en@slang")->data, 200);
  });
}

TEST(test_locale, no_match) {
  const locale_entry<int> entries[] = {
      {"fr_FR", 100},
      {"en@slang", 200},
      {},
  };
  EXPECT_EQ(find_locale_entry(entries, "de_DE@a"), nullptr);
}

TEST(test_locale, match_subset_of_locale_name) {
  const locale_entry<int> entries[] = {
      {"fr", 100},
      {"en", 200},
      {},
  };
  EXPECT_EQ(find_locale_entry(entries, "fr_FR.utf8@bon")->data, 100);
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
