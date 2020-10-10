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

#include <array>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/locale.h>
#include <quick-lint-js/narrow-cast.h>
#include <string>
#include <vector>

namespace quick_lint_js {
namespace {
constexpr const char locale_part_separators[] = "_.@";

struct locale_parts {
  // language, territory, codeset, modifier
  std::array<std::string_view, 4> parts;

  static constexpr int language_index = 0;
  static constexpr int territory_index = 1;
  static constexpr int codeset_index = 2;
  static constexpr int modifier_index = 3;

  std::string_view& language() noexcept {
    return this->parts[this->language_index];
  }
};

locale_parts parse_locale(const char* locale_name) {
  struct found_separator {
    std::size_t length;
    std::size_t which_separator;
  };
  auto find_next_separator = [](const char* c,
                                const char* separators) -> found_separator {
    std::size_t length = std::strcspn(c, separators);
    std::size_t which_separator;
    if (c[length] == '\0') {
      which_separator = static_cast<std::size_t>(-1);
    } else {
      const char* separator = std::strchr(separators, c[length]);
      QLJS_ASSERT(separator);
      which_separator = narrow_cast<std::size_t>(separator - separators);
    }
    return found_separator{.length = length,
                           .which_separator = which_separator};
  };

  locale_parts parts;

  const char* current_separators = &locale_part_separators[0];
  std::string_view* current_part = &parts.language();
  const char* c = locale_name;
  for (;;) {
    found_separator part = find_next_separator(c, current_separators);
    *current_part = std::string_view(c, part.length);
    c += part.length;
    if (*c == '\0') {
      break;
    }

    QLJS_ASSERT(part.which_separator != static_cast<std::size_t>(-1));
    current_separators += part.which_separator + 1;
    current_part += part.which_separator + 1;
    c += 1;
  }
  return parts;
}
}

template <class T>
const locale_entry<T>* find_locale_entry(const locale_entry<T>* entries,
                                         const char* locale_name) {
  std::vector<std::string> locale_names_to_search =
      locale_name_combinations(locale_name);
  for (const std::string& current_locale_name : locale_names_to_search) {
    for (const locale_entry<T>* entry = entries; entry->valid(); ++entry) {
      if (entry->has_locale_name(current_locale_name.c_str())) {
        return entry;
      }
    }
  }
  return nullptr;
}

template const locale_entry<const std::uint8_t*>* find_locale_entry(
    const locale_entry<const std::uint8_t*>*, const char*);
template const locale_entry<int>* find_locale_entry(const locale_entry<int>*,
                                                    const char*);

std::vector<std::string> locale_name_combinations(const char* locale_name) {
  locale_parts parts = parse_locale(locale_name);
  enum {
    TERRITORY = 1 << locale_parts::territory_index,
    CODESET = 1 << locale_parts::codeset_index,
    MODIFIER = 1 << locale_parts::modifier_index,
  };
  // clang-format off
  unsigned char masks[] = {
      TERRITORY | CODESET | MODIFIER,
      TERRITORY           | MODIFIER,
                  CODESET | MODIFIER,
                            MODIFIER,
      TERRITORY | CODESET,
      TERRITORY,
                  CODESET,
      0,
  };
  // clang-format on
  std::vector<std::string> results;
  for (unsigned char mask : masks) {
    std::string locale(parts.language());
    for (std::size_t part_index = 1; part_index < 4; ++part_index) {
      if (mask & (1 << part_index)) {
        std::string_view part = parts.parts[part_index];
        if (part.empty()) {
          goto skip;
        }
        locale += locale_part_separators[part_index - 1];
        locale += part;
      }
    }
    results.emplace_back(std::move(locale));
  skip:;
  }
  return results;
}
}
