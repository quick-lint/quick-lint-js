// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_I18N_TRANSLATION_H
#define QUICK_LINT_JS_I18N_TRANSLATION_H

#include <cstdint>
#include <quick-lint-js/i18n/locale.h>
#include <quick-lint-js/i18n/translation-table.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/consteval.h>
#include <string>
#include <string_view>
#include <vector>

#define QLJS_TRANSLATABLE(...) (__VA_ARGS__##_translatable)

namespace quick_lint_js {
class translatable_message;

void initialize_translations_from_environment();
void initialize_translations_from_locale(const char* locale_name);

class translator {
 public:
  // Creates a translator which uses messages from the source code (i.e. no-op).
  explicit translator() = default;

  void use_messages_from_source_code();
  bool use_messages_from_locale(const char* locale_name);
  bool use_messages_from_locales(const std::vector<std::string>& locale_names);

  const char8* translate(const translatable_message& message);

 private:
  int locale_index_ = translation_table_locale_count;
};

// Global instance.
extern translator qljs_messages;

// An un-translated message.
class translatable_message {
 public:
  /*implicit*/ constexpr translatable_message()
      : translation_table_mapping_index_(
            translation_table::unallocated_mapping_index) {}

  explicit QLJS_CONSTEVAL translatable_message(const char* raw_message,
                                               int length)
      : translation_table_mapping_index_(
            translation_table::mapping_index_for_untranslated_string(
                std::string_view(raw_message,
                                 static_cast<std::size_t>(length)))) {}

  constexpr bool valid() const noexcept {
    return this->translation_table_mapping_index_ !=
           translation_table::unallocated_mapping_index;
  }

  constexpr std::uint16_t translation_table_mapping_index() const noexcept {
    return this->translation_table_mapping_index_;
  }

 private:
  std::uint16_t translation_table_mapping_index_;
};

inline QLJS_CONSTEVAL translatable_message
operator""_translatable(const char* raw_message, std::size_t length) {
  return translatable_message(raw_message, static_cast<int>(length));
}
}

#endif

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
