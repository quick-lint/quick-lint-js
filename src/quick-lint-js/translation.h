// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TRANSLATION_H
#define QUICK_LINT_JS_TRANSLATION_H

#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/gmo.h>
#include <quick-lint-js/locale.h>
#include <string>
#include <vector>

#define QLJS_TRANSLATE(...) \
  (::quick_lint_js::translate(__VA_ARGS__##_gmo_message))

#define QLJS_TRANSLATABLE(...) (__VA_ARGS__##_gmo_message)

namespace quick_lint_js {
const char8* translate(const gmo_message&);

void initialize_translations_from_environment();
void initialize_translations_from_locale(const char* locale_name);

class translatable_messages {
 public:
  void use_messages_from_source_code();
  bool use_messages_from_locale(
      const char* locale_name,
      const locale_entry<const std::uint8_t*>* gmo_files);
  bool use_messages_from_locales(
      const std::vector<std::string>& locale_names,
      const locale_entry<const std::uint8_t*>* gmo_files);

  const char* translate(const gmo_message& message);

 private:
  std::optional<gmo_file> translation_;
};
}

#endif

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
