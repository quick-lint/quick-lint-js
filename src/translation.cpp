// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <clocale>
#include <cstring>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/gmo.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/translation-data.h>
#include <quick-lint-js/translation.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <string_view>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
translatable_messages qljs_messages;

std::vector<std::string> split_on(const char* s, char separator) {
  std::vector<std::string> locales;
  for (;;) {
    const char* sep = std::strchr(s, separator);
    if (sep) {
      locales.emplace_back(s, sep);
      s = sep + 1;
    } else {
      locales.emplace_back(s);
      break;
    }
  }
  return locales;
}

std::vector<std::string> get_user_locale_preferences() {
  // This lookup order roughly mimics GNU gettext.

  int category =
#if QLJS_HAVE_LC_MESSAGES
      LC_MESSAGES
#else
      LC_ALL
#endif
      ;
  const char* locale = std::setlocale(category, nullptr);
  if (locale && locale == "C"sv) {
    return {};
  }

  const char* language_env = ::getenv("LANGUAGE");
  if (language_env && language_env[0] != '\0') {
    return split_on(language_env, ':');
  }

  // TODO(strager): Determine the language using macOS' and Windows' native
  // APIs. See GNU gettext's _nl_language_preferences_default.

  return {locale};
}

void initialize_locale() {
  if (!std::setlocale(LC_ALL, "")) {
    std::fprintf(stderr, "warning: failed to set locale: %s\n",
                 std::strerror(errno));
  }
}
}

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

const char8* translate(const gmo_message& message) {
  const char* translated_message = qljs_messages.translate(message);
  // HACK(strager): Assume message encoding is UTF-8.
  return reinterpret_cast<const char8*>(translated_message);
}

QLJS_WARNING_POP

void initialize_translations_from_environment() {
  initialize_locale();
  if (!qljs_messages.use_messages_from_locales(get_user_locale_preferences(),
                                               gmo_files)) {
    qljs_messages.use_messages_from_source_code();
  }
}

void initialize_translations_from_locale(const char* locale_name) {
  initialize_locale();
  if (!qljs_messages.use_messages_from_locale(locale_name, gmo_files)) {
    qljs_messages.use_messages_from_source_code();
  }
}

void translatable_messages::use_messages_from_source_code() {
  this->translation_ = std::nullopt;
}

bool translatable_messages::use_messages_from_locale(
    const char* locale_name,
    const locale_entry<const std::uint8_t*>* gmo_files) {
  const locale_entry<const std::uint8_t*>* gmo_file_entry =
      find_locale_entry(gmo_files, locale_name);
  if (gmo_file_entry) {
    this->translation_.emplace(gmo_file_entry->data);
    return true;
  }
  return false;
}

bool translatable_messages::use_messages_from_locales(
    const std::vector<std::string>& locale_names,
    const locale_entry<const std::uint8_t*>* gmo_files) {
  for (const std::string& locale : locale_names) {
    if (locale == "C" || locale == "POSIX") {
      // Stop seaching. C/POSIX locale takes priority. See GNU gettext.
      break;
    }
    bool found_messages =
        this->use_messages_from_locale(locale.c_str(), gmo_files);
    if (found_messages) {
      return true;
    }
  }
  return false;
}

const char* translatable_messages::translate(const gmo_message& message) {
  if (this->translation_.has_value()) {
    return this->translation_->find_translation(message).data();
  } else {
    return message.message.data();
  }
}
}

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
