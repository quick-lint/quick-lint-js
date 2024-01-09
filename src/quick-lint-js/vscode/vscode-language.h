// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <iterator>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/classify-path.h>
#include <string_view>

namespace quick_lint_js {
// See also LSP_Language.
struct VSCode_Language {
  constexpr VSCode_Language(std::string_view language_id,
                            File_Language language)
      : language(language) {
    quick_lint_js::copy(language_id.begin(), language_id.end(),
                        this->raw_language_id);
    this->language_id_size = static_cast<unsigned char>(language_id.size());
  }

  std::string_view language_id() const {
    return std::string_view(this->raw_language_id, this->language_id_size);
  }

  // Returns nullptr if the language does not exist.
  // TODO(#690): Remove 'allow_typescript'. It should always be true.
  static const VSCode_Language* find(std::string_view language_id,
                                     String8_View uri, bool allow_typescript) {
    using namespace std::literals::string_view_literals;

    static constexpr VSCode_Language ts_definition(
        "typescript"sv, File_Language::typescript_definition);
    static constexpr VSCode_Language languages[] = {
        VSCode_Language("javascript"sv, File_Language::javascript_jsx),
        VSCode_Language("javascriptreact"sv, File_Language::javascript_jsx),

        VSCode_Language("typescript"sv, File_Language::typescript),
        VSCode_Language("typescriptreact"sv, File_Language::typescript_jsx),
    };
    const VSCode_Language* lang =
        find_unique_if(std::begin(languages), std::end(languages),
                       [&](const VSCode_Language& l) {
                         return l.language_id() == language_id;
                       });
    if (lang == std::end(languages)) {
      return nullptr;
    }
    if ((lang->language == File_Language::typescript ||
         lang->language == File_Language::typescript_jsx ||
         lang->language == File_Language::typescript_definition) &&
        !allow_typescript) {
      return nullptr;
    }
    Path_Classification classified_uri = classify_uri(uri);
    if (classified_uri.typescript_definition) {
      return &ts_definition;
    }
    return lang;
  }

  char raw_language_id[16] = {};
  unsigned char language_id_size = 0;
  File_Language language;
};
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
