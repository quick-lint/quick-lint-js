// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <iterator>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/uri.h>
#include <string_view>

namespace quick_lint_js {
// See also VSCode_Language.
struct LSP_Language {
  struct typescript_autodetect_tag {};

  constexpr LSP_Language(std::string_view language_id,
                         Linter_Options lint_options)
      : lint_options(lint_options) {
    quick_lint_js::copy(language_id.begin(), language_id.end(),
                        this->raw_language_id);
    this->language_id_size = static_cast<unsigned char>(language_id.size());
  }

  constexpr LSP_Language(std::string_view language_id,
                         Linter_Options lint_options, typescript_autodetect_tag)
      : LSP_Language(language_id, lint_options) {
    this->typescript_autodetect = true;
  }

  std::string_view language_id() const {
    return std::string_view(this->raw_language_id, this->language_id_size);
  }

  // Returns nullptr if the language does not exist.
  static const LSP_Language* find(std::string_view language_id,
                                  String8_View uri) {
    using namespace std::literals::string_view_literals;

    static constexpr Linter_Options jsx = {
        .jsx = true,
        .typescript = false,
        .typescript_definition = false,
        .print_parser_visits = false,
    };
    static constexpr Linter_Options ts = {
        .jsx = false,
        .typescript = true,
        .typescript_definition = false,
        .print_parser_visits = false,
    };
    static constexpr Linter_Options ts_definition = {
        .jsx = false,
        .typescript = true,
        .typescript_definition = true,
        .print_parser_visits = false,
    };
    static constexpr Linter_Options tsx = {
        .jsx = true,
        .typescript = true,
        .typescript_definition = false,
        .print_parser_visits = false,
    };
    static constexpr LSP_Language languages[] = {
        // Keep in sync with docs/lsp.adoc.
        LSP_Language("javascript"sv, jsx),
        LSP_Language("javascriptreact"sv, jsx),
        LSP_Language("js"sv, jsx),
        LSP_Language("js-jsx"sv, jsx),

        LSP_Language("typescript"sv, ts, typescript_autodetect_tag()),

        LSP_Language("typescriptdefinition"sv, ts_definition),
        LSP_Language("typescriptsource"sv, ts),

        LSP_Language("tsx"sv, tsx),
        LSP_Language("typescriptreact"sv, tsx),
    };
    const LSP_Language* lang = find_unique_if(
        std::begin(languages), std::end(languages),
        [&](const LSP_Language& l) { return l.language_id() == language_id; });
    if (lang == std::end(languages)) {
      return nullptr;
    }
    if (lang->typescript_autodetect) {
      if (uri_looks_like_typescript_definition(uri)) {
        return &languages[5];
      }
      if (uri_looks_like_typescript_jsx(uri)) {
        return &languages[8];
      }
    }
    return lang;
  }

  char raw_language_id[20] = {};
  unsigned char language_id_size = 0;
  Linter_Options lint_options;
  bool typescript_autodetect = false;
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
