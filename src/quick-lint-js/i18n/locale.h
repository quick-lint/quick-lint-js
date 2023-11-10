// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstring>
#include <optional>
#include <quick-lint-js/port/function-ref.h>
#include <string_view>

namespace quick_lint_js {
// Returns the index of the matching locale.
//
// If locales is "en_US\0fr_FR\0de_DE\0", and locale_name is "fr_FR", then the
// result will be 1.
std::optional<int> find_locale(const char* locales,
                               std::string_view locale_name);

// For testing only.
void enumerate_locale_name_combinations(
    std::string_view locale_name,
    Temporary_Function_Ref<bool(std::string_view locale)>);
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
