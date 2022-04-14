// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTF_16_H
#define QUICK_LINT_JS_UTF_16_H

#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
#if defined(_WIN32)
class mbargv {
 public:
  explicit mbargv(int argc, wchar_t **wargv);
  mbargv(const mbargv &) = delete;
  mbargv &operator=(const mbargv &) = delete;
  ~mbargv();
  char **data();
  int size();

 private:
  void wargv_to_mbargv(int argc, wchar_t **wargv);
  char *warg_to_mbarg(wchar_t *warg);
  void conversion_failed(wchar_t *warg);

  std::vector<char *> mbargv_;
};

std::optional<std::wstring> mbstring_to_wstring(const char *mbstring);
std::optional<std::string> wstring_to_mbstring(std::wstring_view);
#endif

std::size_t count_utf_8_code_units(std::u16string_view) noexcept;
#if defined(_WIN32)
std::size_t count_utf_8_code_units(std::wstring_view) noexcept;
#endif
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
