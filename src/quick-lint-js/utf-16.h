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

#ifndef QUICK_LINT_JS_UTF_16_H
#define QUICK_LINT_JS_UTF_16_H

#include <optional>
#include <string>
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
#endif
}
#endif
