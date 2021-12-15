// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <optional>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/utf-16.h>
#include <string>
#include <string_view>
#include <vector>

#if defined(_WIN32)
#include <Windows.h>
#endif

namespace quick_lint_js {
#if defined(_WIN32)
mbargv::mbargv(int argc, wchar_t **wargv) {
  this->wargv_to_mbargv(argc, wargv);
}

mbargv::~mbargv() {
  for (char *mbarg : this->mbargv_) {
    delete[] mbarg;
  }
}

char **mbargv::data() { return this->mbargv_.data(); }
int mbargv::size() { return narrow_cast<int>(this->mbargv_.size()); }

void mbargv::wargv_to_mbargv(int argc, wchar_t **wargv) {
  for (size_t i = 0; i < argc; i++) {
    this->mbargv_.emplace_back(warg_to_mbarg(wargv[i]));
  }
}

char *mbargv::warg_to_mbarg(wchar_t *warg) {
  int size_required = WideCharToMultiByte(
      /*CodePage=*/CP_UTF8,
      /*dwFlags=*/0,
      /*lpWideCharStr=*/warg,
      /*cchWideChar=*/-1,
      /*lpMultiByteStr=*/nullptr,
      /*cbMultiByte=*/0,
      /*lpDefaultChar=*/NULL,
      /*lpUsedDefaultChar=*/NULL);
  if (size_required == 0) {
    this->conversion_failed(warg);
  }
  char *mbarg = new char[size_required];
  int bytes_written = WideCharToMultiByte(
      /*CodePage=*/CP_UTF8,
      /*dwFlags=*/0,
      /*lpWideCharStr=*/warg,
      /*cchWideChar=*/-1,
      /*lpMultiByteStr=*/mbarg,
      /*cbMultiByte=*/size_required,
      /*lpDefaultChar=*/NULL,
      /*lpUsedDefaultChar=*/NULL);
  if (bytes_written == 0 || bytes_written != size_required) {
    this->conversion_failed(warg);
  }
  return mbarg;
}

void mbargv::conversion_failed(wchar_t *warg) {
  std::fprintf(stderr, "error: failed to convert %Ls to mbstring\n", warg);
  std::fprintf(stderr, "%s\n", windows_error_message(GetLastError()).c_str());
  std::exit(EXIT_FAILURE);
}

std::optional<std::wstring> mbstring_to_wstring(const char *mbstring) {
  int mbstring_size = narrow_cast<int>(std::strlen(mbstring));
  if (mbstring_size == 0) {
    return L"";
  }
  int size_required = MultiByteToWideChar(
      /*CodePage=*/CP_UTF8,
      /*dwFlags=*/0,
      /*lpMultiByteStr=*/mbstring,
      /*cbMultiByte=*/mbstring_size,
      /*lpWideCharStr=*/nullptr,
      /*cchWideChar=*/0);
  if (size_required == 0) {
    return std::nullopt;
  }
  std::wstring wstring;
  wstring.resize(size_required);
  int bytes_written = MultiByteToWideChar(
      /*CodePage=*/CP_UTF8,
      /*dwFlags=*/0,
      /*lpMultiByteStr=*/mbstring,
      /*cbMultiByte=*/mbstring_size,
      /*lpWideCharStr=*/wstring.data(),
      /*cchWideChar=*/size_required);
  if (bytes_written == 0 || bytes_written != size_required) {
    return std::nullopt;
  }
  return wstring;
}
#endif

std::size_t count_utf_8_code_units(std::u16string_view utf_16) noexcept {
  std::size_t count = 0;
  for (char16_t c : utf_16) {
    if (c < 0x0080) {
      count += 1;
    } else if (c < 0x0800) {
      count += 2;
    } else if (0xd800 <= c && c <= 0xdfff) {
      // Part of a surrogate pair.
      count += 2;
    } else {
      count += 3;
    }
  }
  return count;
}

#if defined(_WIN32)
std::size_t count_utf_8_code_units(std::wstring_view utf_16) noexcept {
  static_assert(sizeof(char16_t) == sizeof(wchar_t));
  return count_utf_8_code_units(std::u16string_view(
      reinterpret_cast<const char16_t *>(utf_16.data()), utf_16.size()));
}
#endif
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
