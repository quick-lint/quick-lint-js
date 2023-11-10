// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/windows-error.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/utf-16.h>
#include <string>
#include <string_view>
#include <vector>

#if defined(_WIN32)
#include <quick-lint-js/port/windows.h>
#endif

namespace quick_lint_js {
#if defined(_WIN32)
MBArgv::MBArgv(int argc, wchar_t **wargv) {
  this->wargv_to_mbargv(argc, wargv);
}

MBArgv::~MBArgv() {
  for (char *mbarg : this->mbargv_) {
    delete[] mbarg;
  }
}

char **MBArgv::data() { return this->mbargv_.data(); }
int MBArgv::size() { return narrow_cast<int>(this->mbargv_.size()); }

void MBArgv::wargv_to_mbargv(int argc, wchar_t **wargv) {
  for (size_t i = 0; i < argc; i++) {
    this->mbargv_.emplace_back(warg_to_mbarg(wargv[i]));
  }
}

char *MBArgv::warg_to_mbarg(wchar_t *warg) {
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

void MBArgv::conversion_failed(wchar_t *warg) {
  std::fprintf(stderr, "error: failed to convert %Ls to mbstring\n", warg);
  std::fprintf(stderr, "%s\n", windows_last_error_message().c_str());
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

std::optional<std::string> wstring_to_mbstring(std::wstring_view wstring) {
  if (wstring.empty()) {
    return std::string();
  }
  int wstring_size = narrow_cast<int>(wstring.size());
  int size_required = WideCharToMultiByte(
      /*CodePage=*/CP_UTF8,
      /*dwFlags=*/0,
      /*lpWideCharStr=*/wstring.data(),
      /*cchWideChar=*/wstring_size,
      /*lpMultiByteStr=*/nullptr,
      /*cbMultiByte=*/0,
      /*lpDefaultChar=*/NULL,
      /*lpUsedDefaultChar=*/NULL);
  if (size_required == 0) {
    return std::nullopt;
  }
  std::string result;
  result.resize(narrow_cast<std::size_t>(size_required));
  int bytes_written = WideCharToMultiByte(
      /*CodePage=*/CP_UTF8,
      /*dwFlags=*/0,
      /*lpWideCharStr=*/wstring.data(),
      /*cchWideChar=*/wstring_size,
      /*lpMultiByteStr=*/result.data(),
      /*cbMultiByte=*/size_required,
      /*lpDefaultChar=*/NULL,
      /*lpUsedDefaultChar=*/NULL);
  if (bytes_written == 0 || bytes_written != size_required) {
    return std::nullopt;
  }
  return result;
}
#endif

std::size_t count_utf_8_code_units(std::u16string_view utf_16) {
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
std::size_t count_utf_8_code_units(std::wstring_view utf_16) {
  static_assert(sizeof(char16_t) == sizeof(wchar_t));
  return count_utf_8_code_units(std::u16string_view(
      reinterpret_cast<const char16_t *>(utf_16.data()), utf_16.size()));
}
#endif

String8 utf_16_to_utf_8(std::u16string_view s) {
  String8 result;
  result.reserve(s.size());
  auto end = s.end();
  auto it = s.begin();
  while (it != end) {
    char16_t c = *it;
    if (0xd800 <= c && c <= 0xdbff) {
      // Surrogate pair.
      char16_t c2 = it + 1 == end ? 0 : it[1];
      if (0xdc00 <= c2 && c2 <= 0xdfff) {
        char32_t cp = 0x1'0000 +
                      ((static_cast<char32_t>(c & 0x3ff) << 10) | (c2 & 0x3ff));
        // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
        result += static_cast<Char8>(((cp >> (3 * 6)) & 0x07) | 0b1111'0000);
        result += static_cast<Char8>(((cp >> (2 * 6)) & 0x3f) | 0b1000'0000);
        result += static_cast<Char8>(((cp >> (1 * 6)) & 0x3f) | 0b1000'0000);
        result += static_cast<Char8>(((cp >> (0 * 6)) & 0x3f) | 0b1000'0000);
        it += 2;
      } else {
        // Incomplete surrogate pair (invalid).
        goto three_byte_output;
      }
    } else if (0xdc00 <= c && c <= 0xdfff) {
      // Second half of a surrogate pair (invalid).
      goto three_byte_output;
    } else if (c >= 0x0800) {
    three_byte_output:
      // 1110xxxx 10xxxxxx 10xxxxxx
      result += static_cast<Char8>(((c >> (2 * 6)) & 0x0f) | 0b1110'0000);
      result += static_cast<Char8>(((c >> (1 * 6)) & 0x3f) | 0b1000'0000);
      result += static_cast<Char8>(((c >> (0 * 6)) & 0x3f) | 0b1000'0000);
      it += 1;
    } else if (c >= 0x0080) {
      // 110xxxxx 10xxxxxx
      result += static_cast<Char8>(((c >> 6) & 0x1f) | 0b1100'0000);
      result += static_cast<Char8>(((c >> 0) & 0x3f) | 0b1000'0000);
      it += 1;
    } else {
      result += narrow_cast<Char8>(c);
      it += 1;
    }
  }
  return result;
}
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
