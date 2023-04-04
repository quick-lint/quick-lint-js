// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/assert.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/utf-16.h>

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#include <pathcch.h>
#endif

#if QLJS_HAVE_DIRNAME
#include <libgen.h>
#endif

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
void remove_trailing_slashes(std::string_view& path) {
  std::size_t end_index =
      path.find_last_not_of(QLJS_ALL_PATH_DIRECTORY_SEPARATORS);
  if (end_index != std::string_view::npos) {
    path = path.substr(0, end_index + 1);
  }
}

#if QLJS_HAVE_WINDOWS_H
std::wstring wide_path_with_backslashes(const std::string& path) {
  std::optional<std::wstring> wpath = mbstring_to_wstring(path.c_str());
  if (!wpath.has_value()) {
    QLJS_UNIMPLEMENTED();
  }

  // The PathCch functions only support '\' as a directory separator. Convert
  // all '/'s into '\'s.
  for (wchar_t& c : *wpath) {
    if (c == L'/') {
      c = L'\\';
    }
  }

  return std::move(*wpath);
}

void safely_remove_trailing_backslashes(std::wstring& path) {
remove_backslash:
  HRESULT result = ::PathCchRemoveBackslash(path.data(), path.size() + 1);
  switch (result) {
  case S_OK:
    // PathCchRemoveBackslash removes only one backslash. Make sure we remove
    // them all.
    goto remove_backslash;
  case S_FALSE:
    // Path is a root path, or no backslashes needed removal.
    break;
  case HRESULT_FROM_WIN32(ERROR_INVALID_PARAMETER):
    // Path is invalid.
    QLJS_UNIMPLEMENTED();
    break;
  default:
    QLJS_UNIMPLEMENTED();
    break;
  }
  path.resize(std::wcslen(path.data()));
}
#endif
}

std::string parent_path(std::string&& path) {
#if QLJS_HAVE_DIRNAME
  return ::dirname(path.data());
#elif QLJS_HAVE_WINDOWS_H
  HRESULT result;

  if (path == R"(\\?\)"sv || path == R"(\\?)"sv) {
    // Invalid path. Leave as-is.
    return path;
  }

  std::wstring wpath = wide_path_with_backslashes(path);
  safely_remove_trailing_backslashes(wpath);

  result = ::PathCchRemoveFileSpec(wpath.data(), wpath.size() + 1);
  switch (result) {
  case S_OK:
    break;
  case S_FALSE:
    // Path is a root path already.
    break;
  case HRESULT_FROM_WIN32(ERROR_INVALID_PARAMETER):
    // Path is invalid.
    QLJS_UNIMPLEMENTED();
    break;
  default:
    QLJS_UNIMPLEMENTED();
    break;
  }

  wpath.resize(std::wcslen(wpath.data()));
  if (wpath.empty()) {
    return ".";
  }

  // Convert '\' back into '/' if necessary.
  std::string result_with_backslashes = std::filesystem::path(wpath).string();
#if !(defined(NDEBUG) && NDEBUG)
  {
    std::string path_with_backslashes = path;
    for (char& c : path_with_backslashes) {
      if (c == '/') {
        c = '\\';
      }
    }
    QLJS_ALWAYS_ASSERT(
        path_with_backslashes.starts_with(result_with_backslashes));
  }
#endif

  return path.substr(0, result_with_backslashes.size());
#endif
}

std::string_view path_file_name(std::string_view path) {
#if QLJS_HAVE_WINDOWS_H
  {
    if (path == R"(\\?\)"sv || path == R"(\\?)"sv) {
      // Invalid path.
      return ""sv;
    }

    // Remove a drive (such as "C:") if present.
    if (path.size() >= 2 && path[1] == ':' &&
        (('a' <= path[0] && path[0] <= 'z') ||
         ('A' <= path[0] && path[0] <= 'Z'))) {
      path.remove_prefix(2);
    }

    // TODO(strager): Avoid this std::string copy.
    std::wstring temp_path = wide_path_with_backslashes(std::string(path));
    safely_remove_trailing_backslashes(temp_path);
    if (::PathCchIsRoot(temp_path.c_str())) {
      return ""sv;
    }
  }
#endif

  remove_trailing_slashes(path);

  std::size_t last_slash_index =
      path.find_last_of(QLJS_ALL_PATH_DIRECTORY_SEPARATORS);
  if (last_slash_index == std::string_view::npos) {
    return path;
  }
  return path.substr(last_slash_index + 1);
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
