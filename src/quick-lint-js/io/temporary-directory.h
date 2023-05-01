// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_IO_TEMPORARY_DIRECTORY_H
#define QUICK_LINT_JS_IO_TEMPORARY_DIRECTORY_H

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <quick-lint-js/container/result.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/port/function-ref.h>
#include <quick-lint-js/port/have.h>
#include <string>
#include <string_view>

namespace quick_lint_js {
struct create_directory_io_error {
  platform_file_io_error io_error;
  bool is_directory_already_exists_error;

  std::string to_string() const;
};

result<void, platform_file_io_error> make_unique_directory(std::string& path);

// Crashes on failure.
std::string make_temporary_directory();

result<void, create_directory_io_error> create_directory(
    const std::string& path);

// Crashes on failure.
void create_directory_or_exit(const std::string& path);

// format is a std::strftime format string.
result<std::string, platform_file_io_error> make_timestamped_directory(
    std::string_view parent_directory, const char* format);

// Call visit_file for each child of the given directory.
//
// '.' and '..' are excluded.
//
// visit_file is called with the name (not full path) of the child.
result<void, platform_file_io_error> list_directory(
    const char* directory, function_ref<void(const char*)> visit_file);
result<void, platform_file_io_error> list_directory(
    const char* directory,
    function_ref<void(const char*, bool is_directory)> visit_file);

// Call visit_file for each regular file of the given directory and its
// descendant directories and their descendants, etc.
//
// '.' and '..' entries are excluded.
//
// visit_file is called with the full path of the file, including 'directory'.
void list_directory_recursively(
    const char* directory, function_ref<void(const std::string&)> visit_file,
    function_ref<void(const platform_file_io_error&, int depth)> on_error);

result<std::string, platform_file_io_error> get_current_working_directory();
result<void, platform_file_io_error> get_current_working_directory(
    std::string& out);
#if defined(_WIN32)
result<void, platform_file_io_error> get_current_working_directory(
    std::wstring& out);
#endif

void set_current_working_directory_or_exit(const char* path);
}

#endif

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
