// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

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
struct Create_Directory_IO_Error {
  Platform_File_IO_Error io_error;
  bool is_directory_already_exists_error;

  std::string to_string() const;
};

Result<void, Platform_File_IO_Error> make_unique_directory(std::string& path);

// Crashes on failure.
std::string make_temporary_directory();

Result<void, Create_Directory_IO_Error> create_directory(
    const std::string& path);

// Crashes on failure.
void create_directory_or_exit(const std::string& path);

// format is a std::strftime format string.
Result<std::string, Platform_File_IO_Error> make_timestamped_directory(
    std::string_view parent_directory, const char* format);

// Call visit_file for each child of the given directory.
//
// '.' and '..' are excluded.
//
// visit_file is called with the name (not full path) of the child.
Result<void, Platform_File_IO_Error> list_directory(
    const char* directory, Function_Ref<void(const char*)> visit_file);
Result<void, Platform_File_IO_Error> list_directory(
    const char* directory,
    Function_Ref<void(const char*, bool is_directory)> visit_file);

// See list_directory_recursively.
class List_Directory_Visitor {
 public:
  // visit_file is called with the full path of the file, including
  // 'directory' given to list_directory_recursively.
  //
  // visit_file is not called for '.' or '..' entries.
  virtual void visit_file(const std::string& path) = 0;

  virtual void on_error(const Platform_File_IO_Error& error, int depth) = 0;
};

// Call visit_file for each regular file of the given directory and its
// descendant directories and their descendants, etc.
void list_directory_recursively(const char* directory, List_Directory_Visitor&);

Result<std::string, Platform_File_IO_Error> get_current_working_directory();
Result<void, Platform_File_IO_Error> get_current_working_directory(
    std::string& out);
#if defined(_WIN32)
Result<void, Platform_File_IO_Error> get_current_working_directory(
    std::wstring& out);
#endif

void set_current_working_directory_or_exit(const char* path);
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
