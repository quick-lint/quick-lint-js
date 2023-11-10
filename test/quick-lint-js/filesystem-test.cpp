// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/windows-error.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/math-overflow.h>
#include <quick-lint-js/util/utf-16.h>
#include <string>

#if QLJS_HAVE_DIRECT_H
#include <direct.h>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace quick_lint_js {
void delete_directory_recursive(const std::string& path) {
  struct Delete_Visitor : public List_Directory_Visitor {
    void visit_file(const std::string& path) override {
#if QLJS_HAVE_UNISTD_H
      int rc = std::remove(path.c_str());
      if (rc != 0) {
        std::fprintf(stderr, "warning: failed to delete %s: %s\n", path.c_str(),
                     std::strerror(errno));
      }
#elif defined(_WIN32)
      std::optional<std::wstring> wpath = mbstring_to_wstring(path.c_str());
      if (!wpath.has_value()) {
        std::fprintf(stderr,
                     "warning: failed to delete %s: cannot convert to UTF-16\n",
                     path.c_str());
        return;
      }
      if (!::DeleteFileW(wpath->c_str())) {
        std::fprintf(stderr, "warning: failed to delete %s: %s\n", path.c_str(),
                     windows_error_message(::GetLastError()).c_str());
      }
#else
#error "Unsupported platform"
#endif
    }

    void visit_directory_pre([
        [maybe_unused]] const std::string& path) override {
#if QLJS_HAVE_UNISTD_H
      // Make sure the directory is traversable before traversing.
      int rc = ::chmod(path.c_str(), 0700);
      if (rc != 0) {
        std::fprintf(stderr,
                     "warning: failed to change permissions for %s: %s\n",
                     path.c_str(), std::strerror(errno));
      }
#endif
    }

    void visit_directory_post(const std::string& path) override {
#if QLJS_HAVE_UNISTD_H
      int rc = ::rmdir(path.c_str());
      if (rc != 0) {
        std::fprintf(stderr, "warning: failed to delete %s: %s\n", path.c_str(),
                     std::strerror(errno));
      }
#elif defined(_WIN32)
      std::optional<std::wstring> wpath = mbstring_to_wstring(path.c_str());
      if (!wpath.has_value()) {
        std::fprintf(stderr,
                     "warning: failed to delete %s: cannot convert to UTF-16\n",
                     path.c_str());
        return;
      }
      if (!::RemoveDirectoryW(wpath->c_str())) {
        std::fprintf(stderr, "warning: failed to delete %s: %s\n", path.c_str(),
                     windows_error_message(::GetLastError()).c_str());
      }
#else
#error "Unsupported platform"
#endif
    }

    void on_error(const Platform_File_IO_Error& error,
                  [[maybe_unused]] int depth) override {
      std::fprintf(stderr, "fatal: %s\n", error.to_string().c_str());
      std::abort();
    }
  };
  Delete_Visitor visitor;
  list_directory_recursively(path.c_str(), visitor);
}

std::vector<std::string> list_files_in_directory(const std::string& directory) {
  std::vector<std::string> files;
  Result<void, Platform_File_IO_Error> error =
      list_directory(directory.c_str(),
                     [&](const char* name) -> void { files.push_back(name); });
  if (!error.ok()) {
    std::fprintf(stderr, "fatal: failed to read directory %s: %s\n",
                 directory.c_str(), std::strerror(errno));
    std::abort();
  }
  return files;
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
