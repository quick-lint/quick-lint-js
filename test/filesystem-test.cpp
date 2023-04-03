// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <exception>
#include <limits.h>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/util/math-overflow.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <quick-lint-js/util/utf-16.h>
#include <string>
#include <string_view>

#if QLJS_HAVE_DIRENT_H
#include <dirent.h>
#endif

#if QLJS_HAVE_FTS_H
#include <fts.h>
#endif

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if QLJS_HAVE_MKDTEMP
#include <sys/stat.h>
#endif

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_FTS_H
void delete_directory_recursive(const std::string &path) {
  char *paths[] = {const_cast<char *>(path.c_str()), nullptr};
  ::FTS *fts = ::fts_open(paths, FTS_PHYSICAL | FTS_XDEV, nullptr);
  if (!fts) {
    std::fprintf(stderr, "fatal: fts_open failed to open %s: %s\n",
                 path.c_str(), std::strerror(errno));
    std::abort();
  }
  while (::FTSENT *entry = ::fts_read(fts)) {
    switch (entry->fts_info) {
    case FTS_D: {
      // Make sure the directory is traversable before traversing.
      int rc = ::chmod(entry->fts_accpath, 0700);
      if (rc != 0) {
        std::fprintf(stderr,
                     "warning: failed to change permissions for %s: %s\n",
                     entry->fts_accpath, std::strerror(errno));
      }
      break;
    }

    case FTS_DP: {
      int rc = ::rmdir(entry->fts_accpath);
      if (rc != 0) {
        std::fprintf(stderr, "warning: failed to delete %s: %s\n",
                     entry->fts_accpath, std::strerror(errno));
      }
      break;
    }

    case FTS_F:
    case FTS_SL:
    case FTS_SLNONE:
    case FTS_DEFAULT: {
      int rc = ::unlink(entry->fts_accpath);
      if (rc != 0) {
        std::fprintf(stderr, "warning: failed to delete %s: %s\n",
                     entry->fts_accpath, std::strerror(errno));
      }
      break;
    }

    case FTS_DNR:
    case FTS_ERR:
    case FTS_NS:
      std::fprintf(stderr, "fatal: fts_read failed to read %s: %s\n",
                   entry->fts_accpath, std::strerror(entry->fts_errno));
      std::abort();
      break;

    case FTS_DC:
    case FTS_DOT:
    case FTS_NSOK:
      QLJS_UNREACHABLE();
      break;
    }
  }
  ::fts_close(fts);
}
#elif QLJS_HAVE_STD_FILESYSTEM
void delete_directory_recursive(const std::string &path) {
  std::filesystem::remove_all(std::filesystem::path(path));
}
#endif

void set_current_working_directory(const char *path) {
#if QLJS_HAVE_STD_FILESYSTEM
  std::filesystem::current_path(path);
#else
  if (::chdir(path) != 0) {
    std::fprintf(stderr, "error: failed to set current directory to %s: %s\n",
                 path, std::strerror(errno));
    std::terminate();
  }
#endif
}

std::vector<std::string> list_files_in_directory(const std::string &directory) {
#if QLJS_HAVE_STD_FILESYSTEM
  std::vector<std::string> result;
  for (auto &entry : std::filesystem::directory_iterator(directory)) {
    result.push_back(entry.path().filename().string());
  }
  return result;
#elif QLJS_HAVE_DIRENT_H
  std::vector<std::string> result;
  ::DIR *d = ::opendir(directory.c_str());
  for (;;) {
    errno = 0;
    ::dirent *entry = ::readdir(d);
    if (!entry) {
      if (errno != 0) {
        std::fprintf(stderr, "fatal: failed to read directory %s: %s\n",
                     directory.c_str(), std::strerror(errno));
        std::abort();
      }
      break;
    }
    bool is_dot_or_dot_dot =
        entry->d_name[0] == '.' &&
        (entry->d_name[1] == '\0' ||
         (entry->d_name[1] == '.' && entry->d_name[2] == '\0'));
    if (!is_dot_or_dot_dot) {
      result.emplace_back(entry->d_name);
    }
  }
  ::closedir(d);
  return result;
#else
#error "Unsupported platform"
#endif
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
