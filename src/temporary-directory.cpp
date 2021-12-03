// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <limits.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/temporary-directory.h>
#include <quick-lint-js/unreachable.h>
#include <random>
#include <string>

#if QLJS_HAVE_FTS_H
#include <fts.h>
#endif

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

#if QLJS_HAVE_MKDTEMP
#include <sys/stat.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace quick_lint_js {
#if QLJS_HAVE_MKDTEMP
std::string make_temporary_directory() {
  std::string temp_directory_name = "/tmp/quick-lint-js.XXXXXX";
  if (!::mkdtemp(temp_directory_name.data())) {
    std::fprintf(stderr, "failed to create temporary directory\n");
    std::abort();
  }
  return temp_directory_name;
}
#elif QLJS_HAVE_STD_FILESYSTEM
std::string make_temporary_directory() {
  std::string_view characters = "abcdefghijklmnopqrstuvwxyz";
  std::uniform_int_distribution<std::size_t> character_index_distribution(
      0, characters.size() - 1);

  std::filesystem::path system_temp_dir_path =
      std::filesystem::temp_directory_path();
  std::random_device system_rng;
  std::mt19937 rng(/*seed=*/system_rng());

  for (int attempt = 0; attempt < 100; ++attempt) {
    std::string file_name = "quick-lint-js.";
    for (int i = 0; i < 10; ++i) {
      file_name += characters[character_index_distribution(rng)];
    }

    std::filesystem::path temp_directory_path =
        system_temp_dir_path / file_name;
    std::error_code error;
    if (!std::filesystem::create_directory(temp_directory_path, error)) {
      continue;
    }
    return temp_directory_path.string();
  }
  std::fprintf(stderr, "failed to create temporary directory\n");
  std::abort();
}
#else
#error "Unsupported platform"
#endif

void create_directory(const std::string &path) {
#if QLJS_HAVE_STD_FILESYSTEM
  std::filesystem::create_directory(to_string8(path));
#else
  if (::mkdir(path.c_str(), 0755) != 0) {
    std::fprintf(stderr, "error: failed to create directory %s: %s\n",
                 path.c_str(), std::strerror(errno));
    std::terminate();
  }
#endif
}

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

std::string get_current_working_directory() {
#if QLJS_HAVE_STD_FILESYSTEM
  return std::filesystem::current_path().string();
#else
  std::string cwd;
  cwd.resize(PATH_MAX);
  if (!::getcwd(cwd.data(), cwd.size() + 1)) {
    std::fprintf(stderr, "error: failed to get current directory: %s\n",
                 std::strerror(errno));
    std::terminate();
  }
  return cwd;
#endif
}

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
