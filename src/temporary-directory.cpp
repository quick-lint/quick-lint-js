// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <exception>
#include <limits.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-path.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/temporary-directory.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/utf-16.h>
#include <random>
#include <string>
#include <string_view>

#if QLJS_HAVE_FTS_H
#include <fts.h>
#endif

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

#if QLJS_HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if QLJS_HAVE_MKDTEMP
#include <sys/stat.h>
#endif

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace quick_lint_js {
std::string create_directory_io_error::to_string() const {
  return this->io_error.to_string();
}

#if QLJS_HAVE_MKDTEMP
result<void, platform_file_io_error> make_unique_directory(std::string &path) {
  path += ".XXXXXX";
  if (!::mkdtemp(path.data())) {
    return result<void, platform_file_io_error>::failure(
        platform_file_io_error{.error = errno});
  }
  return {};
}
#elif QLJS_HAVE_STD_FILESYSTEM
result<void, platform_file_io_error> make_unique_directory(std::string &path) {
  std::string_view characters = "abcdefghijklmnopqrstuvwxyz";
  std::uniform_int_distribution<std::size_t> character_index_distribution(
      0, characters.size() - 1);

  std::random_device system_rng;
  std::mt19937 rng(/*seed=*/system_rng());
  std::error_code error;

  for (int attempt = 0; attempt < 100; ++attempt) {
    std::string suffix = ".";
    for (int i = 0; i < 10; ++i) {
      suffix += characters[character_index_distribution(rng)];
    }

    std::filesystem::path directory_path = path + suffix;
    if (!std::filesystem::create_directory(directory_path, error)) {
      continue;
    }

    path += suffix;
    return {};
  }

  // TODO(strager): Return the proper error code from 'error'.
  return result<void, platform_file_io_error>::failure(platform_file_io_error{
      .error = 0,
  });
}
#else
#error "Unsupported platform"
#endif

#if QLJS_HAVE_MKDTEMP
std::string make_temporary_directory() {
  std::string temp_directory_name = "/tmp/quick-lint-js";
  auto result = make_unique_directory(temp_directory_name);
  if (!result.ok()) {
    std::fprintf(stderr, "failed to create temporary directory: %s\n",
                 result.error_to_string().c_str());
    std::abort();
  }
  return temp_directory_name;
}
#elif QLJS_HAVE_STD_FILESYSTEM
std::string make_temporary_directory() {
  std::filesystem::path system_temp_dir_path =
      std::filesystem::temp_directory_path();
  std::string temp_directory_name =
      (system_temp_dir_path / "quick-lint-js").string();
  auto result = make_unique_directory(temp_directory_name);
  if (!result.ok()) {
    std::fprintf(stderr, "failed to create temporary directory: %s\n",
                 result.error_to_string().c_str());
    std::abort();
  }
  return temp_directory_name;
}
#else
#error "Unsupported platform"
#endif

result<void, create_directory_io_error> create_directory(
    const std::string &path) {
#if QLJS_HAVE_WINDOWS_H
  std::optional<std::wstring> wpath = mbstring_to_wstring(path.c_str());
  if (!wpath.has_value()) {
    QLJS_UNIMPLEMENTED();
  }
  if (!::CreateDirectoryW(wpath->c_str(), /*lpSecurityAttributes=*/nullptr)) {
    ::DWORD error = ::GetLastError();
    bool directory_existed = false;
    if (error == ERROR_ALREADY_EXISTS) {
      ::DWORD attributes = ::GetFileAttributesW(wpath->c_str());
      if (attributes != INVALID_FILE_ATTRIBUTES) {
        directory_existed = attributes & FILE_ATTRIBUTE_DIRECTORY;
      }
    }
    return result<void, create_directory_io_error>::failure(
        create_directory_io_error{
            .io_error =
                windows_file_io_error{
                    .error = error,
                },
            .is_directory_already_exists_error = directory_existed,
        });
  }
  return {};
#elif QLJS_HAVE_FCNTL_H
  if (::mkdir(path.c_str(), 0755) != 0) {
    int error = errno;
    bool directory_existed = false;
    if (error == EEXIST) {
      struct ::stat s;
      if (::lstat(path.c_str(), &s) == 0) {
        directory_existed = S_ISDIR(s.st_mode);
      }
    }
    return result<void, create_directory_io_error>::failure(
        create_directory_io_error{
            .io_error =
                posix_file_io_error{
                    .error = error,
                },
            .is_directory_already_exists_error = directory_existed,
        });
  }
  return {};
#elif QLJS_HAVE_STD_FILESYSTEM
  std::error_code error;
  if (!std::filesystem::create_directory(to_string8(path), error)) {
    // TODO(strager): Return the proper error code from 'error'.
    return result<void, create_directory_io_error>::failure(
        create_directory_io_error{
            .io_error =
                platform_file_io_error{
                    .error = 0,
                },
        });
  }
  return {};
#else
#error "Unsupported platform"
#endif
}

void create_directory_or_exit(const std::string &path) {
  auto result = create_directory(path);
  if (!result.ok()) {
    std::fprintf(stderr, "error: failed to create directory %s: %s\n",
                 path.c_str(), result.error_to_string().c_str());
    std::terminate();
  }
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

result<std::string, platform_file_io_error> make_timestamped_directory(
    std::string_view parent_directory, const char *format) {
  std::time_t now = std::time(nullptr);
  std::tm *now_tm = std::localtime(&now);

  std::string directory(parent_directory);
  directory += QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR;
  std::size_t name_begin_index = directory.size();
  std::size_t name_size = 1;
retry:
  directory.resize(name_begin_index + name_size + 1);
  std::size_t actual_name_size = std::strftime(&directory[name_begin_index],
                                               name_size + 1, format, now_tm);
  if (actual_name_size == 0) {
    name_size *= 2;
    goto retry;
  }
  directory.resize(name_begin_index + actual_name_size);

  auto result = make_unique_directory(directory);
  if (!result.ok()) {
    return result.propagate();
  }
  return directory;
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
