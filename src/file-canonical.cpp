// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cerrno>
#include <climits>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-path.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/utf-16.h>
#include <string>

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <Windows.h>
#include <pathcch.h>
#endif

#if defined(_WIN32)
#define QLJS_PATHS_POSIX 0
#define QLJS_PATHS_WIN32 1
#endif

#if defined(_POSIX_VERSION) && _POSIX_VERSION >= 200112L
#define QLJS_PATHS_POSIX 1
#define QLJS_PATHS_WIN32 0
#endif

namespace quick_lint_js {
namespace {
#if QLJS_HAVE_UNISTD_H
int read_symbolic_link(const char *symlink_path, std::string *out);
#endif

#if QLJS_PATHS_POSIX
std::string string_for_error_message(std::string_view);
const std::string &string_for_error_message(const std::string &);
#endif

#if QLJS_PATHS_WIN32
std::string string_for_error_message(std::wstring_view);
#endif
}

canonical_path_result::canonical_path_result() {}

canonical_path_result::canonical_path_result(std::string &&path)
    : path_(std::move(path)) {}

canonical_path_result::canonical_path_result(const char *path) : path_(path) {}

std::string_view canonical_path_result::path() const &noexcept {
  QLJS_ASSERT(this->ok());
  return this->path_;
}

std::string &&canonical_path_result::path() && noexcept {
  QLJS_ASSERT(this->ok());
  return std::move(this->path_);
}

const char *canonical_path_result::c_str() const noexcept {
  QLJS_ASSERT(this->ok());
  return this->path_.c_str();
}

std::string &&canonical_path_result::error() && noexcept {
  QLJS_ASSERT(!this->ok());
  return std::move(this->error_);
}

canonical_path_result canonical_path_result::failure(std::string &&error) {
  canonical_path_result result;
  result.error_ = std::move(error);
  QLJS_ASSERT(!result.ok());
  return result;
}

namespace {
class path_canonicalizer {
 public:
#if QLJS_PATHS_WIN32
  using path_char = wchar_t;
#elif QLJS_PATHS_POSIX
  using path_char = char;
#else
#error "Unsupported platform"
#endif
  using path_string = std::basic_string<path_char>;
  using path_string_view = std::basic_string_view<path_char>;

#if QLJS_PATHS_WIN32
  static constexpr path_char preferred_component_separator = L'\\';
  static constexpr path_char component_separators[] = L"/\\";
  static constexpr path_string_view dot = L".";
  static constexpr path_string_view dot_dot = L"..";
#elif QLJS_PATHS_POSIX
  static constexpr path_char preferred_component_separator = '/';
  static constexpr path_char component_separators[] = "/";
  static constexpr path_string_view dot = ".";
  static constexpr path_string_view dot_dot = "..";
#else
#error "Unsupported platform"
#endif

  explicit path_canonicalizer(path_string_view path) : original_path_(path) {}

  canonical_path_result result() {
    if (!error_.empty()) {
      return canonical_path_result::failure(std::move(error_));
    }
#if QLJS_PATHS_WIN32
    // HACK(strager): Convert UTF-16 to UTF-8.
    return canonical_path_result(std::filesystem::path(canonical_).string());
#else
    return canonical_path_result(std::move(canonical_));
#endif
  }

  void canonicalize() {
    if (original_path_.empty()) {
      error_ = std::string("failed to canonicalize empty path: ") +
               std::strerror(EINVAL);
      return;
    }

    process_start_of_path();
    if (!error_.empty()) return;

    while (!path_to_process_.empty()) {
      process_next_component();
      if (!error_.empty()) return;
    }

    if (need_root_slash_) {
#if QLJS_PATHS_POSIX
      QLJS_ASSERT(canonical_.empty());
#endif
      canonical_ += preferred_component_separator;
    }
  }

 private:
  enum class file_type {
    error,

    directory,
    other,
    symlink,
  };

  void process_start_of_path() {
#if defined(_WIN32)
    std::wstring temp(path_to_process_);

    // The PathCch functions only support '\' as a directory separator. Convert
    // all '/'s into '\'s.
    for (wchar_t &c : temp) {
      if (c == L'/') {
        c = L'\\';
      }
    }

    wchar_t *root_end;
    HRESULT result = ::PathCchSkipRoot(temp.data(), &root_end);
    switch (result) {
    case S_OK:
      // Path is absolute.
      QLJS_ASSERT(root_end != temp.data());

      path_to_process_ = path_to_process_.substr(root_end - temp.data());
      skip_to_next_component();

      // Drop '\' from 'C:\' if present.
      if (root_end[-1] == L'\\') {
        --root_end;
      }
      canonical_.assign(temp.data(), root_end);

      need_root_slash_ = true;
      break;

    case HRESULT_FROM_WIN32(ERROR_INVALID_PARAMETER):
      // Path is invalid or is relative. Assume that it is relative.
      load_cwd();
      break;

    default:
      QLJS_UNIMPLEMENTED();
      break;
    }
#elif QLJS_PATHS_POSIX
    bool is_absolute = !path_to_process_.empty() &&
                       path_to_process_[0] == preferred_component_separator;
    if (is_absolute) {
      path_to_process_ = path_to_process_.substr(1);
      canonical_.clear();
      need_root_slash_ = true;
    } else {
      load_cwd();
    }
#else
#error "Unsupported platform"
#endif
  }

  void load_cwd() {
#if defined(_WIN32)
    // size includes the null terminator.
    DWORD size = ::GetCurrentDirectoryW(0, nullptr);
    if (size == 0) {
      QLJS_UNIMPLEMENTED();
    }
    canonical_.resize(size - 1);
    // length excludes the null terminator.
    DWORD length = ::GetCurrentDirectoryW(size, canonical_.data());
    if (length == 0) {
      QLJS_UNIMPLEMENTED();
    }
    if (length != size - 1) {
      QLJS_UNIMPLEMENTED();
    }

    need_root_slash_ = false;
#elif QLJS_HAVE_UNISTD_H && QLJS_PATHS_POSIX
    // TODO(strager): Is PATH_MAX sufficient? Do we need to keep growing our
    // buffer?
    canonical_.resize(PATH_MAX);
    if (!::getcwd(canonical_.data(), canonical_.size() + 1)) {
      error_ = std::string("failed to get current directory: ") +
               std::strerror(errno);
      return;
    }
    canonical_.resize(std::strlen(canonical_.c_str()));

    need_root_slash_ = false;
#else
#error "Unsupported platform"
#endif
  }

  void process_next_component() {
    path_string_view component = parse_next_component();
    QLJS_ASSERT(!component.empty());
    if (component == dot) {
      skip_to_next_component();
    } else if (component == dot_dot) {
      parent();
      skip_to_next_component();
    } else {
      canonical_ += preferred_component_separator;
      canonical_ += component;
      need_root_slash_ = false;

      file_type type = get_file_type(canonical_);
      switch (type) {
      case file_type::error:
        QLJS_ASSERT(!error_.empty());
        return;

      case file_type::directory:
        skip_to_next_component();
        break;

      case file_type::other:
        // Extra components and trailing slashes are not allowed for regular
        // files, FIFOs, etc.
        if (!path_to_process_.empty()) {
          error_ = std::string("failed to canonicalize path ") +
                   string_for_error_message(original_path_) + ": " +
                   string_for_error_message(canonical_) + ": " +
                   std::strerror(ENOTDIR);
          return;
        }
        break;

      case file_type::symlink:
        resolve_symlink();
        if (!error_.empty()) return;
        break;
      }
    }
  }

  path_string_view parse_next_component() {
    std::size_t slash_index =
        path_to_process_.find_first_of(component_separators);
    if (slash_index == path_to_process_.npos) {
      slash_index = path_to_process_.size();
    }
    std::size_t component_end_index = slash_index;

    path_string_view component =
        path_to_process_.substr(0, component_end_index);
    path_to_process_ = path_to_process_.substr(component_end_index);
    return component;
  }

  void skip_to_next_component() {
    std::size_t next_component_index =
        path_to_process_.find_first_not_of(component_separators);
    if (next_component_index == path_to_process_.npos) {
      next_component_index = path_to_process_.size();
    }
    path_to_process_ = path_to_process_.substr(next_component_index);
  }

  void parent() {
#if defined(_WIN32)
    HRESULT result =
        ::PathCchRemoveFileSpec(canonical_.data(), canonical_.size() + 1);
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
    canonical_.resize(std::wcslen(canonical_.data()));
#else
    if (!canonical_.empty()) {
      canonical_ = parent_path(std::move(canonical_));
    }
#endif
  }

  file_type get_file_type(const path_string &file_path) {
#if defined(_WIN32)
    DWORD attributes = ::GetFileAttributesW(file_path.c_str());
    if (attributes == INVALID_FILE_ATTRIBUTES) {
      error_ = std::string("failed to canonicalize path ") +
               string_for_error_message(original_path_) + ": " +
               string_for_error_message(canonical_) + ": " +
               windows_error_message(::GetLastError());
      return file_type::error;
    }
    if (attributes & FILE_ATTRIBUTE_REPARSE_POINT) {
      return file_type::symlink;
    }
    if (attributes & FILE_ATTRIBUTE_DIRECTORY) {
      return file_type::directory;
    }
    return file_type::other;
#elif QLJS_HAVE_SYS_STAT_H
    struct stat s;
    int lstat_rc = ::lstat(file_path.c_str(), &s);
    if (lstat_rc == -1) {
      error_ = std::string("failed to canonicalize path ") +
               string_for_error_message(original_path_) + ": " +
               string_for_error_message(canonical_) + ": " +
               std::strerror(errno);
      return file_type::error;
    }
    if (S_ISLNK(s.st_mode)) {
      return file_type::symlink;
    }
    if (S_ISDIR(s.st_mode)) {
      return file_type::directory;
    }
    return file_type::other;
#else
#error "Unsupported platform"
#endif
  }

  void resolve_symlink() {
#if QLJS_HAVE_UNISTD_H
    symlink_depth_ += 1;
    if (symlink_depth_ >= symlink_depth_limit_) {
      error_ = std::string("failed to canonicalize path ") +
               string_for_error_message(original_path_) + ": " +
               std::strerror(ELOOP);
      return;
    }

    std::string &new_readlink_buffer =
        readlink_buffers_[1 - used_readlink_buffer_];
    int readlink_rc =
        read_symbolic_link(canonical_.c_str(), &new_readlink_buffer);
    if (readlink_rc == -1) {
      error_ = std::string("failed to canonicalize path ") +
               string_for_error_message(original_path_) + ": " +
               string_for_error_message(canonical_) + ": " +
               std::strerror(errno);
      return;
    }

    // Rebase the remaining input components onto the readlink result.
    bool symlink_is_absolute = new_readlink_buffer[0] == '/';
    if (!symlink_is_absolute) {
      canonical_ = parent_path(std::move(canonical_));
      if (canonical_ != "/" && canonical_ != "//") {
        canonical_ += '/';
      }
      new_readlink_buffer.insert(0, canonical_);
    }
    new_readlink_buffer += path_to_process_;
    path_to_process_ = new_readlink_buffer;
    // After assigning to path_to_process_,
    // readlink_buffers_[used_readlink_buffer_] is no longer in use.
    swap_readlink_buffers();

    process_start_of_path();
#else
    // TODO(strager): Support symlinks on Windows.
    QLJS_UNIMPLEMENTED();
#endif
  }

  void swap_readlink_buffers() {
    used_readlink_buffer_ = 1 - used_readlink_buffer_;
  }

  path_string_view original_path_;

  // path_to_process_ points either to path (caller's input) or
  // readlink_buffers_[used_readlink_buffer_].
  path_string_view path_to_process_ = original_path_;

  path_string canonical_;
  bool need_root_slash_;

  path_string readlink_buffers_[2];
  int used_readlink_buffer_ = 0;  // Index into readlink_buffers_.

  // If non-empty, then an error occurred.
  std::string error_;

  int symlink_depth_ = 0;
  static constexpr int symlink_depth_limit_ = 100;
};
}

canonical_path_result canonicalize_path(const char *path) {
#if defined(_WIN32)
  std::optional<std::wstring> wpath = mbstring_to_wstring(path);
  if (!wpath.has_value()) {
    QLJS_UNIMPLEMENTED();
  }
  path_canonicalizer canonicalizer(*wpath);
#else
  path_canonicalizer canonicalizer(path);
#endif
  canonicalizer.canonicalize();
  return canonicalizer.result();
}

canonical_path_result canonicalize_path(const std::string &path) {
  return canonicalize_path(path.c_str());
}

namespace {
#if QLJS_HAVE_UNISTD_H
int read_symbolic_link(const char *symlink_path, std::string *out) {
  out->clear();
  out->reserve(PATH_MAX);
  out->push_back('\0');

retry:
  QLJS_ASSERT(!out->empty());
  ssize_t rc = ::readlink(symlink_path, out->data(), out->size());
  if (rc == -1) {
    return -1;
  }
  QLJS_ASSERT(rc >= 0);
  std::size_t written_size = narrow_cast<std::size_t>(rc);
  QLJS_ASSERT(written_size <= out->size());
  if (written_size == out->size()) {
    std::size_t new_buffer_size = out->size() * 2;
    QLJS_ASSERT(new_buffer_size > out->size());
    out->resize(new_buffer_size);
    goto retry;
  }

  out->resize(written_size);
  return 0;
}
#endif

#if QLJS_PATHS_POSIX
std::string string_for_error_message(std::string_view s) {
  return std::string(s);
}

const std::string &string_for_error_message(const std::string &s) { return s; }
#endif

#if QLJS_PATHS_WIN32
std::string string_for_error_message(std::wstring_view s) {
  // HACK(strager): Convert UTF-16 to UTF-8.
  return std::filesystem::path(s).string();
}
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
