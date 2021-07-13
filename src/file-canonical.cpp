// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <boost/leaf/common.hpp>
#include <boost/leaf/handle_errors.hpp>
#include <boost/leaf/on_error.hpp>
#include <boost/leaf/result.hpp>
#include <cerrno>
#include <climits>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-canonical.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file-path.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/leaf.h>
#include <quick-lint-js/narrow-cast.h>
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

using namespace std::literals::string_literals;

namespace quick_lint_js {
namespace {
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
constexpr path_char preferred_component_separator = L'\\';
constexpr path_char component_separators[] = L"/\\";
constexpr path_string_view dot = L".";
constexpr path_string_view dot_dot = L"..";
#elif QLJS_PATHS_POSIX
constexpr path_char preferred_component_separator = '/';
constexpr path_char component_separators[] = "/";
constexpr path_string_view dot = ".";
constexpr path_string_view dot_dot = "..";
#else
#error "Unsupported platform"
#endif

// TODO(strager): Avoid hard-coding std::string. Use std::wstring always on
// Windows.
constexpr char preferred_component_separator_char =
    static_cast<char>(preferred_component_separator);

#if QLJS_HAVE_UNISTD_H
int read_symbolic_link(const char *symlink_path, std::string *out);
#endif

#if QLJS_PATHS_POSIX
const std::string &string_for_error_message(const std::string &);
#endif

#if QLJS_PATHS_WIN32
std::string string_for_error_message(std::wstring_view);
#endif
}

canonical_path::canonical_path(std::string &&path) : path_(std::move(path)) {
  QLJS_ASSERT(!this->path_.empty());
}

std::string_view canonical_path::path() const &noexcept { return this->path_; }

std::string &&canonical_path::path() && noexcept {
  return std::move(this->path_);
}

const char *canonical_path::c_str() const noexcept {
  return this->path_.c_str();
}

bool operator==(const canonical_path &lhs, const canonical_path &rhs) noexcept {
  return lhs.path() == rhs.path();
}

bool operator!=(const canonical_path &lhs, const canonical_path &rhs) noexcept {
  return !(lhs == rhs);
}

bool operator==(std::string_view lhs, const canonical_path &rhs) noexcept {
  return lhs == rhs.path();
}

bool operator!=(std::string_view lhs, const canonical_path &rhs) noexcept {
  return !(lhs == rhs);
}

bool operator==(const canonical_path &lhs, std::string_view rhs) noexcept {
  return lhs.path() == rhs;
}

bool operator!=(const canonical_path &lhs, std::string_view rhs) noexcept {
  return !(lhs == rhs);
}

void canonical_path::append_component(std::string_view component) {
  if (this->path_[this->path_.size() - 1] !=
      preferred_component_separator_char) {
    this->path_.push_back(preferred_component_separator_char);
  }
  this->path_.append(component);
}

bool canonical_path::parent() {
#if QLJS_PATHS_POSIX
  if (this->path_[this->path_.size() - 1] == '/') {
    return false;
  }
  std::size_t slash_index = this->path_.find_last_of("/");
  QLJS_ASSERT(slash_index != std::string::npos);
  if (slash_index == 0 || slash_index == 1) {
    // Preserve the slash for resulting root paths.
    this->path_.resize(slash_index + 1);
  } else {
    this->path_.resize(slash_index);
  }
  return true;
#elif QLJS_PATHS_WIN32
  // TODO(strager): Avoid string conversions and copying.
  std::optional<std::wstring> wpath = mbstring_to_wstring(this->path_.c_str());
  QLJS_ASSERT(wpath.has_value());
  HRESULT result = ::PathCchRemoveFileSpec(wpath->data(), wpath->size() + 1);
  switch (result) {
  case S_OK:
    wpath->resize(std::wcslen(wpath->data()));
    this->path_ = std::filesystem::path(*wpath).string();
    return true;
  case S_FALSE:
    // Path is a root path already.
    return false;
  default:
    QLJS_UNIMPLEMENTED();
    break;
  }
#else
#error "Unsupported platform"
#endif
}

canonical_path_result::canonical_path_result() {}

canonical_path_result::canonical_path_result(std::string &&path,
                                             std::size_t existing_path_length)
    : path_(std::move(path)), existing_path_length_(existing_path_length) {}

std::string_view canonical_path_result::path() const &noexcept {
  return this->path_->path();
}

std::string &&canonical_path_result::path() && noexcept {
  return std::move(*this->path_).path();
}

const char *canonical_path_result::c_str() const noexcept {
  return this->path_->c_str();
}

const canonical_path &canonical_path_result::canonical() const &noexcept {
  return *this->path_;
}

canonical_path &&canonical_path_result::canonical() && noexcept {
  return std::move(*this->path_);
}

bool canonical_path_result::have_missing_components() const noexcept {
  return this->existing_path_length_ != this->path_->path_.size();
}

void canonical_path_result::drop_missing_components() {
  this->path_->path_.resize(this->existing_path_length_);
}

namespace {
template <class Derived>
class path_canonicalizer_base {
 public:
  explicit path_canonicalizer_base(path_string_view path)
      : original_path_(path) {}

  boost::leaf::result<void> canonicalize() {
    if (original_path_.empty()) {
      return boost::leaf::new_error(e_invalid_argument_empty_path());
    }

    boost::leaf::result<void> ok = this->derived().process_start_of_path();
    if (!ok) return ok.error();

    while (!path_to_process_.empty()) {
      boost::leaf::result<void> next_ok = process_next_component();
      if (!next_ok) return next_ok.error();
    }

    if (need_root_slash_) {
#if QLJS_PATHS_POSIX
      QLJS_ASSERT(canonical_.empty());
#endif
      canonical_ += preferred_component_separator;
    }

    if (existing_path_length_ == 0) {
      existing_path_length_ = canonical_.size();
    }

    return {};
  }

 protected:
  enum class file_type {
    directory,
    does_not_exist,
    other,
    symlink,
  };

 private:
  boost::leaf::result<void> process_next_component() {
    path_string_view component = parse_next_component();
    QLJS_ASSERT(!component.empty());
    if (component == dot) {
      skip_to_next_component();
    } else if (component == dot_dot) {
      if (existing_path_length_ == 0) {
        this->derived().parent();
      } else {
        QLJS_ASSERT(!canonical_.empty());
        canonical_ += preferred_component_separator;
        canonical_ += component;
      }
      skip_to_next_component();
    } else {
      std::size_t canonical_length_without_component = canonical_.size();

      canonical_ += preferred_component_separator;
      canonical_ += component;
      need_root_slash_ = false;

      if (existing_path_length_ != 0) {
        // A parent path did not exist, so this path certainly does not exist.
        // Don't bother checking.
        skip_to_next_component();
        return {};
      }

      boost::leaf::result<file_type> type =
          this->derived().get_file_type(canonical_);
      if (!type) return type.error();
      switch (*type) {
      case file_type::does_not_exist:
        if (existing_path_length_ == 0) {
          existing_path_length_ = canonical_length_without_component;
        }
        skip_to_next_component();
        break;

      case file_type::directory:
        skip_to_next_component();
        break;

      case file_type::other:
        // Extra components and trailing slashes are not allowed for regular
        // files, FIFOs, etc.
        if (!path_to_process_.empty()) {
          return boost::leaf::new_error(
              e_errno{ENOTDIR},
              e_canonicalizing_path{string_for_error_message(canonical_)});
        }
        break;

      case file_type::symlink: {
        boost::leaf::result<void> ok = this->derived().resolve_symlink();
        if (!ok) return ok.error();
        break;
      }
      }
    }

    return {};
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

  Derived &derived() { return *static_cast<Derived *>(this); }

 protected:
  void skip_to_next_component() {
    std::size_t next_component_index =
        path_to_process_.find_first_not_of(component_separators);
    if (next_component_index == path_to_process_.npos) {
      next_component_index = path_to_process_.size();
    }
    path_to_process_ = path_to_process_.substr(next_component_index);
  }

  path_string_view original_path_;

  // path_to_process_ points either to path (caller's input) or
  // readlink_buffers_[used_readlink_buffer_].
  path_string_view path_to_process_ = original_path_;

  path_string canonical_;
  bool need_root_slash_;

  // During canonicalization, if existing_path_length_ is 0, then we have not
  // found a non-existing path.
  //
  // During canonicalization, if existing_path_length_ is not 0, then we have
  // found a non-existing path. '..' should be preserved.
  std::size_t existing_path_length_ = 0;

  path_string readlink_buffers_[2];
  int used_readlink_buffer_ = 0;  // Index into readlink_buffers_.

  int symlink_depth_ = 0;
  static constexpr int symlink_depth_limit_ = 100;
};

#if QLJS_PATHS_POSIX
class posix_path_canonicalizer
    : public path_canonicalizer_base<posix_path_canonicalizer> {
 private:
  using base = path_canonicalizer_base<posix_path_canonicalizer>;

 public:
  using base::canonicalize;
  using base::path_canonicalizer_base;

  canonical_path_result result() {
    return canonical_path_result(std::move(canonical_), existing_path_length_);
  }

  boost::leaf::result<void> process_start_of_path() {
    bool is_absolute = !path_to_process_.empty() &&
                       path_to_process_[0] == preferred_component_separator;
    if (is_absolute) {
      path_to_process_ = path_to_process_.substr(1);
      canonical_.clear();
      need_root_slash_ = true;
    } else {
      boost::leaf::result<void> ok = load_cwd();
      if (!ok) return ok.error();
    }
    return {};
  }

  boost::leaf::result<void> load_cwd() {
    // TODO(strager): Is PATH_MAX sufficient? Do we need to keep growing our
    // buffer?
    canonical_.resize(PATH_MAX);
    if (!::getcwd(canonical_.data(), canonical_.size() + 1)) {
      return boost::leaf::new_error(e_errno{errno});
    }
    canonical_.resize(std::strlen(canonical_.c_str()));

    need_root_slash_ = false;
    return {};
  }

  void parent() {
    if (!canonical_.empty()) {
      canonical_ = parent_path(std::move(canonical_));
    }
  }

  boost::leaf::result<file_type> get_file_type(const path_string &file_path) {
    struct stat s;
    int lstat_rc = ::lstat(file_path.c_str(), &s);
    if (lstat_rc == -1) {
      if (errno == ENOENT) {
        return file_type::does_not_exist;
      }
      return boost::leaf::new_error(
          e_errno{errno},
          e_canonicalizing_path{string_for_error_message(canonical_)});
    }
    if (S_ISLNK(s.st_mode)) {
      return file_type::symlink;
    }
    if (S_ISDIR(s.st_mode)) {
      return file_type::directory;
    }
    return file_type::other;
  }

  boost::leaf::result<void> resolve_symlink() {
    symlink_depth_ += 1;
    if (symlink_depth_ >= symlink_depth_limit_) {
      return boost::leaf::new_error(e_too_many_symlinks());
    }

    std::string &new_readlink_buffer =
        readlink_buffers_[1 - used_readlink_buffer_];
    int readlink_rc =
        read_symbolic_link(canonical_.c_str(), &new_readlink_buffer);
    if (readlink_rc == -1) {
      return boost::leaf::new_error(
          e_errno{errno},
          e_canonicalizing_path{string_for_error_message(canonical_)});
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

    return {};
  }

 private:
  void swap_readlink_buffers() {
    used_readlink_buffer_ = 1 - used_readlink_buffer_;
  }
};
#endif

#if QLJS_PATHS_WIN32
class windows_path_canonicalizer
    : public path_canonicalizer_base<windows_path_canonicalizer> {
 private:
  using base = path_canonicalizer_base<windows_path_canonicalizer>;

 public:
  using base::canonicalize;
  using base::path_canonicalizer_base;

  canonical_path_result result() {
    // HACK(strager): Convert UTF-16 to UTF-8.
    // TODO(strager): existing_path_length_ is in UTF-16 code units, but it's
    // interpreted as UTF-8 code units! Fix by storing a std::wstring in
    // canonical_path.
    return canonical_path_result(std::filesystem::path(canonical_).string(),
                                 existing_path_length_);
  }

  boost::leaf::result<void> process_start_of_path() {
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

    case HRESULT_FROM_WIN32(ERROR_INVALID_PARAMETER): {
      // Path is invalid or is relative. Assume that it is relative.
      boost::leaf::result<void> ok = load_cwd();
      if (!ok) return ok.error();
      break;
    }

    default:
      QLJS_UNIMPLEMENTED();
      break;
    }

    return {};
  }

  boost::leaf::result<void> load_cwd() {
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
    return {};
  }

  void parent() {
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
  }

  boost::leaf::result<file_type> get_file_type(const path_string &file_path) {
    DWORD attributes = ::GetFileAttributesW(file_path.c_str());
    if (attributes == INVALID_FILE_ATTRIBUTES) {
      DWORD error = ::GetLastError();
      if (error == ERROR_FILE_NOT_FOUND) {
        return file_type::does_not_exist;
      }
      return boost::leaf::new_error(
          e_LastError{error},
          e_canonicalizing_path{string_for_error_message(canonical_)});
    }
    if (attributes & FILE_ATTRIBUTE_REPARSE_POINT) {
      return file_type::symlink;
    }
    if (attributes & FILE_ATTRIBUTE_DIRECTORY) {
      return file_type::directory;
    }
    return file_type::other;
  }

  boost::leaf::result<void> resolve_symlink() {
    // TODO(strager): Support symlinks on Windows.
    QLJS_UNIMPLEMENTED();
    return {};
  }

 private:
  void swap_readlink_buffers() {
    used_readlink_buffer_ = 1 - used_readlink_buffer_;
  }
};
#endif
}

boost::leaf::result<canonical_path_result> canonicalize_path(const char *path) {
  auto api_guard = boost::leaf::on_error(e_api_canonicalize_path());
  auto path_guard = boost::leaf::on_error(e_file_path{path});
#if defined(_WIN32)
  std::optional<std::wstring> wpath = mbstring_to_wstring(path);
  if (!wpath.has_value()) {
    QLJS_UNIMPLEMENTED();
  }
  windows_path_canonicalizer canonicalizer(*wpath);
#else
  posix_path_canonicalizer canonicalizer(path);
#endif
  boost::leaf::result<void> ok = canonicalizer.canonicalize();
  if (!ok) return ok.error();
  return canonicalizer.result();
}

boost::leaf::result<canonical_path_result> canonicalize_path(
    const std::string &path) {
  return canonicalize_path(path.c_str());
}

sloppy_result<canonical_path_result> canonicalize_path_sloppy(
    const char *path) {
  return boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<sloppy_result<canonical_path_result>> {
        boost::leaf::result<canonical_path_result> canonical =
            canonicalize_path(path);
        if (!canonical) return canonical.error();
        return sloppy_result<canonical_path_result>(*canonical);
      },
      make_canonicalize_path_error_handlers([](std::string &&message) {
        return sloppy_result<canonical_path_result>::failure(
            std::move(message));
      }),
      []() {
        QLJS_ASSERT(false);
        return sloppy_result<canonical_path_result>::failure("unknown error");
      });
}

sloppy_result<canonical_path_result> canonicalize_path_sloppy(
    const std::string &path) {
  return canonicalize_path_sloppy(path.c_str());
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
