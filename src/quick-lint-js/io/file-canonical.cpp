// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if !defined(__EMSCRIPTEN__)

#include <algorithm>
#include <cerrno>
#include <climits>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/cast.h>
#include <quick-lint-js/util/utf-16.h>
#include <string>
#include <string_view>

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if QLJS_HAVE_WINDOWS_H
#include <quick-lint-js/port/windows.h>
#include <pathcch.h>
#endif

#if defined(_WIN32)
#define QLJS_PATHS_POSIX 0
#define QLJS_PATHS_WIN32 1
#endif

#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 200112L
#define QLJS_PATHS_POSIX 1
#define QLJS_PATHS_WIN32 0
#endif

using namespace std::literals::string_literals;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
#if QLJS_PATHS_WIN32
using Path_Char = wchar_t;
#elif QLJS_PATHS_POSIX
using Path_Char = char;
#else
#error "Unsupported platform"
#endif
using Path_String = std::basic_string<Path_Char>;
using Path_String_View = std::basic_string_view<Path_Char>;

#if QLJS_PATHS_WIN32
constexpr Path_Char preferred_component_separator = L'\\';
constexpr Path_Char component_separators[] = L"/\\";
constexpr Path_String_View dot = L".";
constexpr Path_String_View dot_dot = L"..";
#elif QLJS_PATHS_POSIX
constexpr Path_Char preferred_component_separator = '/';
constexpr Path_Char component_separators[] = "/";
constexpr Path_String_View dot = ".";
constexpr Path_String_View dot_dot = "..";
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

struct Canonicalizing_Path_IO_Error {
  Path_String canonicalizing_path;
  Platform_File_IO_Error io_error;
};

class Null_Canonicalize_Observer : public Canonicalize_Observer {
 public:
  static Null_Canonicalize_Observer *instance() {
    static Null_Canonicalize_Observer singleton;
    return &singleton;
  }

  void on_canonicalize_child_of_directory(const char *) override {}
  void on_canonicalize_child_of_directory(const wchar_t *) override {}
};
}

Canonical_Path::Canonical_Path(std::string &&path) : path_(std::move(path)) {
  QLJS_ASSERT(!this->path_.empty());

#if QLJS_PATHS_POSIX
  std::string_view p(this->path_);
  while (!ends_with(p, "/"sv)) {
    std::size_t slash_index = p.find_last_of("/");
    QLJS_ASSERT(slash_index != std::string::npos);
    if (slash_index == 0 || slash_index == 1) {
      // Preserve the slash for resulting root paths.
      this->path_lengths_.push_back(slash_index + 1);
      p = p.substr(0, slash_index + 1);
    } else {
      this->path_lengths_.push_back(slash_index);
      p = p.substr(0, slash_index);
    }
  }
  reverse(this->path_lengths_);
#elif QLJS_PATHS_WIN32
  // TODO(strager): Avoid string conversions and copying.
  std::optional<std::wstring> wpath = mbstring_to_wstring(this->path_.c_str());
  QLJS_ASSERT(wpath.has_value());
  for (;;) {
    HRESULT result = ::PathCchRemoveFileSpec(wpath->data(), wpath->size() + 1);
    switch (result) {
    case S_OK:
      this->path_lengths_.push_back(count_utf_8_code_units(wpath->c_str()));
      break;
    case S_FALSE:
      // Path is a root path already.
      goto done;
    default:
      QLJS_UNIMPLEMENTED();
      break;
    }
  }
done:
  reverse(this->path_lengths_);
#else
#error "Unsupported platform"
#endif
}

std::string_view Canonical_Path::path() const & { return this->path_; }

std::string &&Canonical_Path::path() && { return std::move(this->path_); }

const char *Canonical_Path::c_str() const { return this->path_.c_str(); }

bool operator==(const Canonical_Path &lhs, const Canonical_Path &rhs) {
  return lhs.path() == rhs.path();
}

bool operator!=(const Canonical_Path &lhs, const Canonical_Path &rhs) {
  return !(lhs == rhs);
}

bool operator==(std::string_view lhs, const Canonical_Path &rhs) {
  return lhs == rhs.path();
}

bool operator!=(std::string_view lhs, const Canonical_Path &rhs) {
  return !(lhs == rhs);
}

bool operator==(const Canonical_Path &lhs, std::string_view rhs) {
  return lhs.path() == rhs;
}

bool operator!=(const Canonical_Path &lhs, std::string_view rhs) {
  return !(lhs == rhs);
}

void Canonical_Path::append_component(std::string_view component) {
  this->path_lengths_.push_back(this->path_.size());
  if (this->path_[this->path_.size() - 1] !=
      preferred_component_separator_char) {
    this->path_.push_back(preferred_component_separator_char);
  }
  this->path_.append(component);
}

bool Canonical_Path::parent() {
  if (this->path_lengths_.empty()) {
    return false;
  }
  std::size_t path_length = this->path_lengths_.back();
  this->path_lengths_.pop_back();
  this->path_.resize(path_length);
  return true;
}

Canonical_Path_Result::Canonical_Path_Result(std::string &&path,
                                             std::size_t existing_path_length)
    : path_(std::move(path)), existing_path_length_(existing_path_length) {}

std::string_view Canonical_Path_Result::path() const & {
  return this->path_.path();
}

std::string &&Canonical_Path_Result::path() && {
  return std::move(this->path_).path();
}

const char *Canonical_Path_Result::c_str() const { return this->path_.c_str(); }

const Canonical_Path &Canonical_Path_Result::canonical() const & {
  return this->path_;
}

Canonical_Path &&Canonical_Path_Result::canonical() && {
  return std::move(this->path_);
}

bool Canonical_Path_Result::have_missing_components() const {
  return this->existing_path_length_ != this->path_.path_.size();
}

void Canonical_Path_Result::drop_missing_components() {
  while (!this->path_.path_lengths_.empty() &&
         this->path_.path_lengths_.back() >= this->existing_path_length_) {
    this->path_.path_lengths_.pop_back();
  }
  this->path_.path_.resize(this->existing_path_length_);
}

std::string Canonicalize_Path_IO_Error::to_string() const {
  return "failed to canonicalize "s + this->input_path + ": "s +
         this->canonicalizing_path + ": "s + this->io_error.to_string();
}

bool operator==(const Canonicalize_Path_IO_Error &lhs,
                const Canonicalize_Path_IO_Error &rhs) {
  return lhs.input_path == rhs.input_path &&
         lhs.canonicalizing_path == rhs.canonicalizing_path &&
         lhs.io_error == rhs.io_error;
}

bool operator!=(const Canonicalize_Path_IO_Error &lhs,
                const Canonicalize_Path_IO_Error &rhs) {
  return !(lhs == rhs);
}

namespace {
template <class Derived>
class Path_Canonicalizer_Base {
 public:
  explicit Path_Canonicalizer_Base(Path_String_View path,
                                   Canonicalize_Observer *observer)
      : observer_(observer), original_path_(path) {}

  Result<void, Canonicalizing_Path_IO_Error> canonicalize() {
    if (original_path_.empty()) {
#if QLJS_HAVE_WINDOWS_H
      Windows_File_IO_Error io_error = {ERROR_INVALID_PARAMETER};
#elif QLJS_HAVE_UNISTD_H
      POSIX_File_IO_Error io_error = {EINVAL};
#else
#error "Unsupported platform"
#endif
      return failed_result(Canonicalizing_Path_IO_Error{
          .canonicalizing_path = {},
          .io_error = io_error,
      });
    }

    Result<void, Canonicalizing_Path_IO_Error> r =
        this->derived().process_start_of_path();
    if (!r.ok()) return r.propagate();

    while (!path_to_process_.empty()) {
      Result<void, Canonicalizing_Path_IO_Error> next_r =
          process_next_component();
      if (!next_r.ok()) return next_r.propagate();
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
  enum class File_Type {
    directory,
    does_not_exist,
    other,
    symlink,
  };

  quick_lint_js::Result<void, Platform_File_IO_Error> load_cwd() {
    Result<void, Platform_File_IO_Error> r =
        get_current_working_directory(this->canonical_);
    if (!r.ok()) {
      return r.propagate();
    }
    this->need_root_slash_ = false;
    return {};
  }

 private:
  Result<void, Canonicalizing_Path_IO_Error> process_next_component() {
    Path_String_View component = parse_next_component();
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

      bool parent_path_exists = existing_path_length_ == 0;
      if (parent_path_exists && canonical_length_without_component > 0) {
        this->observer_->on_canonicalize_child_of_directory(canonical_.c_str());
      }

      canonical_ += preferred_component_separator;
      canonical_ += component;
      need_root_slash_ = false;

      if (!parent_path_exists) {
        // A parent path did not exist, so this path certainly does not exist.
        // Don't bother checking.
        skip_to_next_component();
        return {};
      }

      Result<File_Type, Platform_File_IO_Error> type =
          this->derived().get_file_type(canonical_);
      if (!type.ok()) {
        return failed_result(Canonicalizing_Path_IO_Error{
            .canonicalizing_path = canonical_,
            .io_error = type.error(),
        });
      }
      switch (*type) {
      case File_Type::does_not_exist:
        if (existing_path_length_ == 0) {
          existing_path_length_ = canonical_length_without_component;
        }
        skip_to_next_component();
        break;

      case File_Type::directory:
        skip_to_next_component();
        break;

      case File_Type::other:
        // Extra components and trailing slashes are not allowed for regular
        // files, FIFOs, etc.
        if (!path_to_process_.empty()) {
          return failed_result(Canonicalizing_Path_IO_Error {
            .canonicalizing_path = canonical_,
#if QLJS_HAVE_UNISTD_H
            .io_error = POSIX_File_IO_Error{ENOTDIR},
#elif QLJS_HAVE_WINDOWS_H
                .io_error = Windows_File_IO_Error{ERROR_DIRECTORY},
#else
#error "Unsupported platform"
#endif
          });
        }
        break;

      case File_Type::symlink: {
        quick_lint_js::Result<void, Canonicalizing_Path_IO_Error> r =
            this->derived().resolve_symlink();
        if (!r.ok()) return r.propagate();
        break;
      }
      }
    }

    return {};
  }

  Path_String_View parse_next_component() {
    std::size_t slash_index =
        path_to_process_.find_first_of(component_separators);
    if (slash_index == path_to_process_.npos) {
      slash_index = path_to_process_.size();
    }
    std::size_t component_end_index = slash_index;

    Path_String_View component =
        path_to_process_.substr(0, component_end_index);
    path_to_process_ = path_to_process_.substr(component_end_index);
    return component;
  }

  Derived &derived() { return *derived_cast<Derived *>(this); }

 protected:
  void skip_to_next_component() {
    std::size_t next_component_index =
        path_to_process_.find_first_not_of(component_separators);
    if (next_component_index == path_to_process_.npos) {
      next_component_index = path_to_process_.size();
    }
    path_to_process_ = path_to_process_.substr(next_component_index);
  }

  Canonicalize_Observer *observer_;
  Path_String_View original_path_;

  // path_to_process_ points either to path (caller's input) or
  // readlink_buffers_[used_readlink_buffer_].
  Path_String_View path_to_process_ = original_path_;

  Path_String canonical_;
  bool need_root_slash_;

  // During canonicalization, if existing_path_length_ is 0, then we have not
  // found a non-existing path.
  //
  // During canonicalization, if existing_path_length_ is not 0, then we have
  // found a non-existing path. '..' should be preserved.
  std::size_t existing_path_length_ = 0;

  Path_String readlink_buffers_[2];
  int used_readlink_buffer_ = 0;  // Index into readlink_buffers_.

  int symlink_depth_ = 0;
  static constexpr int symlink_depth_limit_ = 100;
};

#if QLJS_PATHS_POSIX
class POSIX_Path_Canonicalizer
    : public Path_Canonicalizer_Base<POSIX_Path_Canonicalizer> {
 private:
  using Base = Path_Canonicalizer_Base<POSIX_Path_Canonicalizer>;

 public:
  using Base::canonicalize;
  using Base::Path_Canonicalizer_Base;

  Canonical_Path_Result result() {
    return Canonical_Path_Result(std::move(canonical_), existing_path_length_);
  }

  quick_lint_js::Result<void, Canonicalizing_Path_IO_Error>
  process_start_of_path() {
    bool is_absolute = !path_to_process_.empty() &&
                       path_to_process_[0] == preferred_component_separator;
    if (is_absolute) {
      path_to_process_ = path_to_process_.substr(1);
      canonical_.clear();
      need_root_slash_ = true;
    } else {
      quick_lint_js::Result<void, POSIX_File_IO_Error> r = load_cwd();
      if (!r.ok()) {
        return failed_result(Canonicalizing_Path_IO_Error{
            .canonicalizing_path = Path_String(this->path_to_process_),
            .io_error = r.error(),
        });
      }
    }
    return {};
  }

  void parent() {
    if (!canonical_.empty()) {
      canonical_ = parent_path(std::move(canonical_));
    }
  }

  quick_lint_js::Result<File_Type, POSIX_File_IO_Error> get_file_type(
      const Path_String &file_path) {
    struct stat s;
    int lstat_rc = ::lstat(file_path.c_str(), &s);
    if (lstat_rc == -1) {
      if (errno == ENOENT) {
        return File_Type::does_not_exist;
      }
      return failed_result(POSIX_File_IO_Error{errno});
    }
    if (S_ISLNK(s.st_mode)) {
      return File_Type::symlink;
    }
    if (S_ISDIR(s.st_mode)) {
      return File_Type::directory;
    }
    return File_Type::other;
  }

  quick_lint_js::Result<void, Canonicalizing_Path_IO_Error> resolve_symlink() {
    symlink_depth_ += 1;
    if (symlink_depth_ >= symlink_depth_limit_) {
      return failed_result(Canonicalizing_Path_IO_Error{
          .canonicalizing_path = canonical_,
          .io_error = POSIX_File_IO_Error{ELOOP},
      });
    }

    std::string &new_readlink_buffer =
        readlink_buffers_[1 - used_readlink_buffer_];
    int readlink_rc =
        read_symbolic_link(canonical_.c_str(), &new_readlink_buffer);
    if (readlink_rc == -1) {
      return failed_result(Canonicalizing_Path_IO_Error{
          .canonicalizing_path = canonical_,
          .io_error = POSIX_File_IO_Error{errno},
      });
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

    quick_lint_js::Result<void, Canonicalizing_Path_IO_Error> r =
        process_start_of_path();
    if (!r.ok()) return r.propagate();

    return {};
  }

 private:
  void swap_readlink_buffers() {
    used_readlink_buffer_ = 1 - used_readlink_buffer_;
  }
};
#endif

#if QLJS_PATHS_WIN32
class Windows_Path_Canonicalizer
    : public Path_Canonicalizer_Base<Windows_Path_Canonicalizer> {
 private:
  using Base = Path_Canonicalizer_Base<Windows_Path_Canonicalizer>;

 public:
  using Base::canonicalize;
  using Base::Path_Canonicalizer_Base;

  Canonical_Path_Result result() {
    std::optional<std::string> canonical_utf_8 =
        wstring_to_mbstring(canonical_);
    if (!canonical_utf_8.has_value()) {
      QLJS_UNIMPLEMENTED();
    }
    std::size_t existing_path_length_utf_8 =
        count_utf_8_code_units(std::u16string_view(
            reinterpret_cast<const char16_t *>(canonical_.data()),
            existing_path_length_));
    return Canonical_Path_Result(std::move(*canonical_utf_8),
                                 existing_path_length_utf_8);
  }

  quick_lint_js::Result<void, Canonicalizing_Path_IO_Error>
  process_start_of_path() {
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
      quick_lint_js::Result<void, Windows_File_IO_Error> r = load_cwd();
      if (!r.ok()) {
        return failed_result(Canonicalizing_Path_IO_Error{
            .canonicalizing_path = Path_String(this->path_to_process_),
            .io_error = r.error(),
        });
      }
      break;
    }

    default:
      QLJS_UNIMPLEMENTED();
      break;
    }

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

  quick_lint_js::Result<File_Type, Windows_File_IO_Error> get_file_type(
      const Path_String &file_path) {
    DWORD attributes = ::GetFileAttributesW(file_path.c_str());
    if (attributes == INVALID_FILE_ATTRIBUTES) {
      DWORD error = ::GetLastError();
      if (error == ERROR_FILE_NOT_FOUND) {
        return File_Type::does_not_exist;
      }
      return failed_result(Windows_File_IO_Error{error});
    }
    if (attributes & FILE_ATTRIBUTE_REPARSE_POINT) {
      return File_Type::symlink;
    }
    if (attributes & FILE_ATTRIBUTE_DIRECTORY) {
      return File_Type::directory;
    }
    return File_Type::other;
  }

  quick_lint_js::Result<void, Canonicalizing_Path_IO_Error> resolve_symlink() {
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

Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
    const char *path) {
  return canonicalize_path(path, Null_Canonicalize_Observer::instance());
}

Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
    const std::string &path) {
  return canonicalize_path(path.c_str());
}

Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
    const char *path, Canonicalize_Observer *observer) {
#if defined(_WIN32)
  std::optional<std::wstring> wpath = mbstring_to_wstring(path);
  if (!wpath.has_value()) {
    QLJS_UNIMPLEMENTED();
  }
  Windows_Path_Canonicalizer canonicalizer(*wpath, observer);
#else
  POSIX_Path_Canonicalizer canonicalizer(path, observer);
#endif
  Result<void, Canonicalizing_Path_IO_Error> r = canonicalizer.canonicalize();
  if (!r.ok()) {
    return failed_result(Canonicalize_Path_IO_Error{
        .input_path = path,
        .canonicalizing_path =
            string_for_error_message(std::move(r.error().canonicalizing_path)),
        .io_error = r.error().io_error,
    });
  }
  return canonicalizer.result();
}

Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
    const std::string &path, Canonicalize_Observer *observer) {
  return canonicalize_path(path.c_str(), observer);
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
  std::optional<std::string> canonical_utf_8 = wstring_to_mbstring(s);
  if (!canonical_utf_8.has_value()) {
    return "(error converting error message to UTF-8)";
  }
  return std::move(*canonical_utf_8);
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
