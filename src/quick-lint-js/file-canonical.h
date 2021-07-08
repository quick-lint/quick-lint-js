// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILE_CANONICAL_H
#define QUICK_LINT_JS_FILE_CANONICAL_H

#include <boost/leaf/common.hpp>
#include <boost/leaf/result.hpp>
#include <cstddef>
#include <functional>
#include <optional>
#include <quick-lint-js/sloppy-result.h>
#include <string>
#include <string_view>
#include <tuple>

namespace quick_lint_js {
class canonical_path_result;

// A filesystem path.
//
// * The path is absolute (i.e. not relative to the current working directory or
//   current drive). (The path is relative to the current chroot/jail/namespace,
//   though.)
// * The path has no '.' components.
// * The path has no '..' components, unless the exception mentioned below
//   applies.
// * The path has no redundant component separators (\ or /, depending on the
//   operating system).
// * No subpath refers to a symlink, assuming no changes to the filesystem since
//   creation of the canonical_path.
//
// Exception to the above rules: A canonical_path can contain one or more '..'
// components before canonical_path_result::drop_missing_components has been
// called.
class canonical_path {
 public:
  // Does not check the validity of the path.
  explicit canonical_path(std::string &&path);

  std::string_view path() const &noexcept;
  std::string &&path() && noexcept;
  const char *c_str() const noexcept;

  // Add a new component to the end of the path.
  //
  // This function does not consult the filesystem.
  void append_component(std::string_view component);

  // Remove the last component of the path.
  //
  // If the path is a root path, this function returns false does not modify
  // *this.
  //
  // If the path is not a root path, this function returns true and modifies
  // *this.
  //
  // This function does not consult the filesystem.
  bool parent();

  friend bool operator==(const canonical_path &,
                         const canonical_path &) noexcept;
  friend bool operator!=(const canonical_path &,
                         const canonical_path &) noexcept;
  friend bool operator==(std::string_view, const canonical_path &) noexcept;
  friend bool operator!=(std::string_view, const canonical_path &) noexcept;
  friend bool operator==(const canonical_path &, std::string_view) noexcept;
  friend bool operator!=(const canonical_path &, std::string_view) noexcept;

 private:
  std::string path_;

  friend canonical_path_result;
};

class canonical_path_result {
 public:
  explicit canonical_path_result(std::string &&path,
                                 std::size_t existing_path_length);

  std::string_view path() const &noexcept;
  std::string &&path() && noexcept;
  const char *c_str() const noexcept;

  const canonical_path &canonical() const &noexcept;
  canonical_path &&canonical() && noexcept;

  std::string &&error() && noexcept;

  bool ok() const noexcept { return this->error_.empty(); }

  bool have_missing_components() const noexcept;
  void drop_missing_components();

  static canonical_path_result failure(std::string &&error);

 private:
  explicit canonical_path_result();

  std::optional<canonical_path> path_;
  std::string error_;
  std::size_t existing_path_length_;
};

struct e_api_canonicalize_path {};
struct e_canonicalizing_path {
  std::string path;
};
struct e_invalid_argument_empty_path {};
struct e_too_many_symlinks {};

// Possible error types:
//
// * boost::leaf::e_errno
// * boost::leaf::e_file_name
// * boost::leaf::windows::e_LastError
// * e_api_canonicalize_path (always present)
// * e_canonicalizing_path
// * e_invalid_argument_empty_path
// * e_too_many_symlinks
boost::leaf::result<canonical_path_result> canonicalize_path_2(
    const char *path);
boost::leaf::result<canonical_path_result> canonicalize_path_2(
    const std::string &path);

sloppy_result<canonical_path_result> canonicalize_path_sloppy(const char *path);
sloppy_result<canonical_path_result> canonicalize_path_sloppy(
    const std::string &path);

// Valid signatures for handle_error:
// <<any rvalue result type>> handle_error(const std::string &message);
// <<any rvalue result type>> handle_error(std::string &&message);
template <class Func>
auto make_canonicalize_path_error_handlers(const Func &handle_error) {
  using namespace std::literals::string_literals;
  return std::tuple(
      [handle_error](e_api_canonicalize_path,
                     const boost::leaf::e_file_name &path,
                     boost::leaf::e_errno error,
                     const e_canonicalizing_path &canonicalizing) {
        return handle_error("failed to canonicalize "s + canonicalizing.path +
                            ": "s + path.value + ": "s +
                            std::strerror(error.value));
      },
      [handle_error](e_api_canonicalize_path,
                     const boost::leaf::e_file_name &path,
                     boost::leaf::e_errno error) {
        return handle_error("failed to canonicalize "s + path.value + ": "s +
                            std::strerror(error.value));
      },
      [handle_error](e_api_canonicalize_path,
                     const boost::leaf::e_file_name &path,
                     e_too_many_symlinks) {
        return handle_error("failed to canonicalize "s + path.value +
                            ": Too many levels of symlink"s);
      },
      [handle_error](e_api_canonicalize_path, e_invalid_argument_empty_path) {
        return handle_error("failed to canonicalize empty path: "s +
                            std::strerror(EINVAL));
      },
      [handle_error](e_api_canonicalize_path) {
        QLJS_ASSERT(false);  // One of the above handlers should have handled
                             // the error already.
        return handle_error("failed to canonicalize path"s);
      });
}
}

namespace std {
template <>
struct hash<quick_lint_js::canonical_path> {
  using is_transparent = void;

  std::size_t operator()(const quick_lint_js::canonical_path &path) const
      noexcept {
    return std::hash<std::string_view>()(path.path());
  }

  std::size_t operator()(std::string_view path) const noexcept {
    return std::hash<std::string_view>()(path);
  }
};
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
