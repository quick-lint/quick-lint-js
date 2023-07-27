// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_IO_FILE_CANONICAL_H
#define QUICK_LINT_JS_IO_FILE_CANONICAL_H

#if defined(__EMSCRIPTEN__)
// No canonicalize_path on the web.
#else

#include <cstddef>
#include <optional>
#include <quick-lint-js/container/hash.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/io/file-handle.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
class Canonical_Path_Result;

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
// components before Canonical_Path_Result::drop_missing_components has been
// called.
class Canonical_Path {
 public:
  // Does not check the validity of the path.
  explicit Canonical_Path(std::string &&path);

  std::string_view path() const &;
  std::string &&path() &&;
  const char *c_str() const;

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

  friend bool operator==(const Canonical_Path &, const Canonical_Path &);
  friend bool operator!=(const Canonical_Path &, const Canonical_Path &);
  friend bool operator==(std::string_view, const Canonical_Path &);
  friend bool operator!=(std::string_view, const Canonical_Path &);
  friend bool operator==(const Canonical_Path &, std::string_view);
  friend bool operator!=(const Canonical_Path &, std::string_view);

 private:
  std::string path_;

  // On POSIX, if path_ is "/hello/world/x", then path_lengths_ is {1, 6, 12}:
  //
  //   /hello/world/x
  //    ^    ^     ^
  //    1    6     12
  //
  // If path_ is a root path, then path_lengths_ is empty.
  std::vector<std::size_t> path_lengths_;

  friend Canonical_Path_Result;
};

class Canonical_Path_Result {
 public:
  explicit Canonical_Path_Result(std::string &&path,
                                 std::size_t existing_path_length);

  std::string_view path() const &;
  std::string &&path() &&;
  const char *c_str() const;

  const Canonical_Path &canonical() const &;
  Canonical_Path &&canonical() &&;

  bool have_missing_components() const;
  void drop_missing_components();

 private:
  Canonical_Path path_;
  std::size_t existing_path_length_;
};

struct Canonicalize_Path_IO_Error {
  std::string input_path;
  std::string canonicalizing_path;
  Platform_File_IO_Error io_error;

  std::string to_string() const;

  friend bool operator==(const Canonicalize_Path_IO_Error &,
                         const Canonicalize_Path_IO_Error &);
  friend bool operator!=(const Canonicalize_Path_IO_Error &,
                         const Canonicalize_Path_IO_Error &);
};

class Canonicalize_Observer {
 public:
  virtual ~Canonicalize_Observer() = default;

  virtual void on_canonicalize_child_of_directory(const char *) = 0;
  virtual void on_canonicalize_child_of_directory(const wchar_t *) = 0;
};

Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
    const char *path);
Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
    const std::string &path);
Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
    const char *path, Canonicalize_Observer *);
Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
    const std::string &path, Canonicalize_Observer *);

template <>
struct Hasher<Canonical_Path> {
  using is_transparent = void;

  std::size_t operator()(const Canonical_Path &path) const {
    return std::hash<std::string_view>()(path.path());
  }

  std::size_t operator()(std::string_view path) const {
    return std::hash<std::string_view>()(path);
  }
};
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
