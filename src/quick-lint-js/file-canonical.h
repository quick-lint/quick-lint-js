// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILE_CANONICAL_H
#define QUICK_LINT_JS_FILE_CANONICAL_H

#include <cstddef>
#include <functional>
#include <optional>
#include <string>
#include <string_view>

namespace quick_lint_js {
// A filesystem path.
//
// * The path is absolute (i.e. not relative to the current working directory or
//   current drive). (The path is relative to the current chroot/jail/namespace,
//   though.)
// * The path has no '.' or '..' components.
// * The path has no redundant component separators (\ or /, depending on the
//   operating system).
// * No subpath refers to a symlink, assuming no changes to the filesystem since
//   creation of the canonical_path.
class canonical_path {
 public:
  // Does not check the validity of the path.
  explicit canonical_path(std::string &&path);

  std::string_view path() const &noexcept;
  std::string &&path() && noexcept;
  const char *c_str() const noexcept;

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
};

class canonical_path_result {
 public:
  explicit canonical_path_result(std::string &&path);
  explicit canonical_path_result(const char *path);

  std::string_view path() const &noexcept;
  std::string &&path() && noexcept;
  const char *c_str() const noexcept;

  const canonical_path &canonical() const &noexcept;
  canonical_path &&canonical() && noexcept;

  std::string &&error() && noexcept;

  bool ok() const noexcept { return this->error_.empty(); }

  static canonical_path_result failure(std::string &&error);

 private:
  explicit canonical_path_result();

  std::optional<canonical_path> path_;
  std::string error_;
};

canonical_path_result canonicalize_path(const char *path);
canonical_path_result canonicalize_path(const std::string &path);
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
