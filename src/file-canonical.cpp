// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/have.h>
#include <string>

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

#if QLJS_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

namespace quick_lint_js {
canonical_path_result::canonical_path_result() {}

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

canonical_path_result canonicalize_path(const char *path) {
#if QLJS_HAVE_STD_FILESYSTEM
  std::error_code error;
  std::filesystem::path canonical = std::filesystem::canonical(path, error);
  if (error) {
    return canonical_path_result::failure(
        std::string("failed to canonicalize path ") + path + ": " +
        error.message());
  }
  return canonical_path_result(canonical.string().c_str());
#elif QLJS_HAVE_REALPATH
  char *allocated_canonical = ::realpath(path, nullptr);
  if (!allocated_canonical) {
    return canonical_path_result::failure(
        std::string("failed to canonicalize path ") + path + ": " +
        std::strerror(errno));
  }
  canonical_path_result result(allocated_canonical);
  std::free(allocated_canonical);
  return result;
#else
#error "Unsupported platform"
#endif
}

canonical_path_result canonicalize_path(const std::string &path) {
  return canonicalize_path(path.c_str());
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
