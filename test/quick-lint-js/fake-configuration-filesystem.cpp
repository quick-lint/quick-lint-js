// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No filesystem on the web.
#else

#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/heap-function.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/fake-configuration-filesystem.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/char8.h>
#include <string>
#include <unordered_map>
#include <utility>

namespace quick_lint_js {
Fake_Configuration_Filesystem::Fake_Configuration_Filesystem() = default;

Fake_Configuration_Filesystem::~Fake_Configuration_Filesystem() = default;

void Fake_Configuration_Filesystem::create_file(const Canonical_Path& path,
                                                String8_View content) {
  Span<Char8> content_copy =
      this->allocator_.allocate_uninitialized_span<Char8>(content.size());
  std::uninitialized_copy(content.begin(), content.end(), content_copy.data());
  String8_View content_copy_view(content_copy.data(),
                                 narrow_cast<std::size_t>(content_copy.size()));
  this->files_.insert_or_assign(path,
                                *this->allocator_.new_object_copy(
                                    [content_copy_view]() -> Read_File_Result {
                                      return Padded_String(content_copy_view);
                                    }));
}

void Fake_Configuration_Filesystem::create_file(
    const Canonical_Path& path,
    Async_Function_Ref<Read_File_Result()> callback) {
  this->files_.insert_or_assign(path, callback);
}

Canonical_Path Fake_Configuration_Filesystem::rooted(const char* path) const {
  std::string full_path;
#if defined(_WIN32)
  full_path = "X:\\";
#else
  full_path = "/";
#endif
  full_path += path;
#if defined(_WIN32)
  for (char& c : full_path) {
    if (c == '/') {
      c = '\\';
    }
  }
#endif
  return Canonical_Path(std::move(full_path));
}

String8 Fake_Configuration_Filesystem::file_uri_prefix_8() const {
#if defined(_WIN32)
  return u8"file:///X:/";
#else
  return u8"file:///";
#endif
}

Result<Canonical_Path_Result, Canonicalize_Path_IO_Error>
Fake_Configuration_Filesystem::canonicalize_path(const std::string& path) {
  // TODO(strager): Check if path components exist.
  return Canonical_Path_Result(std::string(path), path.size());
}

Result<Padded_String, Read_File_IO_Error>
Fake_Configuration_Filesystem::read_file(const Canonical_Path& path) {
  auto file_it = this->files_.find(path);
  if (file_it == this->files_.end()) {
#if QLJS_HAVE_WINDOWS_H
    Windows_File_IO_Error io_error = {ERROR_FILE_NOT_FOUND};
#endif
#if QLJS_HAVE_UNISTD_H
    POSIX_File_IO_Error io_error = {ENOENT};
#endif
    return failed_result(Read_File_IO_Error{
        .path = std::string(path.path()),
        .io_error = io_error,
    });
  }
  return file_it->second();
}

void Fake_Configuration_Filesystem::clear() { this->files_.clear(); }
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
