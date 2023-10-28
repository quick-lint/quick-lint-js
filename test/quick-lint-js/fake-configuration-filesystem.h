// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No filesystem on the web.
#else

#include <quick-lint-js/assert.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/function-ref.h>
#include <string>
#include <utility>

namespace quick_lint_js {
class Fake_Configuration_Filesystem : public Configuration_Filesystem {
 public:
  using Read_File_Result = Result<Padded_String, Read_File_IO_Error>;

  explicit Fake_Configuration_Filesystem();
  ~Fake_Configuration_Filesystem() override;

  // Create a new file, or modify an existing file.
  void create_file(const Canonical_Path& path, String8_View content);
  void create_file(const Canonical_Path& path,
                   Async_Function_Ref<Read_File_Result()> callback);

  Canonical_Path rooted(const char* path) const;

  String8 file_uri_prefix_8() const;

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
      const std::string& path) override;

  Result<Padded_String, Read_File_IO_Error> read_file(
      const Canonical_Path& path) override;

  void clear();

 private:
  Monotonic_Allocator allocator_{"Fake_Configuration_Filesystem"};
  Hash_Map<Canonical_Path, Async_Function_Ref<Read_File_Result()>> files_;
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
