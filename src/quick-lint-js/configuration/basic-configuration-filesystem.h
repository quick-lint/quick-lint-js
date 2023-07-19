// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CONFIGURATION_BASIC_CONFIGURATION_FILESYSTEM_H
#define QUICK_LINT_JS_CONFIGURATION_BASIC_CONFIGURATION_FILESYSTEM_H

#if defined(__EMSCRIPTEN__)
// No filesystem on the web.
#else

#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file.h>
#include <string>

namespace quick_lint_js {
class Basic_Configuration_Filesystem : public Configuration_Filesystem {
 public:
  static Basic_Configuration_Filesystem* instance() noexcept;

  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
      const std::string&) override;
  Result<Padded_String, Read_File_IO_Error> read_file(
      const Canonical_Path&) override;
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
