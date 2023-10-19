// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
struct Path_Classification {
  // True if the path has '.d.' in the base name.
  //
  // This tries to emulate the logic of TypeScript's isDeclarationFileName
  // function [1]. However, this does not require ".ts". It will be true for an
  // URI such as u8"file:///test.d.js".
  //
  // [1]
  // https://github.com/microsoft/TypeScript/blob/daa7e985f5adc972aa241e5b0761c7dc433e94bf/src/compiler/parser.ts#L10408
  bool typescript_definition;

  // True if the path's base name ends with '.tsx'.
  bool typescript_jsx;
};

Path_Classification classify_uri(String8_View uri);
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
