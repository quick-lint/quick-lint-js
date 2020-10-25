// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef QUICK_LINT_JS_VERSION_H
#define QUICK_LINT_JS_VERSION_H

#include <quick-lint-js/cpp.h>

#define QUICK_LINT_JS_VERSION_STRING "0.1.0"
#define QUICK_LINT_JS_VERSION_STRING_U8 \
  QLJS_CPP_CONCAT(u8, QUICK_LINT_JS_VERSION_STRING)

#endif
