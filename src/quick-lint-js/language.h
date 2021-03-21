// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LANGUAGE_H
#define QUICK_LINT_JS_LANGUAGE_H

#include <iosfwd>

namespace quick_lint_js {
enum class variable_kind {
  _catch,
  _class,
  _const,
  _function,
  _import,
  _let,
  _parameter,
  _var,
};

std::ostream& operator<<(std::ostream&, variable_kind);

enum class function_attributes {
  async,
  async_generator,
  generator,
  normal,
};
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
