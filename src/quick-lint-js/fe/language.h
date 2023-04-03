// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_LANGUAGE_H
#define QUICK_LINT_JS_FE_LANGUAGE_H

#include <iosfwd>

namespace quick_lint_js {
enum class statement_kind {
  do_while_loop,
  for_loop,  // TODO(strager): c_style_for_loop + for_in_loop + for_of_loop?
  if_statement,
  while_loop,
  with_statement,
  labelled_statement,
};

enum class enum_kind {
  declare_const_enum,
  const_enum,
  declare_enum,
  normal,
};

enum class variable_kind {
  _arrow_parameter,
  _catch,
  _class,
  _const,
  _enum,  // TypeScript only
  _function,
  _function_parameter,       // Non-arrow parameter
  _function_type_parameter,  // TypeScript only
  _generic_parameter,        // TypeScript only
  _import,
  _import_alias,               // TypeScript only
  _import_type,                // TypeScript only
  _index_signature_parameter,  // TypeScript only
  _infer_type,                 // TypeScript only
  _interface,                  // TypeScript only
  _let,
  _namespace,   // TypeScript only
  _type_alias,  // TypeScript only
  _var,
};

enum class variable_init_kind {
  // Examples:
  //   class C {}
  //   (param, defaultParam = null) => {}
  //   let x, y, z;
  //   for (let x of xs) {}
  normal,

  // Only valid for _const, _let, and _var.
  //
  // Examples:
  //   let x = 42;
  //   const [x] = xs;
  //   for (var x = null in xs) {}
  initialized_with_equals,
};

std::ostream& operator<<(std::ostream&, variable_kind);

enum class function_attributes {
  async,
  async_generator,
  generator,
  normal,
};

std::ostream& operator<<(std::ostream&, function_attributes);
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
