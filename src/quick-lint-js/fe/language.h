// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <iosfwd>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
enum class Statement_Kind {
  do_while_loop,
  for_loop,  // TODO(strager): c_style_for_loop + for_in_loop + for_of_loop?
  if_statement,
  while_loop,
  with_statement,
  labelled_statement,
};

std::ostream& operator<<(std::ostream&, Statement_Kind);

enum class Enum_Kind {
  declare_const_enum,
  const_enum,
  declare_enum,
  normal,
};

std::ostream& operator<<(std::ostream&, Enum_Kind);

enum class Variable_Kind : unsigned char {
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

// For debugging.
String8_View to_string(Variable_Kind);
std::ostream& operator<<(std::ostream&, Variable_Kind);

enum Variable_Declaration_Flags : unsigned char {
  none = 0,

  // Only valid for _const, _let, and _var.
  //
  // Examples set:
  //   let x = 42;
  //   const [x] = xs;
  //   for (var x = null in xs) {}
  // Examples unset:
  //   class C {}
  //   (param, defaultParam = null) => {}
  //   let x, y, z;
  //   for (let x of xs) {}
  initialized_with_equals = 1 << 0,

  // Only valid for _namespace.
  //
  // Examples set:
  //   namespace ns {;}
  //   namespace ns { export class C {} }
  // Examples unset:
  //   namespace ns {}
  //   module ns { namespace subns {} }
  //
  // See also NOTE[non-empty-namespace] and
  // parser::is_current_typescript_namespace_non_empty_.
  non_empty_namespace = 1 << 1,
};

enum class Function_Attributes {
  async,
  async_generator,
  generator,
  normal,
};

std::ostream& operator<<(std::ostream&, Function_Attributes);
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
