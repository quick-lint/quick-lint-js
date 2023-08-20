// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

// These forward declarations let us avoid #include-ing <simdjson.h> in our
// headers. This reduces compile times.

#if defined(SIMDJSON_BUILTIN_IMPLEMENTATION)
#define QUICK_LINT_JS_SIMDJSON_IMPLEMENTATION_NAMESPACE \
  SIMDJSON_BUILTIN_IMPLEMENTATION
#elif defined(__aarch64__) || defined(_M_ARM64)
#define QUICK_LINT_JS_SIMDJSON_IMPLEMENTATION_NAMESPACE arm64
// TODO(strager): Check x86 too.
#endif

#if defined(QUICK_LINT_JS_SIMDJSON_IMPLEMENTATION_NAMESPACE)
namespace simdjson {
template <class T>
class simdjson_result;

namespace QUICK_LINT_JS_SIMDJSON_IMPLEMENTATION_NAMESPACE {
namespace ondemand {
class array;
class object;
class parser;
class value;
}
}

namespace ondemand = QUICK_LINT_JS_SIMDJSON_IMPLEMENTATION_NAMESPACE::ondemand;
}
#else
// We don't know which namespace simdjson will select. Don't try to guess; just
// use simdjson's declarations.
#include <simdjson.h>
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
