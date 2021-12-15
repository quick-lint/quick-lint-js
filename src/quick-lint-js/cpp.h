// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CPP_H
#define QUICK_LINT_JS_CPP_H

#define QLJS_CPP_CONCAT(x, y) QLJS_CPP_CONCAT_(x, y)
#define QLJS_CPP_CONCAT_(x, y) x##y

#define QLJS_CPP_QUOTE(x) QLJS_CPP_QUOTE_(x)
#define QLJS_CPP_QUOTE_(x) #x

#define QLJS_COUNT_ARGS(...) QLJS_COUNT_ARGS_(__VA_ARGS__, 5, 4, 3, 2, 1, 0)
#define QLJS_COUNT_ARGS_(_0, _1, _2, _3, _4, count, ...) count

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
