// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_MAX_ALIGN_H
#define QUICK_LINT_JS_PORT_MAX_ALIGN_H

#include <cstddef>

namespace quick_lint_js {
#if defined(__GNUC__) && defined(__MINGW32__)
// HACK(strager): In 32-bit MinGW (Windows), GCC's stddef.h
// and MinGW's stddef.h have different definitions of
// ::max_align_t which have different alignment [1]. This
// violates ODR and causes nasty things to happen. Work
// around this bug by using our own definition of
// max_align_t.
//
// [1] https://sourceforge.net/p/mingw-w64/bugs/778/
//     https://sourceforge.net/p/mingw-w64/bugs/779/
//     https://github.com/rohlem/gcc-max_align_t-bug-repro
//     https://www.mail-archive.com/mingw-w64-public@lists.sourceforge.net/msg17995.html
union max_align_t {
  long double ld;
  long long ll;
};
#else
using max_align_t = std::max_align_t;
#endif

#if defined(_WIN32) && defined(__i386__)
static_assert(
    alignof(max_align_t) <= 8,
    "malloc only guarantees 8-byte-aligned pointers on Windows x86 (32-bit)");
#endif
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
