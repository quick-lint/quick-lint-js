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

#ifndef QUICK_LINT_JS_HAVE_H
#define QUICK_LINT_JS_HAVE_H

#if defined(QLJS_HAVE_VERSION_HEADER) && QLJS_HAVE_VERSION_HEADER
#elif defined(__has_include)
#if __has_include(<version>)
#define QLJS_HAVE_VERSION_HEADER 1
#endif
#endif
#if !defined(QLJS_HAVE_VERSION_HEADER)
#define QLJS_HAVE_VERSION_HEADER 0
#endif

#if QLJS_HAVE_VERSION_HEADER
#include <version>
#endif

#if defined(QLJS_HAVE_UNISTD_H) && QLJS_HAVE_UNISTD_H
#elif defined(__has_include)
#if __has_include(<unistd.h>)
#define QLJS_HAVE_UNISTD_H 1
#endif
#elif defined(__unix__)
#define QLJS_HAVE_UNISTD_H 1
#endif
#if !defined(QLJS_HAVE_UNISTD_H)
#define QLJS_HAVE_UNISTD_H 0
#endif

#if defined(QLJS_HAVE_FILESYSTEM_HEADER) && QLJS_HAVE_FILESYSTEM_HEADER
#elif defined(__has_include)
#if __has_include(<filesystem>)
#define QLJS_HAVE_FILESYSTEM_HEADER 1
#endif
#endif
#if !defined(QLJS_HAVE_FILESYSTEM_HEADER)
#define QLJS_HAVE_FILESYSTEM_HEADER 0
#endif

#if defined(QLJS_HAVE_EXPERIMENTAL_FILESYSTEM_HEADER) && \
    QLJS_HAVE_EXPERIMENTAL_FILESYSTEM_HEADER
#elif defined(__has_include)
#if __has_include(<experimental/filesystem>)
#define QLJS_HAVE_EXPERIMENTAL_FILESYSTEM_HEADER 1
#endif
#endif
#if !defined(QLJS_HAVE_EXPERIMENTAL_FILESYSTEM_HEADER)
#define QLJS_HAVE_EXPERIMENTAL_FILESYSTEM_HEADER 0
#endif

#if QLJS_HAVE_UNISTD_H
// Define _POSIX_VERSION.
#include <unistd.h>
#endif

#if !defined(QLJS_HAVE_MKDTEMP)
#if (defined(_POSIX_VERSION) && _POSIX_VERSION >= 200809L) || \
    (defined(__APPLE__) && defined(_POSIX_VERSION) &&         \
     _POSIX_VERSION >= 200112L)
#define QLJS_HAVE_MKDTEMP 1
#else
#define QLJS_HAVE_MKDTEMP 0
#endif
#endif

#if !defined(QLJS_HAVE_MKFIFO)
#if defined(_POSIX_VERSION) && _POSIX_VERSION >= 199009L
#define QLJS_HAVE_MKFIFO 1
#else
#define QLJS_HAVE_MKFIFO 0
#endif
#endif

#if !defined(QLJS_HAVE_CHARCONV_HEADER) && defined(__has_include)
#if __has_include(<charconv>)
#define QLJS_HAVE_CHARCONV_HEADER 1
#endif
#endif
#if !defined(QLJS_HAVE_CHARCONV_HEADER)
#define QLJS_HAVE_CHARCONV_HEADER 0
#endif

#if !defined(QLJS_HAVE_X86_SSE2)
#if defined(_M_AMD64) || defined(_M_X64) || \
    (defined(_M_IX86_FP) && _M_IX86_FP == 2) || defined(__SSE2__)
#define QLJS_HAVE_X86_SSE2 1
#else
#define QLJS_HAVE_X86_SSE2 0
#endif
#endif

#if !defined(QLJS_HAVE_CHAR8_T)
#if defined(__cpp_char8_t) && __cpp_char8_t >= 201803L
#define QLJS_HAVE_CHAR8_T 1
#else
#define QLJS_HAVE_CHAR8_T 0
#endif
#endif

#endif
