// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// See ADR001-Feature-testing-with-have-h.md for usage of and rationale for this
// file.

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

#if defined(QLJS_HAVE_FCNTL_H) && QLJS_HAVE_FCNTL_H
#elif defined(__has_include)
#if __has_include(<fcntl.h>)
#define QLJS_HAVE_FCNTL_H 1
#endif
#elif defined(__unix__)
#define QLJS_HAVE_FCNTL_H 1
#endif
#if !defined(QLJS_HAVE_FCNTL_H)
#define QLJS_HAVE_FCNTL_H 0
#endif

#if defined(QLJS_HAVE_LIBGEN_H) && QLJS_HAVE_LIBGEN_H
#elif defined(__has_include)
#if __has_include(<libgen.h>)
#define QLJS_HAVE_LIBGEN_H 1
#endif
#elif defined(__unix__)
#define QLJS_HAVE_LIBGEN_H 1
#endif
#if !defined(QLJS_HAVE_LIBGEN_H)
#define QLJS_HAVE_LIBGEN_H 0
#endif

#if defined(QLJS_HAVE_PTHREAD_H) && QLJS_HAVE_PTHREAD_H
#elif defined(__has_include)
#if __has_include(<pthread.h>)
#define QLJS_HAVE_PTHREAD_H 1
#endif
#elif defined(__unix__)
#define QLJS_HAVE_PTHREAD_H 1
#endif
#if !defined(QLJS_HAVE_PTHREAD_H)
#define QLJS_HAVE_PTHREAD_H 0
#endif

#if defined(QLJS_HAVE_SYS_STAT_H) && QLJS_HAVE_SYS_STAT_H
#elif defined(__has_include)
#if __has_include(<sys/stat.h>)
#define QLJS_HAVE_SYS_STAT_H 1
#endif
#elif defined(__unix__)
#define QLJS_HAVE_SYS_STAT_H 1
#endif
#if !defined(QLJS_HAVE_SYS_STAT_H)
#define QLJS_HAVE_SYS_STAT_H 0
#endif

#if defined(QLJS_HAVE_SYS_WAIT_H) && QLJS_HAVE_SYS_WAIT_H
#elif defined(__has_include)
#if __has_include(<sys/wait.h>)
#define QLJS_HAVE_SYS_WAIT_H 1
#endif
#elif defined(__unix__)
#define QLJS_HAVE_SYS_WAIT_H 1
#endif
#if !defined(QLJS_HAVE_SYS_WAIT_H)
#define QLJS_HAVE_SYS_WAIT_H 0
#endif

#if defined(QLJS_HAVE_UNISTD_H) && QLJS_HAVE_UNISTD_H
#elif defined(__has_include)
#if __has_include(<unistd.h>) && !defined(__EMSCRIPTEN__)
#define QLJS_HAVE_UNISTD_H 1
#endif
#elif defined(__unix__) && !defined(__EMSCRIPTEN__)
#define QLJS_HAVE_UNISTD_H 1
#endif
#if !defined(QLJS_HAVE_UNISTD_H)
#define QLJS_HAVE_UNISTD_H 0
#endif

#if !defined(QLJS_HAVE_WINDOWS_H)
#if defined(_WIN32)
#define QLJS_HAVE_WINDOWS_H 1
#else
#define QLJS_HAVE_WINDOWS_H 0
#endif
#endif

#if !defined(QLJS_HAVE_STD_FILESYSTEM)
#if defined(_WIN32)
#define QLJS_HAVE_STD_FILESYSTEM 1
#else
#define QLJS_HAVE_STD_FILESYSTEM 0
#endif
#endif

#if QLJS_HAVE_UNISTD_H
// Define _POSIX_VERSION.
#include <unistd.h>
#endif

#if !defined(QLJS_HAVE_DIRENT_H)
#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 200112L
#define QLJS_HAVE_DIRENT_H 1
#else
#define QLJS_HAVE_DIRENT_H 0
#endif
#endif

#if !defined(QLJS_HAVE_DIRNAME)
#if QLJS_HAVE_LIBGEN_H
#define QLJS_HAVE_DIRNAME 1
#else
#define QLJS_HAVE_DIRNAME 0
#endif
#endif

#if !defined(QLJS_HAVE_MKDTEMP)
#if defined(QLJS_HAVE_UNISTD_H) &&                             \
    ((defined(_POSIX_VERSION) && _POSIX_VERSION >= 200809L) || \
     ((defined(__APPLE__) || defined(__FreeBSD__)) &&          \
      defined(_POSIX_VERSION) && _POSIX_VERSION >= 200112L))
#define QLJS_HAVE_MKDTEMP 1
#else
#define QLJS_HAVE_MKDTEMP 0
#endif
#endif

#if !defined(QLJS_HAVE_MKFIFO)
#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 199009L
#define QLJS_HAVE_MKFIFO 1
#else
#define QLJS_HAVE_MKFIFO 0
#endif
#endif

#if !defined(QLJS_HAVE_GETPID)
#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 200112L
#define QLJS_HAVE_GETPID 1
#else
#define QLJS_HAVE_GETPID 0
#endif
#endif

#if !defined(QLJS_HAVE_GETTID)
#if defined(__linux__) && __GLIBC__ == 2 && __GLIBC_MINOR__ >= 30
#define QLJS_HAVE_GETTID 1
#else
#define QLJS_HAVE_GETTID 0
#endif
#endif

#if !defined(QLJS_HAVE_PIPE)
#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 200112L
#define QLJS_HAVE_PIPE 1
#else
#define QLJS_HAVE_PIPE 0
#endif
#endif

#if !defined(QLJS_HAVE_REALPATH)
#if defined(QLJS_HAVE_UNISTD_H) &&                             \
    ((defined(_POSIX_VERSION) && _POSIX_VERSION >= 200809L) || \
     ((defined(__APPLE__) || defined(__FreeBSD__)) &&          \
      defined(_POSIX_VERSION) && _POSIX_VERSION >= 200112L))
#define QLJS_HAVE_REALPATH 1
#else
#define QLJS_HAVE_REALPATH 0
#endif
#endif

#if !defined(QLJS_HAVE_POLL)
#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 200112L
#define QLJS_HAVE_POLL 1
#else
#define QLJS_HAVE_POLL 0
#endif
#endif

#if !defined(QLJS_HAVE_SETRLIMIT)
#if defined(QLJS_HAVE_UNISTD_H) &&                             \
    ((defined(_POSIX_VERSION) && _POSIX_VERSION >= 200809L) || \
     ((defined(__APPLE__) || defined(__FreeBSD__)) &&          \
      defined(_POSIX_VERSION) && _POSIX_VERSION >= 200112L))
#define QLJS_HAVE_SETRLIMIT 1
#else
#define QLJS_HAVE_SETRLIMIT 0
#endif
#endif

#if !defined(QLJS_HAVE_UNAME)
#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 198808L
#define QLJS_HAVE_UNAME 1
#else
#define QLJS_HAVE_UNAME 0
#endif
#endif

#if !defined(QLJS_HAVE_WRITEV)
#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 200112L
#define QLJS_HAVE_WRITEV 1
#else
#define QLJS_HAVE_WRITEV 0
#endif
#endif

#if !defined(QLJS_HAVE_CHARCONV_HEADER) && defined(__has_include)
// std::to_chars on libc++ version 7.0.0 is buggy on macOS x86_64.
#if __has_include(<charconv>) && \
    !(defined(_LIBCPP_VERSION) && _LIBCPP_VERSION <= 7000)
#define QLJS_HAVE_CHARCONV_HEADER 1
#endif
#endif
#if !defined(QLJS_HAVE_CHARCONV_HEADER)
#define QLJS_HAVE_CHARCONV_HEADER 0
#endif

#if !defined(QLJS_HAVE_ARM_NEON)
#if defined(__ARM_NEON)
#define QLJS_HAVE_ARM_NEON 1
#else
#define QLJS_HAVE_ARM_NEON 0
#endif
#endif

#if !defined(QLJS_HAVE_X86_SSE2)
#if defined(_M_AMD64) || defined(_M_X64) || \
    (defined(_M_IX86_FP) && _M_IX86_FP == 2) || defined(__SSE2__)
#define QLJS_HAVE_X86_SSE2 1
#else
#define QLJS_HAVE_X86_SSE2 0
#endif
#endif

// TODO(strager): Check for SSE4.2 support in MSVC.
#if !defined(QLJS_HAVE_X86_SSE4_2)
#if defined(__SSE4_2__)
#define QLJS_HAVE_X86_SSE4_2 1
#else
#define QLJS_HAVE_X86_SSE4_2 0
#endif
#endif

#if !defined(QLJS_HAVE_CHAR8_T)
#if defined(__cpp_char8_t) && __cpp_char8_t >= 201803L
#define QLJS_HAVE_CHAR8_T 1
#else
#define QLJS_HAVE_CHAR8_T 0
#endif
#endif

#if !defined(QLJS_HAVE_DEBUGBREAK)
#if defined(_WIN32) && defined(__has_include)
#if __has_include(<intrin.h>)
#define QLJS_HAVE_DEBUGBREAK 1
#endif
#endif
#endif
#if !defined(QLJS_HAVE_DEBUGBREAK)
#define QLJS_HAVE_DEBUGBREAK 0
#endif

#if !defined(QLJS_HAVE_BUILTIN_TRAP)
#if defined(__GNUC__) || defined(__clang__)
#define QLJS_HAVE_BUILTIN_TRAP 1
#else
#define QLJS_HAVE_BUILTIN_TRAP 0
#endif
#endif

#if !defined(QLJS_HAVE_CXX_CONCEPTS)
#if defined(__cpp_concepts) && __cpp_concepts >= 201907L
#define QLJS_HAVE_CXX_CONCEPTS 1
#else
#define QLJS_HAVE_CXX_CONCEPTS 0
#endif
#endif

#if !defined(QLJS_HAVE_F_GETPIPE_SZ)
#if defined(__linux__)
#define QLJS_HAVE_F_GETPIPE_SZ 1
#else
#define QLJS_HAVE_F_GETPIPE_SZ 0
#endif
#endif

#if !defined(QLJS_HAVE_PTHREAD_KILL)
#if QLJS_HAVE_PTHREAD_H
#define QLJS_HAVE_PTHREAD_KILL 1
#else
#define QLJS_HAVE_PTHREAD_KILL 0
#endif
#endif

#include <locale.h>

#if !defined(QLJS_HAVE_LC_MESSAGES)
#if defined(LC_MESSAGES)
#define QLJS_HAVE_LC_MESSAGES 1
#else
#define QLJS_HAVE_LC_MESSAGES 0
#endif
#endif

#if !defined(QLJS_HAVE_BUILTIN_FILE_FUNCTION_LINE)
#if defined(__has_builtin)
#if __has_builtin(__builtin_FILE) && __has_builtin(__builtin_FUNCTION) && \
    __has_builtin(__builtin_LINE)
#define QLJS_HAVE_BUILTIN_FILE_FUNCTION_LINE 1
#else
#define QLJS_HAVE_BUILTIN_FILE_FUNCTION_LINE 0
#endif
#else
#define QLJS_HAVE_BUILTIN_FILE_FUNCTION_LINE 0
#endif
#endif

#if !defined(QLJS_HAVE_FTS_H) && defined(__has_include)
#if __has_include(<fts.h>)
#define QLJS_HAVE_FTS_H 1
#endif
#endif
#if !defined(QLJS_HAVE_FTS_H)
#define QLJS_HAVE_FTS_H 0
#endif

#if !defined(QLJS_HAVE_SETJMP) && defined(__has_include)
#if !defined(__EMSCRIPTEN__) && __has_include(<csetjmp>)
#define QLJS_HAVE_SETJMP 1
#endif
#endif
#if !defined(QLJS_HAVE_SETJMP)
#define QLJS_HAVE_SETJMP 0
#endif

#if !defined(QLJS_HAVE_INOTIFY)
#if defined(__linux__)
#define QLJS_HAVE_INOTIFY 1
#else
#define QLJS_HAVE_INOTIFY 0
#endif
#endif

#if !defined(QLJS_HAVE_KQUEUE)
#if defined(__APPLE__) || defined(__FreeBSD__)
#define QLJS_HAVE_KQUEUE 1
#else
#define QLJS_HAVE_KQUEUE 0
#endif
#endif

#if !defined(QLJS_HAVE_STD_TRANSPARENT_KEYS)
// TODO(strager): Set this to 1 if is_transparent is supported by
// std::unordered_map::find (C++20).
#define QLJS_HAVE_STD_TRANSPARENT_KEYS 0
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
