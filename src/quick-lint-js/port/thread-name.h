// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstddef>
#include <quick-lint-js/port/char8.h>

#if defined(__APPLE__)
#include <libproc.h>
#endif

#if defined(__FreeBSD__)
#include <sys/param.h>
#include <sys/user.h>
#endif

namespace quick_lint_js {
// max_thread_name_length is the number of UTF-8 code units of the thread name,
// excluding the null terminator.
#if defined(__linux__)
inline constexpr std::size_t max_thread_name_length = 15;
#define QLJS_CAN_SET_THREAD_NAMES 1
#endif
#if defined(__APPLE__)
inline constexpr std::size_t max_thread_name_length = MAXTHREADNAMESIZE - 1;
#define QLJS_CAN_SET_THREAD_NAMES 1
#endif
#if defined(__FreeBSD__)
// NOTE(Nico): This seems to be the limit defined in the kernel. It is extremely
//             small so we choose a very short prefix like on Linux. (see
//             sys/sys/proc.h:300, struct thread). Then we find another limit
//             TDNAMLEN which is defined in sys/user.h and using the struct
//             kinfo_proc. The following assert assures that TDNAMLEN isn't
//             bigger than what we use here.
inline constexpr std::size_t max_thread_name_length = MAXCOMLEN;
static_assert(max_thread_name_length >= TDNAMLEN,
              "sys/user.h defines TDNAMLEN to be bigger than MAXCOMLEN. "
              "Your headers are broken.");
#define QLJS_CAN_SET_THREAD_NAMES 1
#endif
#if defined(_WIN32)
// Thread names on Windows can seemingly be any length. Pick a reasonable limit
// for ourselves.
inline constexpr std::size_t max_thread_name_length = 256;
#define QLJS_CAN_SET_THREAD_NAMES 1
#endif

#if !defined(QLJS_CAN_SET_THREAD_NAMES)
#define QLJS_CAN_SET_THREAD_NAMES 0
#endif

#if QLJS_CAN_SET_THREAD_NAMES
// The lowest (most conservative) value of max_thread_name_length on any
// platform.
//
// Invariant: lowest_max_thread_name_length <= max_thread_name_length
inline constexpr std::size_t lowest_max_thread_name_length = 15;
#endif

// Change the name of the current thread.
//
// Logs an error if there was an error.
//
// Precondition: new_name is null-terminated.
// Precondition: strlen(new_name) <= max_thread_name_length
void set_current_thread_name(const Char8* new_name);

// Change the name of the current thread.
//
// long_name is preferred. If long_name is too long of a thread name for the
// current platform, short_name is used instead.
//
// Precondition: long_name is null-terminated.
// Precondition: short_name is null-terminated.
// Precondition: strlen(short_name) <= lowest_max_thread_name_length
void set_current_thread_name(const Char8* long_name, const Char8* short_name);
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
