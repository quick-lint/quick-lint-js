// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/thread-name.h>
#include <quick-lint-js/util/utf-16.h>

#if defined(__linux__)
#include <sys/prctl.h>
#endif

#if defined(__APPLE__)
#include <pthread.h>
#endif

#if defined(__FreeBSD__)
#include <pthread.h>
#include <pthread_np.h>
#endif

#if defined(_WIN32)
#include <quick-lint-js/port/windows.h>
#include <psapi.h>
#endif

namespace quick_lint_js {
#if QLJS_CAN_SET_THREAD_NAMES
static_assert(lowest_max_thread_name_length <= max_thread_name_length);
#endif

void set_current_thread_name(const Char8* new_name) {
  [[maybe_unused]] const char* new_name_cstr =
      reinterpret_cast<const char*>(new_name);

#if defined(__linux__)
  QLJS_ASSERT(std::strlen(new_name_cstr) <= max_thread_name_length);
  int rc = ::prctl(PR_SET_NAME, reinterpret_cast<std::uintptr_t>(new_name_cstr),
                   0, 0, 0);
  if (rc != 0) {
    QLJS_DEBUG_LOG(
        "%s: ignoring failure to set thread name for debug server thread: %s\n",
        __func__, std::strerror(errno));
    return;
  }
#endif

#if defined(__APPLE__)
  QLJS_ASSERT(std::strlen(new_name_cstr) <= max_thread_name_length);
  int rc = ::pthread_setname_np(new_name_cstr);
  if (rc != 0) {
    QLJS_DEBUG_LOG(
        "%s: ignoring failure to set thread name for debug server thread: %s\n",
        __func__, std::strerror(errno));
    return;
  }
#endif

#if defined(__FreeBSD__)
  QLJS_ASSERT(std::strlen(new_name_cstr) <= max_thread_name_length);
  int rc = ::pthread_setname_np(::pthread_self(), new_name_cstr);
  if (rc != 0) {
    QLJS_DEBUG_LOG(
        "%s: ignoring failure to set thread name for debug server thread: %s\n",
        __func__, std::strerror(rc));
    return;
  }
#endif

#if defined(_WIN32)
  // No need to check against max_thread_name_length. ::SetThreadDescription
  // imposes no limit.

  std::optional<std::wstring> name_unicode = mbstring_to_wstring(new_name_cstr);
  if (!name_unicode.has_value()) {
    QLJS_DEBUG_LOG("%s: ignoring failure to convert thread name to UTF-16\n",
                   __func__);
    return;
  }
  ::HRESULT rc =
      ::SetThreadDescription(::GetCurrentThread(), name_unicode->c_str());
  if (FAILED(rc)) {
    QLJS_DEBUG_LOG(
        "%s: ignoring failure to set thread name for debug server thread: "
        "%#08lx\n",
        __func__, rc);
    return;
  }
#endif
}

void set_current_thread_name([[maybe_unused]] const Char8* long_name,
                             [[maybe_unused]] const Char8* short_name) {
#if QLJS_CAN_SET_THREAD_NAMES
  QLJS_ASSERT(std::strlen(reinterpret_cast<const char*>(short_name)) <=
              lowest_max_thread_name_length);
  if (std::strlen(reinterpret_cast<const char*>(long_name)) <=
      max_thread_name_length) {
    set_current_thread_name(long_name);
  } else {
    set_current_thread_name(short_name);
  }
#endif
}
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
