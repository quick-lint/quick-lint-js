// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/have.h>

#if QLJS_HAVE_WINDOWS_H

#include <cstddef>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/windows.h>
#include <string>

namespace quick_lint_js {
std::string windows_error_message(DWORD error) {
  // TODO(strager): Use FormatMessageW.
  LPSTR get_last_error_message;
  DWORD get_last_error_message_length = ::FormatMessageA(
      /*dwFlags=*/FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
          FORMAT_MESSAGE_IGNORE_INSERTS,
      /*lpSource=*/nullptr,
      /*dwMessageId=*/error,
      /*dwLanguageId=*/0,
      /*lpBuffer=*/reinterpret_cast<LPSTR>(&get_last_error_message),
      /*nSize=*/(std::numeric_limits<DWORD>::max)(),
      /*Arguments=*/nullptr);
  if (get_last_error_message_length == 0) {
    // FormatMessageA failed.
    return "unknown error";
  }

  std::string_view message(
      get_last_error_message,
      narrow_cast<std::size_t>(get_last_error_message_length));
  message = remove_suffix_if_present(message, "\r\n");
  std::string message_copy(message);
  static_cast<void>(::LocalFree(get_last_error_message));
  return message_copy;
}

std::string windows_last_error_message() {
  return windows_error_message(::GetLastError());
}
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
