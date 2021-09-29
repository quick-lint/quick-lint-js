// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <quick-lint-js/change-detecting-filesystem.h>
#include <quick-lint-js/file-handle.h>
#include <string>

using namespace std::literals::string_literals;

namespace quick_lint_js {
std::string watch_io_error::to_string() const {
  return "failed to watch "s + this->path + " for changes: "s +
         this->io_error.to_string();
}

bool operator==(const watch_io_error& lhs, const watch_io_error& rhs) noexcept {
  return lhs.path == rhs.path && lhs.io_error == rhs.io_error;
}

bool operator!=(const watch_io_error& lhs, const watch_io_error& rhs) noexcept {
  return !(lhs == rhs);
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
