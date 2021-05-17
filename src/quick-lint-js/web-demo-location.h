// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_WEB_DEMO_LOCATION_H
#define QUICK_LINT_JS_WEB_DEMO_LOCATION_H

#include <cstdint>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
class source_code_span;

using web_demo_source_offset = std::uint32_t;

struct web_demo_source_range {
  web_demo_source_offset begin;
  web_demo_source_offset end;
};

class web_demo_locator {
 public:
  explicit web_demo_locator(padded_string_view input) noexcept;

  web_demo_source_range range(source_code_span) const;
  web_demo_source_offset position(const char8*) const noexcept;

 private:
  padded_string_view input_;
};
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
