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

#ifndef QUICK_LINT_JS_WASM_DEMO_LOCATION_H
#define QUICK_LINT_JS_WASM_DEMO_LOCATION_H

#include <cstdint>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/padded-string.h>

namespace quick_lint_js {
class source_code_span;

using wasm_demo_source_offset = std::uint32_t;

struct wasm_demo_source_range {
  wasm_demo_source_offset begin;
  wasm_demo_source_offset end;
};

class wasm_demo_locator {
 public:
  explicit wasm_demo_locator(padded_string_view input) noexcept;

  wasm_demo_source_range range(source_code_span) const;
  wasm_demo_source_offset position(const char8*) const noexcept;

 private:
  padded_string_view input_;
};
}

#endif
