// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/padded-string.h>

extern "C" {
int LLVMFuzzerTestOneInput(const std::uint8_t *data, std::size_t size) {
  using namespace quick_lint_js;

  padded_string json(string8(reinterpret_cast<const char8 *>(data), size));
  configuration c;
  c.load_from_json(&json);

  return 0;
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
