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

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/options.h>
#include <vector>

extern "C" {
int LLVMFuzzerTestOneInput(const std::uint8_t *data, std::size_t size) {
  bool data_is_null_terminated = size > 0 && data[size - 1] == '\0';
  if (!data_is_null_terminated) {
    return 0;
  }

  std::vector<char *> argv;
  const std::uint8_t *p = data;
  const std::uint8_t *data_end = &data[size];
  while (p != data_end) {
    argv.emplace_back(const_cast<char *>(reinterpret_cast<const char *>(p)));
    p = std::find(p, data_end, '\0');
    QLJS_ASSERT(p != data_end);
    ++p;
  }
  QLJS_ASSERT(!argv.empty());

  quick_lint_js::options o = quick_lint_js::parse_options(
      quick_lint_js::narrow_cast<int>(argv.size()), argv.data());
  static_cast<void>(o);

  return 0;
}
}
