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

#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>

namespace quick_lint_js {
std::string read_file(const char *path) {
  FILE *file = std::fopen(path, "rb");
  if (!file) {
    std::cerr << "error: failed to open " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  if (std::fseek(file, 0, SEEK_END) == -1) {
    std::cerr << "error: failed to seek to end of " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  long file_size = std::ftell(file);
  if (file_size == -1) {
    std::cerr << "error: failed to get size of " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  if (std::fseek(file, 0, SEEK_SET) == -1) {
    std::cerr << "error: failed to seek to beginning of " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  std::string contents;
  contents.resize(file_size);
  std::size_t read_size = std::fread(contents.data(), 1, file_size, file);
  contents.resize(read_size);

  if (std::fclose(file) == -1) {
    std::cerr << "error: failed to close " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  return contents;
}
}  // namespace quick_lint_js
