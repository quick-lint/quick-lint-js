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
#include <ios>
#include <iostream>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/narrow-cast.h>
#include <string>

namespace quick_lint_js {
namespace {
void read_file_buffered(FILE *file, std::size_t buffer_size, string8 *out) {
  while (!std::feof(file)) {
    std::size_t size_before = out->size();
    out->resize(size_before + buffer_size);
    std::size_t read_size =
        std::fread(&out->data()[size_before], 1, buffer_size, file);
    out->resize(size_before + read_size);
  }
}

string8 read_file_buffered(FILE *file, std::size_t buffer_size) {
  string8 result;
  read_file_buffered(file, buffer_size, &result);
  return result;
}

string8 read_file_with_expected_size(FILE *file, std::size_t file_size,
                                     std::size_t buffer_size) {
  string8 result;
  result.resize(file_size);
  std::size_t read_size = std::fread(result.data(), 1, file_size, file);
  result.resize(read_size);
  int c = std::fgetc(file);
  if (c == EOF) {
    // We read the entire file.
    return result;
  } else {
    // We did not read the entire file. There is more data to read.
    [[maybe_unused]] int rc = std::ungetc(c, file);
    assert(rc == c);

    read_file_buffered(file, buffer_size, &result);
    return result;
  }
}

string8 read_file(const char *path, FILE *file) {
  std::size_t buffer_size = 1024;  // TODO(strager): Compute using stat.
  if (std::fseek(file, 0, SEEK_END) == -1) {
    return read_file_buffered(file, buffer_size);
  } else {
    long file_size = std::ftell(file);
    if (file_size == -1) {
      std::cerr << "error: failed to get size of " << path << ": "
                << std::strerror(errno) << '\n';
      exit(1);
    }
    // TODO(strager): Fail if file_size exceeds std::size_t.

    if (std::fseek(file, 0, SEEK_SET) == -1) {
      std::cerr << "error: failed to seek to beginning of " << path << ": "
                << std::strerror(errno) << '\n';
      exit(1);
    }

    return read_file_with_expected_size(
        file, /*file_size=*/narrow_cast<std::size_t>(file_size),
        /*buffer_size=*/buffer_size);
  }
}
}  // namespace

string8 read_file(const char *path) {
  FILE *file = std::fopen(path, "rb");
  if (!file) {
    std::cerr << "error: failed to open " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  string8 contents = read_file(path, file);

  if (std::fclose(file) == -1) {
    std::cerr << "error: failed to close " << path << ": "
              << std::strerror(errno) << '\n';
    exit(1);
  }

  return contents;
}
}  // namespace quick_lint_js
