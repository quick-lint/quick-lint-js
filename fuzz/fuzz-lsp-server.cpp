// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/lsp/lsp-json-rpc-message-parser.h>
#include <quick-lint-js/lsp/lsp-server.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
namespace {
class null_lsp_endpoint_remote {
 public:
  void send_message(const byte_buffer&) {}
};

class null_configuration_filesystem : public configuration_filesystem {
 public:
  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string& path) override {
    return canonical_path_result(std::string(path), /*existing_path_length=*/0);
  }

  result<padded_string, read_file_io_error> read_file(
      const canonical_path& path) override {
#if QLJS_HAVE_WINDOWS_H
    windows_file_io_error io_error = {ERROR_FILE_NOT_FOUND};
#endif
#if QLJS_HAVE_UNISTD_H
    posix_file_io_error io_error = {ENOENT};
#endif
    return failed_result(read_file_io_error{
        .path = path.c_str(),
        .io_error = io_error,
    });
  }
};
}
}

extern "C" {
int LLVMFuzzerTestOneInput(const std::uint8_t* data, std::size_t size) {
  using namespace quick_lint_js;

  null_configuration_filesystem fs;
  lsp_javascript_linter linter;
  lsp_endpoint<linting_lsp_server_handler, null_lsp_endpoint_remote> server(
      std::forward_as_tuple(&fs, &linter), std::forward_as_tuple());

  std::size_t i = 0;
  auto size_remaining = [&]() -> std::size_t { return size - i; };
  for (;;) {
    std::size_t chunk_size;
    if (size_remaining() < sizeof(chunk_size)) {
      break;
    }
    std::memcpy(&chunk_size, &data[i], sizeof(chunk_size));
    i += sizeof(chunk_size);
    chunk_size = std::min(chunk_size, size_remaining());

    string8_view message(reinterpret_cast<const char8*>(&data[i]), chunk_size);
    server.message_parsed(message);
    i += chunk_size;
  }

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
