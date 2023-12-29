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
class Null_LSP_Endpoint_Remote {
 public:
  void send_message(const Byte_Buffer&) {}
};

class Null_Configuration_Filesystem : public Configuration_Filesystem {
 public:
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> canonicalize_path(
      const std::string& path) override {
    return Canonical_Path_Result(std::string(path), /*existing_path_length=*/0);
  }

  Result<Padded_String, Read_File_IO_Error> read_file(
      const Canonical_Path& path) override {
#if QLJS_HAVE_WINDOWS_H
    Windows_File_IO_Error io_error = {ERROR_FILE_NOT_FOUND};
#endif
#if QLJS_HAVE_UNISTD_H
    POSIX_File_IO_Error io_error = {ENOENT};
#endif
    return failed_result(Read_File_IO_Error{
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

  Null_Configuration_Filesystem fs;
  LSP_JavaScript_Linter linter;
  Linting_LSP_Server_Handler handler(&fs, &linter);
  LSP_JSON_RPC_Message_Parser server(&handler);

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

    String8_View message(reinterpret_cast<const Char8*>(&data[i]), chunk_size);
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
