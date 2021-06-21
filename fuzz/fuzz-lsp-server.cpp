// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/lsp-server.h>

namespace quick_lint_js {
namespace {
class null_lsp_endpoint_remote {
 public:
  void send_message(const byte_buffer&) {}
};
}
}

extern "C" {
int LLVMFuzzerTestOneInput(const std::uint8_t* data, std::size_t size) {
  using namespace quick_lint_js;

  lsp_endpoint<linting_lsp_server_handler<lsp_javascript_linter>,
               null_lsp_endpoint_remote>
      server;

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

    server.append(
        string8_view(reinterpret_cast<const char8*>(&data[i]), chunk_size));
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
