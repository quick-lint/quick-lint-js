// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_OUTGOING_JSON_RPC_MESSAGE_QUEUE_H
#define QUICK_LINT_JS_LSP_OUTGOING_JSON_RPC_MESSAGE_QUEUE_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <quick-lint-js/container/byte-buffer.h>
#include <vector>

namespace quick_lint_js {
class lsp_endpoint_remote {
 public:
  virtual ~lsp_endpoint_remote();

  virtual void send_message(byte_buffer&& message) = 0;
};

// List of asynchronous JSON-RPC/LSP messages (requests, responses, and
// notifications) to send to the client.
//
// Each message excludes the LSP header; each message should only be the JSON
// message content.
class outgoing_json_rpc_message_queue {
 public:
  byte_buffer& new_message();

  void send(lsp_endpoint_remote&);

 private:
  std::vector<byte_buffer> messages_;
};
}

#endif

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
