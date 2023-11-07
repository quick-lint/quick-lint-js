// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/container/vector.h>
#include <vector>

namespace quick_lint_js {
class LSP_Endpoint_Remote {
 public:
  virtual ~LSP_Endpoint_Remote();

  virtual void send_message(Byte_Buffer&& message) = 0;
};

// List of asynchronous JSON-RPC/LSP messages (requests, responses, and
// notifications) to send to the client.
//
// Each message excludes the LSP header; each message should only be the JSON
// message content.
class Outgoing_JSON_RPC_Message_Queue {
 public:
  Byte_Buffer& new_message();

  void send(LSP_Endpoint_Remote&);

 private:
  Vector<Byte_Buffer> messages_{"Outgoing_JSON_RPC_Message_Queue::messages_",
                                new_delete_resource()};
};
}

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
