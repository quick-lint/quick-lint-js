// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <quick-lint-js/lsp/lsp-json-rpc-message-parser.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/function-ref.h>
#include <quick-lint-js/simdjson-fwd.h>
#include <string>
#include <vector>

namespace quick_lint_js {
class Byte_Buffer;

// lsp_workspace_configuration manages the LSP protocol bits for configuration
// (e.g. workspace/configuration).
class LSP_Workspace_Configuration {
 public:
  // Register a configuration setting.
  //
  // callback is later called by process_response or process_notification.
  //
  // name must be have global lifetime (e.g. be a compile-time string).
  // name must be a JSON-encoded string (without surrounding quotation marks).
  void add_item(String8_View name,
                Async_Function_Ref<void(std::string_view)> callback);

  // Create a workspace/configuration JSON-RPC request to send to the LSP
  // client.
  void build_request(JSON_RPC_Message_Handler::Request_ID_Type request_id,
                     Byte_Buffer& request_json);

  // Handle a workspace/configuration JSON-RPC response sent by the LSP client.
  bool process_response(::simdjson::ondemand::value result);

  // Handle a workspace/didChangeConfiguration JSON-RPC notification sent by the
  // LSP client.
  bool process_notification(::simdjson::ondemand::object settings);

  // Handle params.initializationOptions.configuration from an initialize
  // JSON-RPC request sent by the LSP client.
  //
  // This is custom to quick-lint-js and is not part of LSP itself.
  bool process_initialization_options(
      ::simdjson::ondemand::object initialization_options_configuration);

 private:
  struct Item {
    String8_View name;
    Async_Function_Ref<void(std::string_view)> callback;
  };

  Item* find_item(String8_View name);
  bool set_item(Item&, ::simdjson::ondemand::value);

  std::vector<Item> items_;
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
