// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_WORKSPACE_CONFIGURATION_H
#define QUICK_LINT_JS_LSP_WORKSPACE_CONFIGURATION_H

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <quick-lint-js/char8.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <simdjson.h>
#include <string>
#include <vector>

namespace quick_lint_js {
class byte_buffer;

// lsp_workspace_configuration manages the LSP protocol bits for configuration
// (e.g. workspace/configuration).
class lsp_workspace_configuration {
 public:
  // Register a configuration setting.
  //
  // out_value is stored; *out_value will be modified by process_response.
  //
  // name must be have global lifetime (e.g. be a compile-time string).
  // name must be a JSON-encoded string (without surrounding quotation marks).
  void add_item(string8_view name, std::string* out_value);

  // Create a workspace/configuration JSON-RPC request to send to the LSP
  // client.
  void build_request(lsp_endpoint_handler::request_id_type request_id,
                     byte_buffer& request_json);

  // Handle a workspace/configuration JSON-RPC response sent by the LSP client.
  bool process_response(::simdjson::ondemand::value result);

 private:
  struct item {
    string8_view name;
    std::string* value;
  };
  std::vector<item> items_;
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
