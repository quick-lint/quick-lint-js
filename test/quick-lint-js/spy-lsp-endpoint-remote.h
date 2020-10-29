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

#ifndef QUICK_LINT_JS_SPY_LSP_ENDPOINT_REMOTE_H
#define QUICK_LINT_JS_SPY_LSP_ENDPOINT_REMOTE_H

#include <gtest/gtest.h>
#include <json/value.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/json.h>
#include <vector>

namespace quick_lint_js {
class spy_lsp_endpoint_remote {
 public:
  void send_message(const byte_buffer& message) {
    string8 message_json;
    message_json.resize(message.size());
    message.copy_to(message_json.data());

    ::Json::Value parsed_message;
    ::Json::String errors;
    bool ok = parse_json(message_json, &parsed_message, &errors);
    EXPECT_TRUE(ok) << errors;

    if (parsed_message.isObject()) {
      EXPECT_EQ(parsed_message["jsonrpc"], "2.0");
    } else if (parsed_message.isArray()) {
      for (::Json::Value& sub_message : parsed_message) {
        EXPECT_EQ(sub_message["jsonrpc"], "2.0");
      }
    }

    this->messages.push_back(parsed_message);
  }

  std::vector<::Json::Value> messages;
};
}

#endif
