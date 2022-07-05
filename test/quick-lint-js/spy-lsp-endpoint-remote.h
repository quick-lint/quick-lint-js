// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SPY_LSP_ENDPOINT_REMOTE_H
#define QUICK_LINT_JS_SPY_LSP_ENDPOINT_REMOTE_H

#include <boost/json/value.hpp>
#include <gtest/gtest.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/parse-json.h>
#include <vector>

namespace quick_lint_js {
class spy_lsp_endpoint_remote final : public lsp_endpoint_remote {
 public:
  void send_message(byte_buffer&& message) override {
    // TODO(strager): SCOPED_TRACE(message);
    ::boost::json::value parsed_message = parse_boost_json(message);
    if (auto object = parsed_message.if_object()) {
      EXPECT_EQ((*object)["jsonrpc"], "2.0");
    } else if (auto array = parsed_message.if_array()) {
      // Visual Studio Code's LSP client does not support batch JSON-RPC
      // messages (as of vscode-jsonrpc version 6.0.0):
      // https://github.com/microsoft/vscode-languageserver-node/issues/781
      if (!this->allow_batch_messages) {
        ADD_FAILURE() << "JSON-RPC batch messages are poorly supported by LSP "
                         "clients, but quick-lint-js gave the client a batch "
                         "message. Send multiple messages instead.";
      }

      for (::boost::json::value& sub_message : *array) {
        EXPECT_EQ(look_up(sub_message, "jsonrpc"), "2.0");
      }
    }

    this->messages.push_back(parsed_message);
  }

  std::vector<::boost::json::object> requests() const {
    return this->collect_message_objects(is_request);
  }

  std::vector<::boost::json::object> responses() const {
    return this->collect_message_objects(is_response);
  }

  std::vector<::boost::json::object> notifications() const {
    return this->collect_message_objects(is_notification);
  }

  template <class Predicate>
  std::vector<::boost::json::object> collect_message_objects(
      Predicate&& include) const {
    std::vector<::boost::json::object> result;
    for (const ::boost::json::value& message_value : this->messages) {
      if (const ::boost::json::object* message = message_value.if_object()) {
        if (include(*message)) {
          result.push_back(*message);
        }
      }
    }
    return result;
  }

  static bool is_request(const ::boost::json::object& message) {
    return message.contains("id") && message.contains("method");
  }

  static bool is_response(const ::boost::json::object& message) {
    return message.contains("id") && !message.contains("method");
  }

  static bool is_notification(const ::boost::json::object& message) {
    return !message.contains("id") && message.contains("method");
  }

  std::vector<::boost::json::value> messages;
  bool allow_batch_messages = false;
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
