// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <gtest/gtest.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/lsp/lsp-json-rpc-message-parser.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/tjson.h>
#include <vector>

namespace quick_lint_js {
class Spy_LSP_Endpoint_Remote final : public LSP_Endpoint_Remote {
 public:
  void send_message(Byte_Buffer&& message) override {
    TJSON parsed_message(message);
    if (parsed_message.root().is_object()) {
      EXPECT_EQ(parsed_message[u8"jsonrpc"_sv], u8"2.0"_sv);
    } else if (parsed_message.root().is_array()) {
      ADD_FAILURE() << "JSON-RPC batch messages are poorly supported by LSP "
                       "clients, but quick-lint-js gave the client a batch "
                       "message. Send multiple messages instead.";
      for (TJSON_Value sub_message :
           parsed_message.root().get_array_or_empty()) {
        EXPECT_EQ(sub_message[u8"jsonrpc"_sv], u8"2.0"_sv);
      }
    }

    this->messages.push_back(std::move(parsed_message));
  }

  std::vector<TJSON_Value> requests() const {
    return this->collect_message_objects(is_request);
  }

  std::vector<TJSON_Value> responses() const {
    return this->collect_message_objects(is_response);
  }

  std::vector<TJSON_Value> notifications() const {
    return this->collect_message_objects(is_notification);
  }

  template <class Predicate>
  std::vector<TJSON_Value> collect_message_objects(Predicate&& include) const {
    std::vector<TJSON_Value> result;
    for (const TJSON& message : this->messages) {
      if (include(message)) {
        result.push_back(message.root());
      }
    }
    return result;
  }

  static bool is_request(const TJSON& message) {
    return message[u8"id"_sv].exists() && message[u8"method"_sv].exists();
  }

  static bool is_response(const TJSON& message) {
    return message[u8"id"_sv].exists() && !message[u8"method"_sv].exists();
  }

  static bool is_notification(const TJSON& message) {
    return !message[u8"id"_sv].exists() && message[u8"method"_sv].exists();
  }

  std::vector<TJSON> messages;
  bool allow_batch_messages = false;
};
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
