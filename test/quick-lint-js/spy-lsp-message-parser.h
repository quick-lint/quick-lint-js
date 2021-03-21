// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SPY_LSP_MESSAGE_PARSER_H
#define QUICK_LINT_JS_SPY_LSP_MESSAGE_PARSER_H

#include <condition_variable>
#include <mutex>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lsp-message-parser.h>
#include <vector>

namespace quick_lint_js {
class spy_lsp_message_parser
    : public lsp_message_parser<spy_lsp_message_parser> {
 public:
  void message_parsed(string8_view message) {
    std::lock_guard lock(this->mutex_);
    this->messages_.emplace_back(message);
    this->new_message_.notify_all();
  }

  std::vector<string8> messages() {
    std::lock_guard lock(this->mutex_);
    return this->messages_;
  }

  template <class Func>
  void wait_until_messages(Func&& predicate) const {
    std::unique_lock lock(this->mutex_);
    this->new_message_.wait(lock, [this, &predicate]() -> bool {
      return predicate(this->messages_);
    });
  }

 private:
  mutable std::mutex mutex_;
  mutable std::condition_variable new_message_;

  std::vector<string8> messages_;
};
}

#endif

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
