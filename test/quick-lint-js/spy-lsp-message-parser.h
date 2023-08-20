// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <quick-lint-js/lsp/lsp-message-parser.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/thread.h>
#include <vector>

namespace quick_lint_js {
class Spy_LSP_Message_Parser
    : public LSP_Message_Parser<Spy_LSP_Message_Parser> {
 public:
  void message_parsed(String8_View message) {
    std::lock_guard lock(this->mutex_);
    this->messages_.emplace_back(message);
    this->new_message_.notify_all();
  }

  std::vector<String8> messages() {
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
  mutable Mutex mutex_;
  mutable Condition_Variable new_message_;

  std::vector<String8> messages_;
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
