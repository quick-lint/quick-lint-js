// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/feature.h>

#if QLJS_FEATURE_DEBUG_SERVER

#include <cstddef>
#include <mongoose.h>
#include <mutex>
#include <quick-lint-js/debug/mongoose.h>
#include <string>

namespace quick_lint_js {
namespace {
thread_local std::string* mongoose_log_capture_target = nullptr;

void mongoose_log(char c, void*) {
  std::fputc(c, stderr);

  std::string* buffer = mongoose_log_capture_target;
  if (buffer) {
    buffer->push_back(c);
  }
}

void mongoose_init() {
  ::mg_log_set(2);  // errors and info
  ::mg_log_set_fn(mongoose_log, nullptr);
}
}

void mongoose_init_if_needed() {
  static std::once_flag init_flag;
  std::call_once(init_flag, mongoose_init);
}

void mongoose_begin_capturing_logs_on_current_thread(std::string* out) {
  mongoose_log_capture_target = out;
}

void mongoose_stop_capturing_logs_on_current_thread() {
  mongoose_log_capture_target = nullptr;
}
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
