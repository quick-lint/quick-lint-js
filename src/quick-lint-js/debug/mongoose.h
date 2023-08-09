// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DEBUG_MONGOOSE_H
#define QUICK_LINT_JS_DEBUG_MONGOOSE_H

#include <quick-lint-js/feature.h>

#if QLJS_FEATURE_DEBUG_SERVER

#include <mongoose.h>
#include <quick-lint-js/util/type-traits.h>
#include <string>

namespace quick_lint_js {
// Configures logging and other stuff.
//
// Thread-safe.
void mongoose_init_if_needed();

void mongoose_begin_capturing_logs_on_current_thread(std::string *out);
void mongoose_stop_capturing_logs_on_current_thread();

// RAII wrapper around mg_mgr.
class Mongoose_Mgr {
 public:
  explicit Mongoose_Mgr() {
    mongoose_init_if_needed();
    ::mg_mgr_init(&this->mgr_);
  }

  Mongoose_Mgr(const Mongoose_Mgr &) = delete;
  Mongoose_Mgr &operator=(const Mongoose_Mgr &) = delete;

  ~Mongoose_Mgr() { ::mg_mgr_free(&this->mgr_); }

  ::mg_mgr *get() { return &this->mgr_; }

 private:
  ::mg_mgr mgr_;
};

template <auto member_function_pointer>
mg_event_handler_t mongoose_callback() {
  using Self = typename Member_Function_Pointer_Traits<
      decltype(member_function_pointer)>::Class_Type;
  return [](::mg_connection *c, int ev, void *ev_data, void *fn_data) -> void {
    (static_cast<Self *>(fn_data)->*member_function_pointer)(c, ev, ev_data);
  };
}
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
