// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/feature.h>

#if QLJS_FEATURE_DEBUG_SERVER

#include <atomic>
#include <cstdio>
#include <cstring>
#include <mongoose.h>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/debug/debug-server-fs.h>
#include <quick-lint-js/debug/debug-server.h>
#include <quick-lint-js/debug/mongoose.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
debug_server::~debug_server() {
  if (this->server_thread_.joinable()) {
    this->stop_server_thread();
    this->server_thread_.join();
  }
}

void debug_server::set_listen_address(std::string_view address) {
  QLJS_ASSERT(!this->server_thread_.joinable());

  this->requested_listen_address_ = address;
}

void debug_server::start_server_thread() {
  QLJS_ASSERT(!this->server_thread_.joinable());

  this->init_data_.reset();
  this->init_error_.clear();

  this->server_thread_ = thread([this] { this->run_on_current_thread(); });
}

void debug_server::stop_server_thread() {
  std::unique_lock<mutex> lock(this->mutex_);
  this->stop_server_thread_ = true;
  this->wake_up_server_thread(lock);

  this->did_wait_for_server_start_ = false;
}

result<void, debug_server_io_error> debug_server::wait_for_server_start() {
  std::unique_lock<mutex> lock(this->mutex_);
  this->initialized_.wait(lock, [&] {
    return this->init_data_.has_value() || !this->init_error_.empty();
  });

  if (!this->init_error_.empty()) {
    return failed_result(debug_server_io_error{
        .error_message = this->init_error_,
    });
  }

  this->did_wait_for_server_start_ = true;
  return {};
}

std::string debug_server::url() const { return this->url("/"sv); }

std::string debug_server::url(std::string_view path) const {
  QLJS_ASSERT(this->did_wait_for_server_start_);

  std::string result;
  result.reserve(path.size() + 100);
  result += "http://"sv;
  {
    std::lock_guard<mutex> lock(this->mutex_);
    result += this->init_data_->actual_listen_address;
  }
  result += path;
  return result;
}

void debug_server::wake_up_server_thread(std::unique_lock<mutex> &) {
  if (this->init_data_.has_value()) {
    char wakeup_signal[] = {0};
    ::ssize_t rc = ::send(this->init_data_->wakeup_pipe, wakeup_signal,
                          sizeof(wakeup_signal), /*flags=*/0);
    QLJS_ALWAYS_ASSERT(rc == 1);
  }
}

void debug_server::run_on_current_thread() {
  mongoose_mgr mgr;

  std::string connect_logs;
  mongoose_begin_capturing_logs_on_current_thread(&connect_logs);
  ::mg_connection *server_connection = ::mg_http_listen(
      mgr.get(), this->requested_listen_address_.c_str(),
      mongoose_callback<&debug_server::http_server_callback>(), this);
  mongoose_stop_capturing_logs_on_current_thread();
  if (!server_connection) {
    std::lock_guard<mutex> lock(this->mutex_);
    if (connect_logs.empty()) {
      this->init_error_ = "unknown error in mg_http_listen";
    } else {
      this->init_error_ = std::move(connect_logs);
    }
    this->initialized_.notify_all();
    return;
  }

  {
    std::lock_guard<mutex> lock(this->mutex_);
    QLJS_ASSERT(!this->init_data_.has_value());
    this->init_data_.emplace();

    // server_connection->loc is initialized synchronously, so we should be able
    // to use c->loc now.
    std::string &address = this->init_data_->actual_listen_address;
    address.resize(100);
    ::mg_straddr(&server_connection->loc, address.data(), address.size());
    address.resize(std::strlen(address.c_str()));

    this->init_data_->wakeup_pipe = ::mg_mkpipe(
        mgr.get(), mongoose_callback<&debug_server::wakeup_pipe_callback>(),
        this,
        /*udp=*/false);
    QLJS_ALWAYS_ASSERT(this->init_data_->wakeup_pipe != -1);

    this->initialized_.notify_all();
  }

  while (!this->stop_server_thread_.load()) {
    ::mg_mgr_poll(mgr.get(), /*timeout_ms=*/-1);
  }
}

void debug_server::begin_closing_all_connections(::mg_mgr *mgr) {
  ::mg_connection *c = mgr->conns;
  while (c) {
    c->is_closing = true;
    c = c->next;
  }
}

void debug_server::http_server_callback(::mg_connection *c, int ev,
                                        void *ev_data) noexcept {
  switch (ev) {
  case ::MG_EV_HTTP_MSG: {
    ::mg_http_message *hm = static_cast<::mg_http_message *>(ev_data);
    if (::mg_http_match_uri(hm, "/")) {
      ::mg_http_reply(c, 200, "Content-Type: text/html\r\n", "%.*s",
                      narrow_cast<int>(debug_server_index_html.size()),
                      debug_server_index_html.data());
    } else {
      ::mg_http_reply(c, 404, "Content-Type: text/plain\r\n",
                      "404 not found\n");
    }
    break;
  }

  default:
    break;
  }
}

void debug_server::wakeup_pipe_callback(::mg_connection *c, int ev,
                                        void *) noexcept {
  switch (ev) {
  case ::MG_EV_READ:
    // wake_up_server_thread was called.
    if (this->stop_server_thread_.load()) {
      this->begin_closing_all_connections(c->mgr);
    }
    break;

  default:
    break;
  }
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
