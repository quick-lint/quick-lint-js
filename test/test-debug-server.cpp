// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/feature.h>

#if QLJS_FEATURE_DEBUG_SERVER

#include <cctype>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <mongoose.h>
#include <optional>
#include <quick-lint-js/debug/debug-server.h>
#include <quick-lint-js/debug/mongoose.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
struct http_response {
  std::optional<int> status;
  std::string data;
  std::vector<std::pair<std::string, std::string>> headers;

  std::string_view get_last_header_value_or_empty(
      std::string_view header_name) const;

  static http_response failed([[maybe_unused]] std::string_view error_message) {
    return http_response();
  }

  // Returns false if there was a failure when making the request.
  /*implicit*/ operator bool() const noexcept {
    return this->status.has_value();
  }
};

http_response http_fetch(const char *url);
http_response http_fetch(const std::string &url);

class test_debug_server : public ::testing::Test {};

TEST_F(test_debug_server, start_thread_then_immediately_stop) {
  debug_server server;
  server.start_server_thread();
  server.stop_server_thread();
}

TEST_F(test_debug_server, serves_html_at_index) {
  debug_server server;
  server.start_server_thread();
  auto wait_result = server.wait_for_server_start();
  ASSERT_TRUE(wait_result.ok()) << wait_result.error_to_string();

  http_response response = http_fetch(server.url("/"));
  ASSERT_TRUE(response);
  EXPECT_EQ(response.status, 200);  // OK
  EXPECT_THAT(response.data, ::testing::StartsWith("<!DOCTYPE html>"));
  EXPECT_EQ(response.get_last_header_value_or_empty("content-type"),
            "text/html");
}

TEST_F(test_debug_server, serves_not_found_page) {
  debug_server server;
  server.start_server_thread();
  auto wait_result = server.wait_for_server_start();
  ASSERT_TRUE(wait_result.ok()) << wait_result.error_to_string();

  http_response response = http_fetch(server.url("/route-does-not-exist"));
  ASSERT_TRUE(response);
  EXPECT_EQ(response.status, 404);  // Not Found
}

TEST_F(test_debug_server, two_servers_listening_on_same_port_fails) {
  debug_server server_a;
  server_a.start_server_thread();
  auto a_wait_result = server_a.wait_for_server_start();
  ASSERT_TRUE(a_wait_result.ok()) << a_wait_result.error_to_string();

  std::string server_a_address = server_a.url("");
  SCOPED_TRACE("server_a address: " + server_a_address);

  debug_server server_b;
  server_b.set_listen_address(server_a_address);
  server_b.start_server_thread();
  auto b_wait_result = server_b.wait_for_server_start();
  EXPECT_FALSE(b_wait_result.ok());
}

bool strings_equal_case_insensitive(std::string_view a, std::string_view b) {
  return ranges_equal(
      a, b, [](char x, char y) { return std::tolower(x) == std::tolower(y); });
}

std::string_view http_response::get_last_header_value_or_empty(
    std::string_view header_name) const {
  for (const auto &[name, value] : this->headers) {
    if (strings_equal_case_insensitive(name, header_name)) {
      return value;
    }
  }
  return ""sv;
}

http_response http_fetch(const char *url) {
  mongoose_mgr mgr;

  struct state {
    const char *request_url;
    http_response response;
    bool done = false;
    std::string protocol_error;

    void callback(::mg_connection *c, int ev, void *ev_data) {
      switch (ev) {
      case ::MG_EV_CONNECT: {
        ::mg_str host = ::mg_url_host(this->request_url);
        ::mg_printf(c,
                    "GET %s HTTP/1.1\r\n"
                    "Host: %.*s\r\n"
                    "\r\n",
                    ::mg_url_uri(this->request_url), narrow_cast<int>(host.len),
                    host.ptr);
        break;
      }

      case ::MG_EV_HTTP_MSG: {
        ::mg_http_message *hm = static_cast<::mg_http_message *>(ev_data);
        this->response.data.assign(hm->body.ptr, hm->body.len);
        this->response.status = ::mg_http_status(hm);

        for (int i = 0; i < MG_MAX_HTTP_HEADERS; ++i) {
          const ::mg_http_header &h = hm->headers[i];
          if (h.name.len == 0) {
            break;
          }
          this->response.headers.emplace_back(
              std::string(h.name.ptr, h.name.len),
              std::string(h.value.ptr, h.value.len));
        }

        this->done = true;
        c->is_closing = true;
        break;
      }

      case ::MG_EV_ERROR:
        this->protocol_error = static_cast<const char *>(ev_data);
        this->done = true;
        c->is_closing = true;
        break;

      default:
        break;
      }
    }
  };

  state s;
  s.request_url = url;
  ::mg_connection *connection = ::mg_http_connect(
      mgr.get(), s.request_url, mongoose_callback<&state::callback>(), &s);
  if (!connection) {
    ADD_FAILURE() << "mg_http_connect failed";
    return http_response::failed("mg_http_connect failed");
  }

  while (!s.done) {
    int timeout_ms = 1000;
    ::mg_mgr_poll(mgr.get(), timeout_ms);
  }

  if (!s.protocol_error.empty()) {
    ADD_FAILURE() << "protocol error: " << s.protocol_error;
    return http_response::failed(s.protocol_error);
  }

  return std::move(s.response);
}

http_response http_fetch(const std::string &url) {
  return http_fetch(url.c_str());
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
