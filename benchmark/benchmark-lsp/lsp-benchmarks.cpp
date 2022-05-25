// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <boost/json/value.hpp>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/benchmark-config.h>
#include <quick-lint-js/boost-json.h>
#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/lsp-benchmarks.h>
#include <quick-lint-js/lsp-server-process.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/result.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
struct source_file {
  const char* name;
  padded_string source;

  static source_file load(const char* name) {
    auto data = read_file((std::string("corpus/") + name).c_str());
    if (!data.ok()) {
      std::fprintf(stderr, "error: %s\n", data.error_to_string().c_str());
      std::exit(1);
    }
    return source_file{
        .name = name,
        .source = std::move(*data),
    };
  }
};
}

bool benchmark::is_supported(
    const benchmark_config_server& server_config) const {
  if (this->name().ends_with(".jsx") && !server_config.supports_jsx) {
    return false;
  }
  return true;
}

class open_wait_close_benchmark : public benchmark {
 public:
  explicit open_wait_close_benchmark(const source_file* sf)
      : source_file_(sf) {}

  std::string name() const override {
    return std::string("open-wait-close/") + this->source_file_->name;
  }

  bool is_supported(
      const benchmark_config_server& server_config) const override {
    return benchmark::is_supported(server_config);
  }

  lsp_task<void> set_up_async(lsp_server_process& server,
                              const benchmark_config_server&,
                              int iteration_count) override {
    string8_view file_name = u8"test.js";
    server.create_file_on_disk_if_needed(file_name);
    string8 uri = server.file_to_uri(file_name);

    this->iterations_.clear();
    this->iterations_.reserve(narrow_cast<std::size_t>(iteration_count));
    for (int i = 0; i < iteration_count; ++i) {
      // NOTE(strager): Deno expects version numbers to be unique, even
      // after closing and reopening a document.
      std::int64_t version = i + 1;
      this->iterations_.emplace_back(
          version,
          make_text_document_did_open_notification(
              uri, version, this->source_file_->source.string_view()),
          make_text_document_did_close_notification(uri));
    }

    co_return;
  }

  lsp_task<void> run_iteration_async(lsp_server_process& server,
                                     int iteration_index) override {
    iteration_data& iteration =
        this->iterations_[narrow_cast<std::size_t>(iteration_index)];
    server.send_message(std::move(iteration.open_notification));

    ::boost::json::array diagnostics;
    do {
      diagnostics =
          co_await server.wait_for_diagnostics_async(iteration.version);
    } while (diagnostics.empty());

    server.send_message(std::move(iteration.close_notification));
  }

 private:
  struct iteration_data {
    explicit iteration_data(std::int64_t version, byte_buffer open_notification,
                            byte_buffer close_notification)
        : version(version),
          open_notification(std::move(open_notification)),
          close_notification(std::move(close_notification)) {}

    std::int64_t version;
    byte_buffer open_notification;
    byte_buffer close_notification;
  };

  const source_file* source_file_;
  std::vector<iteration_data> iterations_;
};

class change_wait_benchmark : public benchmark {
 public:
  explicit change_wait_benchmark(const source_file* sf) : source_file_(sf) {}

  std::string name() const override {
    return std::string("change-wait/") + this->source_file_->name;
  }

  bool is_supported(
      const benchmark_config_server& server_config) const override {
    return benchmark::is_supported(server_config);
  }

  lsp_task<void> set_up_async(lsp_server_process& server,
                              const benchmark_config_server& server_config,
                              int iteration_count) override {
    string8_view initial_source = u8""sv;
    this->iterations_.clear();
    this->iterations_.reserve(narrow_cast<std::size_t>(iteration_count));
    for (int i = 0; i < iteration_count; ++i) {
      string8 file_name = u8"test" + to_string8(std::to_string(i)) + u8".js";
      server.create_file_on_disk_if_needed(file_name);
      string8 uri = server.file_to_uri(file_name);

      server.send_message(make_text_document_did_open_notification(
          uri, this->initial_version, initial_source));

      if (server_config.wait_for_empty_diagnostics_on_open &&
          !server_config.parallelize_open) {
        co_await server.wait_for_diagnostics_async(uri, this->initial_version);
      }

      this->iterations_.emplace_back(
          uri,
          /*change_text_notification=*/
          make_text_document_did_fully_change_notification(
              uri, this->changed_version,
              this->source_file_->source.string_view()));
    }

    if (server_config.wait_for_empty_diagnostics_on_open &&
        server_config.parallelize_open) {
      std::unordered_map<string8, int> remaining_uris;
      for (iteration_data& iteration : this->iterations_) {
        remaining_uris.emplace(
            iteration.uri, server_config.diagnostics_messages_to_ignore + 1);
      }
      while (!remaining_uris.empty()) {
        ::boost::json::object notification =
            co_await server.wait_for_first_diagnostics_notification_async();
        string8 notification_uri = to_string8(to_string_view(
            look_up(notification, "params", "uri").get_string()));

        auto uri_it = remaining_uris.find(notification_uri);
        QLJS_ALWAYS_ASSERT(uri_it != remaining_uris.end());
        uri_it->second -= 1;
        if (uri_it->second == 0) {
          remaining_uris.erase(uri_it);
        }
      }
    }
  }

  lsp_task<void> run_iteration_async(lsp_server_process& server,
                                     int iteration_index) override {
    iteration_data& iteration =
        this->iterations_[narrow_cast<std::size_t>(iteration_index)];

    server.send_message(std::move(iteration.change_text_notification));

    ::boost::json::array diagnostics;
    do {
      diagnostics = co_await server.wait_for_diagnostics_async(
          iteration.uri, this->changed_version);
    } while (diagnostics.empty());
  }

 private:
  struct iteration_data {
    explicit iteration_data(string8 uri, byte_buffer&& change_text_notification)
        : uri(std::move(uri)),
          change_text_notification(std::move(change_text_notification)) {}

    string8 uri;
    byte_buffer change_text_notification;
  };

  static inline constexpr std::int64_t initial_version = 0;
  static inline constexpr std::int64_t changed_version = 1;

  const source_file* source_file_;
  std::vector<iteration_data> iterations_;
};

class incremental_change_wait_benchmark : public benchmark {
 public:
  explicit incremental_change_wait_benchmark(
      const source_file* sf,
      void (*changes_factory)(int i, byte_buffer& out_changes))
      : source_file_(sf), changes_factory_(changes_factory) {}

  std::string name() const override {
    return std::string("incremental-change-wait/") + this->source_file_->name;
  }

  bool is_supported(
      const benchmark_config_server& server_config) const override {
    return benchmark::is_supported(server_config) &&
           server_config.allow_incremental_changes;
  }

  lsp_task<void> set_up_async(lsp_server_process& server,
                              const benchmark_config_server& server_config,
                              int iteration_count) override {
    string8 file_name = u8"test.js";
    server.create_file_on_disk_if_needed(file_name);
    string8 uri = server.file_to_uri(file_name);
    std::int64_t version = 0;

    server.send_message(make_text_document_did_open_notification(
        uri, version, this->source_file_->source.string_view()));
    ::boost::json::array diagnostics;
    do {
      diagnostics = co_await server.wait_for_diagnostics_async(uri, version);
    } while (diagnostics.empty());
    this->expected_diagnostics = diagnostics;
    version += 1;

    this->iterations_.clear();
    this->iterations_.reserve(narrow_cast<std::size_t>(iteration_count));
    for (int i = 0; i < iteration_count; ++i) {
      byte_buffer change_text_notification;
      change_text_notification.append_copy(
          u8R"({"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"version":)"sv);
      change_text_notification.append_decimal_integer(version);
      change_text_notification.append_copy(u8R"(,"uri":")"sv);
      write_json_escaped_string(change_text_notification, uri);
      change_text_notification.append_copy(u8R"("},"contentChanges":)"sv);
      this->changes_factory_(i, change_text_notification);
      change_text_notification.append_copy(u8R"(}})"sv);
      this->iterations_.emplace_back(version,
                                     std::move(change_text_notification));
      version += 1;
    }
  }

  lsp_task<void> run_iteration_async(lsp_server_process& server,
                                     int iteration_index) override {
    iteration_data& iteration =
        this->iterations_[narrow_cast<std::size_t>(iteration_index)];

    server.send_message(std::move(iteration.change_text_notification));

    ::boost::json::array diagnostics =
        co_await server.wait_for_diagnostics_after_incremental_change_async(
            iteration.version);
    if (diagnostics.size() != this->expected_diagnostics.size()) {
      std::fprintf(stderr, "error: expected %zu diagnostics but got %zu\n",
                   this->expected_diagnostics.size(), diagnostics.size());
      std::exit(1);
    }
  }

 private:
  struct iteration_data {
    explicit iteration_data(std::int64_t version,
                            byte_buffer&& change_text_notification)
        : version(version),
          change_text_notification(std::move(change_text_notification)) {}

    std::int64_t version;
    byte_buffer change_text_notification;
  };

  const source_file* source_file_;
  void (*changes_factory_)(int i, byte_buffer& out_changes);
  std::vector<iteration_data> iterations_;
  ::boost::json::array expected_diagnostics;
};

class full_change_wait_benchmark : public benchmark {
 public:
  explicit full_change_wait_benchmark(const char* name,
                                      padded_string (*source_factory)(int i))
      : name_(name), source_factory_(source_factory) {}

  std::string name() const override {
    return std::string("full-change-wait/") + this->name_;
  }

  bool is_supported(
      const benchmark_config_server& server_config) const override {
    return benchmark::is_supported(server_config);
  }

  lsp_task<void> set_up_async(lsp_server_process& server,
                              const benchmark_config_server& server_config,
                              int iteration_count) override {
    string8 file_name = u8"test.js";
    server.create_file_on_disk_if_needed(file_name);
    string8 uri = server.file_to_uri(file_name);
    std::int64_t version = 0;

    padded_string initial_source = this->source_factory_(0);
    server.send_message(make_text_document_did_open_notification(
        uri, version, initial_source.string_view()));
    ::boost::json::array diagnostics;
    do {
      diagnostics = co_await server.wait_for_diagnostics_async(uri, version);
    } while (diagnostics.empty());
    this->expected_diagnostics = diagnostics;
    version += 1;

    this->iterations_.clear();
    this->iterations_.reserve(narrow_cast<std::size_t>(iteration_count));
    for (int i = 0; i < iteration_count; ++i) {
      byte_buffer change_text_notification;
      change_text_notification.append_copy(
          u8R"({"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"version":)"sv);
      change_text_notification.append_decimal_integer(version);
      change_text_notification.append_copy(u8R"(,"uri":")"sv);
      write_json_escaped_string(change_text_notification, uri);
      change_text_notification.append_copy(
          u8R"("},"contentChanges":[{"text":")"sv);
      padded_string new_source = this->source_factory_(i + 1);
      write_json_escaped_string(change_text_notification,
                                new_source.string_view());
      change_text_notification.append_copy(u8R"("}]}})"sv);
      this->iterations_.emplace_back(version,
                                     std::move(change_text_notification));
      version += 1;
    }
  }

  lsp_task<void> run_iteration_async(lsp_server_process& server,
                                     int iteration_index) override {
    iteration_data& iteration =
        this->iterations_[narrow_cast<std::size_t>(iteration_index)];

    server.send_message(std::move(iteration.change_text_notification));

  retry:
    ::boost::json::array diagnostics =
        co_await server.wait_for_diagnostics_async(iteration.version);
    if (diagnostics.size() != this->expected_diagnostics.size()) {
      if (diagnostics.empty()) {
        // HACK(strager): Some LSP servers, such as Flow and TypeScript, give us
        // an empty list of diagnostics before giving us the real list of
        // diagnostics. Skip the empty list and wait for the real list.
        goto retry;
      }
      std::fprintf(stderr, "error: expected %zu diagnostics but got %zu\n",
                   this->expected_diagnostics.size(), diagnostics.size());
      std::exit(1);
    }
  }

 private:
  struct iteration_data {
    explicit iteration_data(std::int64_t version,
                            byte_buffer&& change_text_notification)
        : version(version),
          change_text_notification(std::move(change_text_notification)) {}

    std::int64_t version;
    byte_buffer change_text_notification;
  };

  std::string name_;
  padded_string (*source_factory_)(int i);
  std::vector<iteration_data> iterations_;
  ::boost::json::array expected_diagnostics;
};

std::vector<benchmark_factory> get_benchmark_factories() {
  static source_file tiny_js = source_file::load("tiny.js");
  static source_file edex_ui_filesystem_js =
      source_file::load("edex-ui-filesystem.class.js");
  static source_file express_router_js = source_file::load("express-router.js");
  static source_file react_quickly_ch10_jsx =
      source_file::load("react-quickly-ch10.jsx");

  return std::vector<benchmark_factory>{
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<open_wait_close_benchmark>(&tiny_js);
      },
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<open_wait_close_benchmark>(
            &edex_ui_filesystem_js);
      },
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<open_wait_close_benchmark>(&express_router_js);
      },
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<open_wait_close_benchmark>(
            &react_quickly_ch10_jsx);
      },
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<change_wait_benchmark>(&tiny_js);
      },
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<change_wait_benchmark>(&edex_ui_filesystem_js);
      },
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<change_wait_benchmark>(&express_router_js);
      },
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<change_wait_benchmark>(&react_quickly_ch10_jsx);
      },
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<
            incremental_change_wait_benchmark>(&express_router_js, [](int i,
                                                                      byte_buffer&
                                                                          out_changes) {
          // In the "create Router#VERB functions" arrow function in
          // express-router.js, replace 'method' (declaration and
          // references) with 'm00001', then 'm00002', etc.
          char replacement_text[10];
          QLJS_ASSERT(i >= 0);
          QLJS_ASSERT(i <= 99999);
          std::snprintf(replacement_text, sizeof(replacement_text), "m%05d", i);
          QLJS_ASSERT(std::strlen(replacement_text) == 6);
          out_changes.append_copy(u8R"([{"text":")"sv);
          out_changes.append_copy(to_string8_view(replacement_text));
          out_changes.append_copy(
              u8R"(","range":{"start":{"line":506,"character":39},"end":{"line":506,"character":45}}},)"sv);
          out_changes.append_copy(u8R"({"text":")"sv);
          out_changes.append_copy(to_string8_view(replacement_text));
          out_changes.append_copy(
              u8R"(","range":{"start":{"line":507,"character":8},"end":{"line":507,"character":14}}},)"sv);
          out_changes.append_copy(u8R"({"text":")"sv);
          out_changes.append_copy(to_string8_view(replacement_text));
          out_changes.append_copy(
              u8R"(","range":{"start":{"line":509,"character":10},"end":{"line":509,"character":16}}}])"sv);
        });
      },
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<incremental_change_wait_benchmark>(
            &react_quickly_ch10_jsx, [](int i, byte_buffer& out_changes) {
              // In Cart's render function in react-quickly-ch10.jsx, clear the
              // "Your cart is empty" text then re-type it character by
              // character.
              static constexpr char text[] = "Your cart is empty";
              static constexpr int text_length = sizeof(text) - 1;
              int characters_already_typed = (i % text_length == 0)
                                                 ? text_length
                                                 : ((i % text_length) - 1);
              if (characters_already_typed == text_length) {
                // The text has been fully typed. Erase it.
                out_changes.append_copy(
                    u8R"([{"text":"","range":{"start":{"line":53,"character":49},"end":{"line":53,"character":67}}}])"sv);
              } else {
                // Type the next character.
                int column = 49 + characters_already_typed;
                out_changes.append_copy(u8R"([{"text":")"sv);
                out_changes.append_copy(text[characters_already_typed]);
                out_changes.append_copy(
                    u8R"(","range":{"start":{"line":53,"character":)"sv);
                out_changes.append_decimal_integer(column);
                out_changes.append_copy(
                    u8R"(},"end":{"line":53,"character":)"sv);
                out_changes.append_decimal_integer(column);
                out_changes.append_copy(u8R"(}}}])"sv);
              }
            });
      },
      []() -> std::unique_ptr<benchmark> {
        return std::make_unique<full_change_wait_benchmark>(
            express_router_js.name, [](int i) {
              // In the "create Router#VERB functions" arrow function in
              // express-router.js, replace 'method' (declaration and
              // references) with 'm00001', then 'm00002', etc.
              char replacement_text[10];
              QLJS_ASSERT(i >= 0);
              QLJS_ASSERT(i <= 99999);
              std::snprintf(replacement_text, sizeof(replacement_text), "m%05d",
                            i);
              QLJS_ASSERT(std::strlen(replacement_text) == 6);
              string8_view replacement_text_sv =
                  to_string8_view(replacement_text);

              padded_string new_source(express_router_js.source.string_view());
              auto write_replacement = [&](int offset) -> void {
                std::copy(replacement_text_sv.begin(),
                          replacement_text_sv.end(),
                          new_source.data() + offset);
              };
              write_replacement(11854);
              write_replacement(11871);
              write_replacement(11940);
              return new_source;
            });
      },
  };
}
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
