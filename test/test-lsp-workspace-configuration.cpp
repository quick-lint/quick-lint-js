// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <boost/json/value.hpp>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/boost-json.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/lsp/lsp-workspace-configuration.h>
#include <quick-lint-js/parse-json.h>
#include <simdjson.h>
#include <string>
#include <string_view>
#include <utility>

using ::testing::IsEmpty;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
struct easy_simdjson_parser {
  explicit easy_simdjson_parser(::simdjson::padded_string json)
      : error(::simdjson::TAPE_ERROR),  // Arbitrary non-successful error code.
        json(std::move(json)) {
    this->error = parser.iterate(this->json).get(this->document);
    if (this->error != ::simdjson::SUCCESS) {
      return;
    }
    this->error = this->document.get_value().get(this->value);
  }

  ::simdjson::ondemand::object value_object() {
    ::simdjson::ondemand::object o;
    this->error = this->value.get_object().get(o);
    return o;
  }

  ::simdjson::error_code error;
  ::simdjson::padded_string json;
  ::simdjson::ondemand::parser parser;
  ::simdjson::ondemand::document document;
  ::simdjson::ondemand::value value;
};

TEST(test_lsp_workspace_configuration, empty_config_request) {
  lsp_workspace_configuration config;

  byte_buffer request_json;
  config.build_request(77, request_json);

  ::boost::json::value request = parse_boost_json(request_json);
  EXPECT_EQ(look_up(request, "jsonrpc"), "2.0");
  EXPECT_EQ(look_up(request, "id"), 77);
  EXPECT_EQ(look_up(request, "method"), "workspace/configuration");
  ::boost::json::array items = look_up(request, "params", "items").as_array();
  EXPECT_THAT(items, IsEmpty());
}

TEST(test_lsp_workspace_configuration, config_request_with_three_items) {
  lsp_workspace_configuration config;
  config.add_item(u8"first"_sv, [](std::string_view) {});
  config.add_item(u8"second"_sv, [](std::string_view) {});
  config.add_item(u8"third"_sv, [](std::string_view) {});

  byte_buffer request_json;
  config.build_request(77, request_json);

  ::boost::json::value request = parse_boost_json(request_json);
  ::boost::json::array request_items =
      look_up(request, "params", "items").as_array();
  ASSERT_EQ(request_items.size(), 3);
  EXPECT_EQ(look_up(request_items[0], "section"), "first");
  EXPECT_EQ(look_up(request_items[1], "section"), "second");
  EXPECT_EQ(look_up(request_items[2], "section"), "third");
}

TEST(test_lsp_workspace_configuration, empty_config_response) {
  lsp_workspace_configuration config;

  easy_simdjson_parser result("[]"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  ASSERT_TRUE(ok);
}

TEST(test_lsp_workspace_configuration, config_response_with_strings) {
  std::string items[3];
  lsp_workspace_configuration config;
  config.add_item(u8"first"_sv, [&items](std::string_view new_value) {
    items[0] = new_value;
  });
  config.add_item(u8"second"_sv, [&items](std::string_view new_value) {
    items[1] = new_value;
  });
  config.add_item(u8"third"_sv, [&items](std::string_view new_value) {
    items[2] = new_value;
  });

  easy_simdjson_parser result(
      R"(["firstval", "secondval", "thirdval"])"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  ASSERT_TRUE(ok);

  EXPECT_EQ(items[0], "firstval");
  EXPECT_EQ(items[1], "secondval");
  EXPECT_EQ(items[2], "thirdval");
}

TEST(test_lsp_workspace_configuration,
     empty_config_response_with_added_items_fails) {
  lsp_workspace_configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"myitem"_sv, [&myitem_callback_called](
                                     std::string_view new_value) {
    myitem_callback_called = true;
    ADD_FAILURE() << "myitem callback should not have been called; new_value="
                  << new_value;
  });

  easy_simdjson_parser result("[]"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  ASSERT_FALSE(ok);

  EXPECT_FALSE(myitem_callback_called);
}

TEST(test_lsp_workspace_configuration,
     more_values_than_config_fails_but_calls_callback_anyway) {
  lsp_workspace_configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"myitem"_sv,
                  [&myitem_callback_called](std::string_view new_value) {
                    myitem_callback_called = true;
                    EXPECT_EQ(new_value, "val");
                  });

  easy_simdjson_parser result(R"(["val", "otherval"])"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  ASSERT_FALSE(ok);

  EXPECT_TRUE(myitem_callback_called);
}

TEST(test_lsp_workspace_configuration, null_is_coerced_to_empty_string) {
  lsp_workspace_configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"myitem"_sv,
                  [&myitem_callback_called](std::string_view new_value) {
                    myitem_callback_called = true;
                    EXPECT_EQ(new_value, "");
                  });

  easy_simdjson_parser result(R"([null])"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  EXPECT_TRUE(ok);

  EXPECT_TRUE(myitem_callback_called);
}

TEST(test_lsp_workspace_configuration, non_array_config_response_fails) {
  lsp_workspace_configuration config;

  easy_simdjson_parser result("{}"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  ASSERT_FALSE(ok);
}

TEST(test_lsp_workspace_configuration, empty_config_notification_does_nothing) {
  lsp_workspace_configuration config;

  easy_simdjson_parser result("{}"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_notification(result.value_object());
  ASSERT_TRUE(ok);
}

TEST(test_lsp_workspace_configuration,
     config_notification_calls_item_callbacks) {
  lsp_workspace_configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"myitem"_sv,
                  [&myitem_callback_called](std::string_view new_value) {
                    myitem_callback_called = true;
                    EXPECT_EQ(new_value, "hello");
                  });

  easy_simdjson_parser result(R"({"myitem": "hello"})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_notification(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_TRUE(myitem_callback_called);
}

TEST(test_lsp_workspace_configuration,
     config_notification_ignores_extra_entries) {
  lsp_workspace_configuration config;
  int myitem_callback_called_count = 0;
  config.add_item(u8"myitem"_sv,
                  [&myitem_callback_called_count](std::string_view new_value) {
                    myitem_callback_called_count += 1;
                    EXPECT_EQ(new_value, "hello");
                  });

  easy_simdjson_parser result(
      R"({"myitem": "hello", "extraitem": "hi"})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_notification(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_EQ(myitem_callback_called_count, 1);
}

TEST(test_lsp_workspace_configuration,
     config_notification_does_not_call_callback_for_unnotified_items) {
  lsp_workspace_configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"myitem"_sv, [&myitem_callback_called](
                                     std::string_view new_value) {
    myitem_callback_called = true;
    ADD_FAILURE() << "myitem callback should not have been called; new_value="
                  << new_value;
  });

  easy_simdjson_parser result(R"({})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_notification(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_FALSE(myitem_callback_called);
}

TEST(test_lsp_workspace_configuration,
     initialization_options_calls_item_callbacks) {
  lsp_workspace_configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"mysection.myitem"_sv,
                  [&myitem_callback_called](std::string_view new_value) {
                    myitem_callback_called = true;
                    EXPECT_EQ(new_value, "hello");
                  });

  easy_simdjson_parser result(R"({"mysection.myitem": "hello"})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_initialization_options(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_TRUE(myitem_callback_called);
}

TEST(test_lsp_workspace_configuration,
     initialization_options_ignores_extra_entries) {
  lsp_workspace_configuration config;
  int myitem_callback_called_count = 0;
  config.add_item(u8"mysection.myitem"_sv,
                  [&myitem_callback_called_count](std::string_view new_value) {
                    myitem_callback_called_count += 1;
                    EXPECT_EQ(new_value, "hello");
                  });

  easy_simdjson_parser result(
      R"({"mysection.myitem": "hello", "mysection.extraitem": "hi"})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_initialization_options(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_EQ(myitem_callback_called_count, 1);
}

TEST(test_lsp_workspace_configuration,
     initialization_options_does_not_call_callback_for_unnotified_items) {
  lsp_workspace_configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"mysection.myitem"_sv, [&myitem_callback_called](
                                               std::string_view new_value) {
    myitem_callback_called = true;
    ADD_FAILURE()
        << "mysection.myitem callback should not have been called; new_value="
        << new_value;
  });

  easy_simdjson_parser result(R"({})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_initialization_options(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_FALSE(myitem_callback_called);
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
