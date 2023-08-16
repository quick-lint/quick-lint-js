// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/byte-buffer.h>
#include <quick-lint-js/lsp/lsp-workspace-configuration.h>
#include <quick-lint-js/tjson.h>
#include <simdjson.h>
#include <string>
#include <string_view>
#include <utility>

using ::testing::IsEmpty;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
struct Easy_SIMDJSON_Parser {
  explicit Easy_SIMDJSON_Parser(::simdjson::padded_string json)
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

TEST(Test_LSP_Workspace_Configuration, empty_config_request) {
  LSP_Workspace_Configuration config;

  Byte_Buffer request_json;
  config.build_request(77, request_json);

  TJSON request(request_json);
  EXPECT_EQ(request[u8"jsonrpc"_sv], u8"2.0"_sv);
  EXPECT_EQ(request[u8"id"_sv], 77);
  EXPECT_EQ(request[u8"method"_sv], u8"workspace/configuration"_sv);
  TJSON_Value items = request[u8"params"_sv][u8"items"_sv];
  EXPECT_THAT(items.try_get_array().value(), IsEmpty());
}

TEST(Test_LSP_Workspace_Configuration, config_request_with_three_items) {
  LSP_Workspace_Configuration config;
  config.add_item(u8"first"_sv, [](std::string_view) {});
  config.add_item(u8"second"_sv, [](std::string_view) {});
  config.add_item(u8"third"_sv, [](std::string_view) {});

  Byte_Buffer request_json;
  config.build_request(77, request_json);

  TJSON request(request_json);
  TJSON_Value request_items = request[u8"params"_sv][u8"items"_sv];
  ASSERT_EQ(request_items.size(), 3);
  EXPECT_EQ(request_items[0][u8"section"_sv], u8"first"_sv);
  EXPECT_EQ(request_items[1][u8"section"_sv], u8"second"_sv);
  EXPECT_EQ(request_items[2][u8"section"_sv], u8"third"_sv);
}

TEST(Test_LSP_Workspace_Configuration, empty_config_response) {
  LSP_Workspace_Configuration config;

  Easy_SIMDJSON_Parser result("[]"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  ASSERT_TRUE(ok);
}

TEST(Test_LSP_Workspace_Configuration, config_response_with_strings) {
  std::string items[3];
  LSP_Workspace_Configuration config;
  config.add_item(u8"first"_sv, [&items](std::string_view new_value) {
    items[0] = new_value;
  });
  config.add_item(u8"second"_sv, [&items](std::string_view new_value) {
    items[1] = new_value;
  });
  config.add_item(u8"third"_sv, [&items](std::string_view new_value) {
    items[2] = new_value;
  });

  Easy_SIMDJSON_Parser result(
      R"(["firstval", "secondval", "thirdval"])"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  ASSERT_TRUE(ok);

  EXPECT_EQ(items[0], "firstval");
  EXPECT_EQ(items[1], "secondval");
  EXPECT_EQ(items[2], "thirdval");
}

TEST(Test_LSP_Workspace_Configuration,
     empty_config_response_with_added_items_fails) {
  LSP_Workspace_Configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"myitem"_sv, [&myitem_callback_called](
                                     std::string_view new_value) {
    myitem_callback_called = true;
    ADD_FAILURE() << "myitem callback should not have been called; new_value="
                  << new_value;
  });

  Easy_SIMDJSON_Parser result("[]"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  ASSERT_FALSE(ok);

  EXPECT_FALSE(myitem_callback_called);
}

TEST(Test_LSP_Workspace_Configuration,
     more_values_than_config_fails_but_calls_callback_anyway) {
  LSP_Workspace_Configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"myitem"_sv,
                  [&myitem_callback_called](std::string_view new_value) {
                    myitem_callback_called = true;
                    EXPECT_EQ(new_value, "val");
                  });

  Easy_SIMDJSON_Parser result(R"(["val", "otherval"])"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  ASSERT_FALSE(ok);

  EXPECT_TRUE(myitem_callback_called);
}

TEST(Test_LSP_Workspace_Configuration, null_is_coerced_to_empty_string) {
  LSP_Workspace_Configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"myitem"_sv,
                  [&myitem_callback_called](std::string_view new_value) {
                    myitem_callback_called = true;
                    EXPECT_EQ(new_value, "");
                  });

  Easy_SIMDJSON_Parser result(R"([null])"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  EXPECT_TRUE(ok);

  EXPECT_TRUE(myitem_callback_called);
}

TEST(Test_LSP_Workspace_Configuration, non_array_config_response_fails) {
  LSP_Workspace_Configuration config;

  Easy_SIMDJSON_Parser result("{}"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_response(result.value);
  ASSERT_FALSE(ok);
}

TEST(Test_LSP_Workspace_Configuration, empty_config_notification_does_nothing) {
  LSP_Workspace_Configuration config;

  Easy_SIMDJSON_Parser result("{}"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_notification(result.value_object());
  ASSERT_TRUE(ok);
}

TEST(Test_LSP_Workspace_Configuration,
     config_notification_calls_item_callbacks) {
  LSP_Workspace_Configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"myitem"_sv,
                  [&myitem_callback_called](std::string_view new_value) {
                    myitem_callback_called = true;
                    EXPECT_EQ(new_value, "hello");
                  });

  Easy_SIMDJSON_Parser result(R"({"myitem": "hello"})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_notification(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_TRUE(myitem_callback_called);
}

TEST(Test_LSP_Workspace_Configuration,
     config_notification_ignores_extra_entries) {
  LSP_Workspace_Configuration config;
  int myitem_callback_called_count = 0;
  config.add_item(u8"myitem"_sv,
                  [&myitem_callback_called_count](std::string_view new_value) {
                    myitem_callback_called_count += 1;
                    EXPECT_EQ(new_value, "hello");
                  });

  Easy_SIMDJSON_Parser result(
      R"({"myitem": "hello", "extraitem": "hi"})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_notification(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_EQ(myitem_callback_called_count, 1);
}

TEST(Test_LSP_Workspace_Configuration,
     config_notification_does_not_call_callback_for_unnotified_items) {
  LSP_Workspace_Configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"myitem"_sv, [&myitem_callback_called](
                                     std::string_view new_value) {
    myitem_callback_called = true;
    ADD_FAILURE() << "myitem callback should not have been called; new_value="
                  << new_value;
  });

  Easy_SIMDJSON_Parser result(R"({})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_notification(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_FALSE(myitem_callback_called);
}

TEST(Test_LSP_Workspace_Configuration,
     initialization_options_calls_item_callbacks) {
  LSP_Workspace_Configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"mysection.myitem"_sv,
                  [&myitem_callback_called](std::string_view new_value) {
                    myitem_callback_called = true;
                    EXPECT_EQ(new_value, "hello");
                  });

  Easy_SIMDJSON_Parser result(R"({"mysection.myitem": "hello"})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_initialization_options(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_TRUE(myitem_callback_called);
}

TEST(Test_LSP_Workspace_Configuration,
     initialization_options_ignores_extra_entries) {
  LSP_Workspace_Configuration config;
  int myitem_callback_called_count = 0;
  config.add_item(u8"mysection.myitem"_sv,
                  [&myitem_callback_called_count](std::string_view new_value) {
                    myitem_callback_called_count += 1;
                    EXPECT_EQ(new_value, "hello");
                  });

  Easy_SIMDJSON_Parser result(
      R"({"mysection.myitem": "hello", "mysection.extraitem": "hi"})"_padded);
  ASSERT_EQ(result.error, ::simdjson::SUCCESS);
  bool ok = config.process_initialization_options(result.value_object());
  ASSERT_TRUE(ok);

  EXPECT_EQ(myitem_callback_called_count, 1);
}

TEST(Test_LSP_Workspace_Configuration,
     initialization_options_does_not_call_callback_for_unnotified_items) {
  LSP_Workspace_Configuration config;
  bool myitem_callback_called = false;
  config.add_item(u8"mysection.myitem"_sv, [&myitem_callback_called](
                                               std::string_view new_value) {
    myitem_callback_called = true;
    ADD_FAILURE()
        << "mysection.myitem callback should not have been called; new_value="
        << new_value;
  });

  Easy_SIMDJSON_Parser result(R"({})"_padded);
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
