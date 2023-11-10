// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/json.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/util/cast.h>
#include <simdjson.h>

namespace quick_lint_js {
namespace {
TEST(Test_JSON, escapes_backslashes) {
  Memory_Output_Stream json;
  write_json_escaped_string(json, u8R"(hello\world)"_sv);
  json.flush();
  EXPECT_EQ(json.get_flushed_string8(), u8R"(hello\\world)");
}

TEST(Test_JSON, escapes_double_quotes) {
  Memory_Output_Stream json;
  write_json_escaped_string(json, u8R"(hello"world)"_sv);
  json.flush();
  EXPECT_EQ(json.get_flushed_string8(), u8R"(hello\"world)");
}

TEST(Test_JSON, escapes_newlines) {
  Memory_Output_Stream json;
  write_json_escaped_string(json, u8"hello\nworld"_sv);
  json.flush();
  EXPECT_EQ(json.get_flushed_string8(), u8R"(hello\nworld)");
}

TEST(Test_JSON, escapes_tabs) {
  Memory_Output_Stream json;
  write_json_escaped_string(json, u8"hello\tworld"_sv);
  json.flush();
  EXPECT_EQ(json.get_flushed_string8(), u8R"(hello\tworld)");
}

TEST(Test_JSON, escapes_carriage_returns) {
  Memory_Output_Stream json;
  write_json_escaped_string(json, u8"hello\rworld"_sv);
  json.flush();
  EXPECT_EQ(json.get_flushed_string8(), u8R"(hello\rworld)");
}

TEST(Test_JSON, escapes_backspaces) {
  Memory_Output_Stream json;
  write_json_escaped_string(json, u8"hello\bworld"_sv);
  json.flush();
  EXPECT_EQ(json.get_flushed_string8(), u8R"(hello\bworld)");
}

TEST(Test_JSON, escapes_form_feeds) {
  Memory_Output_Stream json;
  write_json_escaped_string(json, u8"hello\fworld"_sv);
  json.flush();
  EXPECT_EQ(json.get_flushed_string8(), u8R"(hello\fworld)");
}

TEST(Test_JSON, ascii_characters_are_parsable_by_simdjson_ondemand) {
  for (int c = 0; c < 128; ++c) {
    String8 string = String8(u8"hello") + narrow_cast<Char8>(c) + u8"world";
    SCOPED_TRACE(out_string8(string));

    Memory_Output_Stream json;
    json.append_copy(u8'"');
    write_json_escaped_string(json, String8_View(string));
    json.append_copy(u8'"');
    json.flush();
    SCOPED_TRACE(out_string8(json.get_flushed_string8()));

    ::simdjson::padded_string json_padded(
        to_string_view(json.get_flushed_string8()));
    ::simdjson::ondemand::parser parser;
    ::simdjson::simdjson_result<::simdjson::ondemand::document> document =
        parser.iterate(json_padded);
    std::string_view parsed_string;
    ASSERT_EQ(document.get_string().get(parsed_string), ::simdjson::SUCCESS);
    EXPECT_EQ(parsed_string, to_string_view(string));
  }
}
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
