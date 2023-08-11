// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/tjson.h>

namespace quick_lint_js {
namespace {
TEST(Test_TJSON, object_lookup) {
  TJSON o(
      u8R"({
        "int": 42,
        "zero_int": 0,
        "string": "hello",
        "bool": false,
        "bool2": true,
        "nil": null
      })"_sv);

  EXPECT_EQ(o[u8"int"_sv], 42);
  EXPECT_NE(o[u8"int"_sv], 69);
  EXPECT_NE(o[u8"int"_sv], u8"42"_sv);
  EXPECT_NE(o[u8"int"_sv], true);
  EXPECT_NE(o[u8"int"_sv], nullptr);

  EXPECT_EQ(o[u8"zero_int"_sv], 0);
  EXPECT_NE(o[u8"zero_int"_sv], false);
  EXPECT_NE(o[u8"zero_int"_sv], nullptr);

  EXPECT_EQ(o[u8"string"_sv], u8"hello"_sv);
  EXPECT_NE(o[u8"string"_sv], u8"nope"_sv);
  EXPECT_NE(o[u8"string"_sv], 42);
  EXPECT_NE(o[u8"string"_sv], true);
  EXPECT_NE(o[u8"string"_sv], nullptr);

  EXPECT_EQ(o[u8"bool"_sv], false);
  EXPECT_NE(o[u8"bool"_sv], true);
  EXPECT_NE(o[u8"bool"_sv], 0);
  EXPECT_NE(o[u8"bool"_sv], u8"false"_sv);
  EXPECT_NE(o[u8"bool"_sv], nullptr);

  EXPECT_EQ(o[u8"bool2"_sv], true);
  EXPECT_NE(o[u8"bool2"_sv], false);
  EXPECT_NE(o[u8"bool2"_sv], 0);
  EXPECT_NE(o[u8"bool2"_sv], 1);
  EXPECT_NE(o[u8"bool2"_sv], u8"true"_sv);
  EXPECT_NE(o[u8"bool2"_sv], nullptr);

  EXPECT_EQ(o[u8"nil"_sv], nullptr);
  EXPECT_NE(o[u8"nil"_sv], 0);
  EXPECT_NE(o[u8"nil"_sv], u8"null"_sv);
  EXPECT_NE(o[u8"nil"_sv], u8""_sv);
  EXPECT_NE(o[u8"nil"_sv], false);
}

TEST(Test_TJSON, array_lookup) {
  TJSON o(
      u8R"([
        42,
        0,
        "hello",
        false,
        true,
        null
      ])"_sv);

  EXPECT_EQ(o[0], 42);
  EXPECT_NE(o[0], 69);
  EXPECT_NE(o[0], u8"42"_sv);
  EXPECT_NE(o[0], true);
  EXPECT_NE(o[0], nullptr);

  EXPECT_EQ(o[1], 0);
  EXPECT_NE(o[1], false);
  EXPECT_NE(o[1], nullptr);

  EXPECT_EQ(o[2], u8"hello"_sv);
  EXPECT_NE(o[2], u8"nope"_sv);
  EXPECT_NE(o[2], 42);
  EXPECT_NE(o[2], true);
  EXPECT_NE(o[2], nullptr);

  EXPECT_EQ(o[3], false);
  EXPECT_NE(o[3], true);
  EXPECT_NE(o[3], 0);
  EXPECT_NE(o[3], u8"false"_sv);
  EXPECT_NE(o[3], nullptr);

  EXPECT_EQ(o[4], true);
  EXPECT_NE(o[4], false);
  EXPECT_NE(o[4], 0);
  EXPECT_NE(o[4], 1);
  EXPECT_NE(o[4], u8"true"_sv);
  EXPECT_NE(o[4], nullptr);

  EXPECT_EQ(o[5], nullptr);
  EXPECT_NE(o[5], 0);
  EXPECT_NE(o[5], u8"null"_sv);
  EXPECT_NE(o[5], u8""_sv);
  EXPECT_NE(o[5], false);
}

TEST(Test_TJSON, array_size) {
  {
    TJSON o(u8"[]"_sv);
    EXPECT_EQ(o.size(), 0);
  }

  {
    TJSON o(u8R"([ 42, 0, "hello", false, true, null ])"_sv);
    EXPECT_EQ(o.size(), 6);
  }

  {
    TJSON o(u8"[[]]"_sv);
    EXPECT_EQ(o.size(), 1);
    EXPECT_EQ(o[0].size(), 0);
  }
}

TEST(Test_TJSON, object_size) {
  {
    TJSON o(u8"{}"_sv);
    EXPECT_EQ(o.size(), 0);
  }

  {
    TJSON o(u8R"({"a": 1, "b": true, "c": "three"})"_sv);
    EXPECT_EQ(o.size(), 3);
  }

  {
    TJSON o(u8R"({"k": {}})"_sv);
    EXPECT_EQ(o.size(), 1);
    EXPECT_EQ(o[u8"k"_sv].size(), 0);
  }
}

TEST(Test_TJSON, nested_object_lookup) {
  TJSON o(
      u8R"({
        "key": {
          "value": 100,
          "subkey": {
            "value": 200,
            "subsubkey": {
              "value": 300
            }
          }
        }
      })"_sv);
  EXPECT_EQ(o[u8"key"_sv][u8"value"_sv], 100);
  EXPECT_EQ(o[u8"key"_sv][u8"subkey"_sv][u8"value"_sv], 200);
  EXPECT_EQ(o[u8"key"_sv][u8"subkey"_sv][u8"subsubkey"_sv][u8"value"_sv], 300);
}

TEST(Test_TJSON, nested_array_lookup) {
  TJSON o(
      u8R"([
        [
          100,
          [
            200,
            [ 300 ]
          ]
        ]
      ])"_sv);
  EXPECT_EQ(o[0][0], 100);
  EXPECT_EQ(o[0][1][0], 200);
  EXPECT_EQ(o[0][1][1][0], 300);
}

TEST(Test_TJSON, to_string) {
  TJSON o(
      u8R"({
        "int": 42,
        "string": "hello",
        "bool": false,
        "bool2": true,
        "nil": null,
        "empty_array": [],
        "array": [ 1, true, "three" ],
        "empty_object": {},
        "object": { "key": "value" }
      })"_sv);

  EXPECT_EQ(o[u8"int"_sv].to_string(), u8"42"_sv);
  EXPECT_EQ(o[u8"string"_sv].to_string(), u8"\"hello\""_sv);
  EXPECT_EQ(o[u8"bool"_sv].to_string(), u8"false"_sv);
  EXPECT_EQ(o[u8"bool2"_sv].to_string(), u8"true"_sv);
  EXPECT_EQ(o[u8"nil"_sv].to_string(), u8"null"_sv);
  EXPECT_EQ(o[u8"empty_array"_sv].to_string(), u8"[]"_sv);
  EXPECT_EQ(o[u8"array"_sv].to_string(), u8"[1,true,\"three\"]"_sv);
  EXPECT_EQ(o[u8"empty_object"_sv].to_string(), u8"{}"_sv);
  EXPECT_EQ(o[u8"object"_sv].to_string(), u8"{\"key\":\"value\"}"_sv);
}

TEST(Test_TJSON, to_string_of_missing) {
  {
    TJSON o(u8R"({ "existing": true })"_sv);

    // TODO(strager): Include the missing key path in the error.
    EXPECT_EQ(o[u8"key"_sv].to_string(), u8"(error)"_sv);
    EXPECT_EQ(o[u8"key"_sv][u8"subkey"_sv].to_string(), u8"(error)"_sv);
    EXPECT_EQ(o[u8"key"_sv][0].to_string(), u8"(error)"_sv);
    EXPECT_EQ(o[u8"existing"_sv][0].to_string(), u8"(error)"_sv);
  }

  {
    TJSON o(u8"[ 42 ]"_sv);

    // TODO(strager): Include the missing key path in the error.
    EXPECT_EQ(o[1].to_string(), u8"(error)"_sv);
    EXPECT_EQ(o[1][0].to_string(), u8"(error)"_sv);
    EXPECT_EQ(o[1][u8"key"_sv].to_string(), u8"(error)"_sv);
    EXPECT_EQ(o[0][0].to_string(), u8"(error)"_sv);
  }
}

TEST(Test_TJSON, exists_for_object) {
  {
    TJSON o(u8R"({ "existing": true, "number": 0, "0": [] })"_sv);
    EXPECT_TRUE(o[u8"existing"_sv].exists());
    EXPECT_TRUE(o[u8"number"_sv].exists());
    EXPECT_TRUE(o[u8"0"_sv].exists());
    EXPECT_FALSE(o[u8"key"_sv].exists());
    EXPECT_FALSE(o[u8"key"_sv][u8"subkey"_sv].exists());
    EXPECT_FALSE(o[u8"key"_sv][0].exists());
    EXPECT_FALSE(o[0].exists());
  }

  {
    TJSON o(u8R"([ null, {"0": true} ])"_sv);
    EXPECT_TRUE(o[0].exists());
    EXPECT_TRUE(o[1].exists());
    EXPECT_FALSE(o[2].exists());
    EXPECT_FALSE(o[0][0].exists());
    EXPECT_FALSE(o[1][0].exists());
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
