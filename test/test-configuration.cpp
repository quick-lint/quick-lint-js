// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lint.h>
#include <string_view>
#include <vector>

#define EXPECT_DEFAULT_CONFIG(config)                                  \
  do {                                                                 \
    EXPECT_TRUE((config).globals().find(u8"Array"sv));                 \
    EXPECT_TRUE((config).globals().find(u8"console"sv));               \
    EXPECT_FALSE((config).globals().find(u8"variableDoesNotExist"sv)); \
  } while (false)

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
error_collector load_from_json(configuration&, padded_string_view json);

TEST(test_configuration, browser_globals_are_present_by_default) {
  configuration c;

  constexpr const char8* global_variables[] = {
      // Variables:
      u8"document",
      u8"frames",
      u8"length",
      u8"name",
      u8"window",

      // Functions:
      u8"alert",
      u8"btoa",
      u8"close",
      u8"console",
      u8"postMessage",
      u8"setTimeout",

      // Event handlers:
      u8"onabort",
      u8"onmouseenter",
      u8"onunload",

      // Classes:
      u8"Image",
      u8"MessageEvent",
      u8"SVGAnimatedRect",
  };
  for (string8_view variable_name : global_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    const global_declared_variable* var = c.globals().find(variable_name);
    ASSERT_TRUE(var);
  }
}

TEST(test_configuration, ecmascript_globals_are_present_by_default) {
  configuration c;

  constexpr const char8* writable_global_variables[] = {
      // ECMA-262 18.1 Value Properties of the Global Object
      u8"globalThis",

      // ECMA-262 18.2 Function Properties of the Global Object
      u8"decodeURI",
      u8"decodeURIComponent",
      u8"encodeURI",
      u8"encodeURIComponent",
      u8"eval",
      u8"isFinite",
      u8"isNaN",
      u8"parseFloat",
      u8"parseInt",

      // ECMA-262 18.3 Constructor Properties of the Global Object
      u8"Array",
      u8"ArrayBuffer",
      u8"BigInt",
      u8"BigInt64Array",
      u8"BigUint64Array",
      u8"Boolean",
      u8"DataView",
      u8"Date",
      u8"Error",
      u8"EvalError",
      u8"Float32Array",
      u8"Float64Array",
      u8"Function",
      u8"Int16Array",
      u8"Int32Array",
      u8"Int8Array",
      u8"Map",
      u8"Number",
      u8"Object",
      u8"Promise",
      u8"Proxy",
      u8"RangeError",
      u8"ReferenceError",
      u8"RegExp",
      u8"Set",
      u8"SharedArrayBuffer",
      u8"String",
      u8"Symbol",
      u8"SyntaxError",
      u8"TypeError",
      u8"URIError",
      u8"Uint16Array",
      u8"Uint32Array",
      u8"Uint8Array",
      u8"Uint8ClampedArray",
      u8"WeakMap",
      u8"WeakSet",

      // ECMA-262 18.4 Other Properties of the Global Object
      u8"Atomics",
      u8"JSON",
      u8"Math",
      u8"Reflect",
  };
  for (string8_view variable_name : writable_global_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    const global_declared_variable* var = c.globals().find(variable_name);
    ASSERT_TRUE(var);
    EXPECT_TRUE(var->is_writable);
    EXPECT_TRUE(var->is_shadowable);
  }

  constexpr const char8* non_writable_global_variables[] = {
      // ECMA-262 18.1 Value Properties of the Global Object
      u8"Infinity",
      u8"NaN",
      u8"undefined",
  };
  for (string8_view variable_name : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    const global_declared_variable* var = c.globals().find(variable_name);
    ASSERT_TRUE(var);
    EXPECT_FALSE(var->is_writable);
    EXPECT_TRUE(var->is_shadowable);
  }
}

TEST(test_configuration, node_js_globals_are_present_by_default) {
  configuration c;

  constexpr const char8* writable_commonjs_module_variables[] = {
      u8"__dirname", u8"__filename", u8"exports", u8"module", u8"require",
  };
  for (string8_view variable_name : writable_commonjs_module_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    const global_declared_variable* var = c.globals().find(variable_name);
    ASSERT_TRUE(var);
    EXPECT_TRUE(var->is_writable);
    EXPECT_FALSE(var->is_shadowable);
  }
}

TEST(test_configuration, missing_global_variable_does_not_exist_by_default) {
  configuration c;
  ASSERT_FALSE(c.globals().find(u8"variableDoesNotExist"sv));
}

TEST(test_configuration, add_new_global_variable) {
  configuration c;

  global_declared_variable* var =
      c.add_global_variable(u8"myGlobalVariable"_sv);
  EXPECT_EQ(var->name, u8"myGlobalVariable"_sv);
  EXPECT_TRUE(var->is_shadowable);
  EXPECT_TRUE(var->is_writable);

  const global_declared_variable* found_var =
      c.globals().find(u8"myGlobalVariable"_sv);
  EXPECT_TRUE(found_var);
}

TEST(test_configuration, added_global_variable_shadows_default) {
  configuration c;

  global_declared_variable* var = c.add_global_variable(u8"Array"_sv);
  var->is_shadowable = false;
  var->is_writable = false;

  const global_declared_variable* found_var = c.globals().find(u8"Array"_sv);
  ASSERT_TRUE(found_var);
  EXPECT_FALSE(found_var->is_shadowable);
  EXPECT_FALSE(found_var->is_writable);
}

TEST(test_configuration, removed_global_variable_shadows_default) {
  configuration c;
  c.remove_global_variable(u8"Array"_sv);
  EXPECT_FALSE(c.globals().find(u8"Array"_sv));
  EXPECT_TRUE(c.globals().find(u8"Object"_sv))
      << "defaults should still be defined";
}

TEST(test_configuration,
     resetting_global_groups_makes_all_variables_undefined) {
  configuration c;
  c.reset_global_groups();
  EXPECT_FALSE(c.globals().find(u8"Array"sv));
  EXPECT_FALSE(c.globals().find(u8"console"sv));
}

TEST(test_configuration, add_ecmascript_group) {
  configuration c;
  c.reset_global_groups();
  EXPECT_TRUE(c.add_global_group(u8"ecmascript"sv));

  EXPECT_TRUE(c.globals().find(u8"Array"sv));
  EXPECT_FALSE(c.globals().find(u8"console"sv));
}

TEST(test_configuration, add_node_js_group) {
  configuration c;
  c.reset_global_groups();
  EXPECT_TRUE(c.add_global_group(u8"node.js"sv));

  EXPECT_FALSE(c.globals().find(u8"Array"sv));
  EXPECT_TRUE(c.globals().find(u8"console"sv));
}

TEST(test_configuration, add_invalid_group) {
  configuration c;
  c.reset_global_groups();
  EXPECT_FALSE(c.add_global_group(u8"groupDoesNotExist"sv));
}

TEST(test_configuration, add_ecmascript_and_node_js_groups) {
  {
    configuration c;
    c.reset_global_groups();

    EXPECT_TRUE(c.add_global_group(u8"node.js"sv));
    EXPECT_TRUE(c.add_global_group(u8"ecmascript"sv));

    EXPECT_TRUE(c.globals().find(u8"Array"sv));
    EXPECT_TRUE(c.globals().find(u8"console"sv));
  }

  {
    configuration c;
    c.reset_global_groups();

    EXPECT_TRUE(c.add_global_group(u8"ecmascript"sv));
    EXPECT_TRUE(c.add_global_group(u8"node.js"sv));

    EXPECT_TRUE(c.globals().find(u8"Array"sv));
    EXPECT_TRUE(c.globals().find(u8"console"sv));
  }
}

TEST(test_configuration_json, empty_json_creates_default_config) {
  configuration c;

  padded_string json(u8"{}"sv);
  c.load_from_json(&json);

  EXPECT_DEFAULT_CONFIG(c);
}

TEST(test_configuration_json, true_global_groups_leaves_defaults) {
  configuration c;

  padded_string json(u8R"({"global-groups": true})"sv);
  c.load_from_json(&json);

  EXPECT_DEFAULT_CONFIG(c);
}

TEST(test_configuration_json, false_global_groups_disables_all_groups) {
  configuration c;

  padded_string json(u8R"({"global-groups": false})"sv);
  c.load_from_json(&json);

  EXPECT_FALSE(c.globals().find(u8"Array"sv));
  EXPECT_FALSE(c.globals().find(u8"console"sv));
}

TEST(test_configuration_json, empty_global_groups_disables_all_groups) {
  configuration c;

  padded_string json(u8R"({"global-groups": []})"sv);
  c.load_from_json(&json);

  EXPECT_FALSE(c.globals().find(u8"Array"sv));
  EXPECT_FALSE(c.globals().find(u8"console"sv));
}

TEST(test_configuration_json, global_groups_with_node_js_enables_only_node_js) {
  configuration c;

  padded_string json(u8R"({"global-groups": ["node.js"]})"sv);
  c.load_from_json(&json);

  EXPECT_TRUE(c.globals().find(u8"console"sv));
  EXPECT_FALSE(c.globals().find(u8"Array"sv));
}

TEST(test_configuration_json, empty_globals_leaves_defaults) {
  configuration c;

  padded_string json(u8R"({"globals": {}})"sv);
  c.load_from_json(&json);

  EXPECT_DEFAULT_CONFIG(c);
}

TEST(test_configuration_json, true_global_is_usable) {
  configuration c;

  padded_string json(u8R"({"globals": {"myTestGlobalVariable": true}})"sv);
  c.load_from_json(&json);

  const global_declared_variable* found_var =
      c.globals().find(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var);
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(test_configuration_json, empty_object_global_is_usable) {
  configuration c;

  padded_string json(u8R"({"globals": {"myTestGlobalVariable": {}}})"sv);
  c.load_from_json(&json);

  const global_declared_variable* found_var =
      c.globals().find(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var);
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(test_configuration_json, unwritable_global_is_not_writable) {
  configuration c;

  padded_string json(
      u8R"({"globals": {"myTestGlobalVariable": {"writable": false}}})"sv);
  c.load_from_json(&json);

  const global_declared_variable* found_var =
      c.globals().find(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var);
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_FALSE(found_var->is_writable);
}

TEST(test_configuration_json, unshadowable_global_is_not_shadowable) {
  configuration c;

  padded_string json(
      u8R"({"globals": {"myTestGlobalVariable": {"shadowable": false}}})"sv);
  c.load_from_json(&json);

  const global_declared_variable* found_var =
      c.globals().find(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var);
  EXPECT_FALSE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(test_configuration_json, false_global_overrides_global_group) {
  configuration c;

  padded_string json(
      u8R"({"globals": {"console": false}, "global-groups": ["ecmascript", "node.js"]})"sv);
  c.load_from_json(&json);

  EXPECT_TRUE(c.globals().find(u8"Array"_sv))
      << "ecmascript group should take effect";
  EXPECT_TRUE(c.globals().find(u8"require"_sv))
      << "node.js group should take effect";
  EXPECT_FALSE(c.globals().find(u8"console"_sv))
      << "'console' from node.js group should overwritten";
}

TEST(test_configuration_json, invalid_json_reports_error) {
  // TODO(strager): The following are erroneously treated as schema
  // errors, but should be JSON parse errors:
  // u8R"({"global-groups": {42}})"sv,
  for (string8_view json_string : {
           u8R"({)"sv,
           u8R"({"globals)"sv,
           u8R"({"globals": {42}})"sv,
           u8"{\"globals\":{\"globals\":\u0000{}}}}"sv,
           u8R"({"globals":{"G":{":"}}})"sv,
           u8R"({"globals":}})"sv,
           u8R"({"global-groups":=)"sv,
           u8R"({"globals":{"g":f}})"sv,
           u8R"({"global-groups":[)"sv,
           u8R"({"global-groups":t)"sv,
       }) {
    SCOPED_TRACE(out_string8(json_string));
    configuration c;

    padded_string json(json_string);
    c.load_from_json(&json);

    error_collector errors;
    c.report_errors(&errors);

    // TODO(strager): Check error_config_json_syntax_error::where.
    EXPECT_THAT(
        errors.errors,
        ElementsAre(::testing::VariantWith<error_config_json_syntax_error>(
            ::testing::_)));
  }
}

TEST(test_configuration_json, bad_schema_in_globals_reports_error) {
  {
    padded_string json(u8R"({"globals":["myGlobalVariable"]})"sv);
    configuration c;
    error_collector errors = load_from_json(c, &json);
    EXPECT_THAT(
        errors.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_config_globals_type_mismatch, value,
            offsets_matcher(&json, strlen(u8R"({"globals":)"), u8"["))));
    EXPECT_FALSE(c.globals().find(u8"myGlobalVariable"_sv))
        << "invalid global should be ignored";
  }

  {
    padded_string json(
        u8R"({"globals":{"testBefore":true,"testBad":"string","testAfter":true}})"sv);
    configuration c;
    error_collector errors = load_from_json(c, &json);
    EXPECT_THAT(
        errors.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_config_globals_descriptor_type_mismatch, descriptor,
            offsets_matcher(
                &json, strlen(u8R"({"globals":{"testBefore":true,"testBad":)"),
                u8R"("string")"))));

    EXPECT_TRUE(c.globals().find(u8"testBefore"_sv))
        << "valid globals before should work";
    EXPECT_TRUE(c.globals().find(u8"testAfter"_sv))
        << "valid globals after should work";
    EXPECT_FALSE(c.globals().find(u8"testBad"_sv))
        << "invalid global should be ignored";
  }

  {
    padded_string json(
        u8R"({"globals":{"testBefore":true,"testBad":{"writable":false,"shadowable":"string"},"testAfter":true}})"sv);
    configuration c;
    error_collector errors = load_from_json(c, &json);
    EXPECT_THAT(
        errors.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_config_globals_descriptor_shadowable_type_mismatch, value,
            offsets_matcher(
                &json,
                strlen(
                    u8R"({"globals":{"testBefore":true,"testBad":{"writable":false,"shadowable":)"),
                u8R"("string")"))));

    EXPECT_TRUE(c.globals().find(u8"testBefore"_sv))
        << "valid globals before should work";
    EXPECT_TRUE(c.globals().find(u8"testAfter"_sv))
        << "valid globals after should work";
    auto* var = c.globals().find(u8"testBad"_sv);
    ASSERT_TRUE(var) << "broken global should be present";
    EXPECT_FALSE(var->is_writable)
        << "valid property on broken global should work";
    EXPECT_TRUE(var->is_shadowable)
        << "invalid global property should be ignored (default)";
  }

  {
    padded_string json(
        u8R"({"globals":{"testBefore":true,"testBad":{"writable":"string","shadowable":false},"testAfter":true}})"sv);
    configuration c;
    error_collector errors = load_from_json(c, &json);
    EXPECT_THAT(
        errors.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_config_globals_descriptor_writable_type_mismatch, value,
            offsets_matcher(
                &json,
                strlen(
                    u8R"({"globals":{"testBefore":true,"testBad":{"writable":)"),
                u8R"("string")"))));

    EXPECT_TRUE(c.globals().find(u8"testBefore"_sv))
        << "valid globals before should work";
    EXPECT_TRUE(c.globals().find(u8"testAfter"_sv))
        << "valid globals after should work";
    auto* var = c.globals().find(u8"testBad"_sv);
    ASSERT_TRUE(var) << "broken global should be present";
    EXPECT_TRUE(var->is_writable)
        << "invalid global property should be ignored (default)";
    EXPECT_FALSE(var->is_shadowable)
        << "valid property on broken global should work";
  }
}

TEST(test_configuration_json, bad_schema_in_global_groups_reports_error) {
  {
    padded_string json(u8R"({"global-groups":{"browser":true}})"sv);
    configuration c;
    error_collector errors = load_from_json(c, &json);
    EXPECT_THAT(
        errors.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_config_global_groups_type_mismatch, value,
            offsets_matcher(&json, strlen(u8R"({"global-groups":)"), u8"{"))));
    EXPECT_TRUE(c.globals().find(u8"Array"_sv))
        << "invalid global-groups should be ignored";
  }

  {
    padded_string json(
        u8R"({"global-groups":["browser",false,"ecmascript"]})"sv);
    configuration c;
    error_collector errors = load_from_json(c, &json);
    EXPECT_THAT(
        errors.errors,
        ElementsAre(ERROR_TYPE_FIELD(
            error_config_global_groups_group_type_mismatch, group,
            offsets_matcher(&json, strlen(u8R"({"global-groups":["browser",)"),
                            u8"false"))));

    EXPECT_TRUE(c.globals().find(u8"Array"_sv))
        << "valid group-groups entries should take effect\n"
           "('Array' is from the 'ecmascript' group)";
    EXPECT_TRUE(c.globals().find(u8"document"_sv))
        << "valid group-groups entries should take effect\n"
           "('document' is from the 'browser' group)";
    EXPECT_FALSE(c.globals().find(u8"require"_sv))
        << "invalid global-groups entry should be ignored; "
           "it shouldn't cause the entire global-groups array to be ignored\n"
           "('require' is a default)";
  }
}

TEST(test_configuration_json, bad_global_error_excludes_trailing_whitespace) {
  // simdjson's raw_json_token function returns trailing whitespace by default.
  // Ensure the whitespace is not included in error messages.

  // According to RFC 8259, whitespace characters are U+0009, U+000A, U+000D,
  // and U+0020.
  padded_string json(u8"{ \"globals\": { \"a\": \"b\"  \n\t\r }})"sv);
  configuration c;
  error_collector errors = load_from_json(c, &json);

  EXPECT_THAT(errors.errors,
              ElementsAre(ERROR_TYPE_FIELD(
                  error_config_globals_descriptor_type_mismatch, descriptor,
                  offsets_matcher(&json, strlen(u8R"({ "globals": { "a": )"),
                                  u8R"("b")"))));
}

error_collector load_from_json(configuration& config, padded_string_view json) {
  config.load_from_json(json);
  error_collector errors;
  config.report_errors(&errors);
  return errors;
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
