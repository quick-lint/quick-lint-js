// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/port/char8.h>
#include <string_view>
#include <vector>

#define EXPECT_DEFAULT_CONFIG(config)                                   \
  do {                                                                  \
    EXPECT_TRUE((config).globals().find(u8"Array"_sv));                 \
    EXPECT_TRUE((config).globals().find(u8"console"_sv));               \
    EXPECT_FALSE((config).globals().find(u8"variableDoesNotExist"_sv)); \
  } while (false)

using ::testing::ElementsAreArray;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
void load_from_json(configuration&, padded_string_view json);
void load_from_json(configuration&, string8_view json);

TEST(test_configuration, browser_globals_are_present_by_default) {
  configuration c;

  constexpr string8_view global_variables[] = {
      // Variables:
      u8"document"_sv,
      u8"frames"_sv,
      u8"length"_sv,
      u8"name"_sv,
      u8"window"_sv,

      // Functions:
      u8"alert"_sv,
      u8"btoa"_sv,
      u8"close"_sv,
      u8"console"_sv,
      u8"postMessage"_sv,
      u8"setTimeout"_sv,

      // Event handlers:
      u8"onabort"_sv,
      u8"onmouseenter"_sv,
      u8"onunload"_sv,

      // Classes:
      u8"Image"_sv,
      u8"MessageEvent"_sv,
      u8"SVGAnimatedRect"_sv,
  };
  for (string8_view variable_name : global_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    std::optional<global_declared_variable> var =
        c.globals().find(variable_name);
    ASSERT_TRUE(var.has_value());
  }
}

TEST(test_configuration, ecmascript_globals_are_present_by_default) {
  configuration c;

  constexpr string8_view writable_global_variables[] = {
      // ECMA-262 18.1 Value Properties of the Global Object
      u8"globalThis"_sv,

      // ECMA-262 18.2 Function Properties of the Global Object
      u8"decodeURI"_sv,
      u8"decodeURIComponent"_sv,
      u8"encodeURI"_sv,
      u8"encodeURIComponent"_sv,
      u8"eval"_sv,
      u8"isFinite"_sv,
      u8"isNaN"_sv,
      u8"parseFloat"_sv,
      u8"parseInt"_sv,

      // ECMA-262 18.3 Constructor Properties of the Global Object
      u8"Array"_sv,
      u8"ArrayBuffer"_sv,
      u8"BigInt"_sv,
      u8"BigInt64Array"_sv,
      u8"BigUint64Array"_sv,
      u8"Boolean"_sv,
      u8"DataView"_sv,
      u8"Date"_sv,
      u8"Error"_sv,
      u8"EvalError"_sv,
      u8"Float32Array"_sv,
      u8"Float64Array"_sv,
      u8"Function"_sv,
      u8"Int16Array"_sv,
      u8"Int32Array"_sv,
      u8"Int8Array"_sv,
      u8"Map"_sv,
      u8"Number"_sv,
      u8"Object"_sv,
      u8"Promise"_sv,
      u8"Proxy"_sv,
      u8"RangeError"_sv,
      u8"ReferenceError"_sv,
      u8"RegExp"_sv,
      u8"Set"_sv,
      u8"SharedArrayBuffer"_sv,
      u8"String"_sv,
      u8"Symbol"_sv,
      u8"SyntaxError"_sv,
      u8"TypeError"_sv,
      u8"URIError"_sv,
      u8"Uint16Array"_sv,
      u8"Uint32Array"_sv,
      u8"Uint8Array"_sv,
      u8"Uint8ClampedArray"_sv,
      u8"WeakMap"_sv,
      u8"WeakSet"_sv,

      // ECMA-262 18.4 Other Properties of the Global Object
      u8"Atomics"_sv,
      u8"JSON"_sv,
      u8"Math"_sv,
      u8"Reflect"_sv,
  };
  for (string8_view variable_name : writable_global_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    std::optional<global_declared_variable> var =
        c.globals().find(variable_name);
    ASSERT_TRUE(var.has_value());
    EXPECT_TRUE(var->is_writable);
    EXPECT_TRUE(var->is_shadowable);
  }

  constexpr string8_view non_writable_global_variables[] = {
      // ECMA-262 18.1 Value Properties of the Global Object
      u8"Infinity"_sv,
      u8"NaN"_sv,
      u8"undefined"_sv,
  };
  for (string8_view variable_name : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    std::optional<global_declared_variable> var =
        c.globals().find(variable_name);
    ASSERT_TRUE(var.has_value());
    EXPECT_FALSE(var->is_writable);
    EXPECT_TRUE(var->is_shadowable);
  }
}

TEST(test_configuration, node_js_globals_are_present_by_default) {
  configuration c;

  constexpr string8_view writable_commonjs_variables[] = {
      u8"__dirname"_sv, u8"__filename"_sv, u8"exports"_sv,
      u8"module"_sv,    u8"require"_sv,
  };
  for (string8_view variable_name : writable_commonjs_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    std::optional<global_declared_variable> var =
        c.globals().find(variable_name);
    ASSERT_TRUE(var.has_value());
    EXPECT_TRUE(var->is_writable);
  }
}

TEST(test_configuration, default_globals_are_all_shadowable) {
  configuration c;
  for (string8_view variable_name : c.globals().get_all_variable_names()) {
    SCOPED_TRACE(out_string8(variable_name));
    std::optional<global_declared_variable> var =
        c.globals().find(variable_name);
    ASSERT_TRUE(var.has_value());
    EXPECT_TRUE(var->is_shadowable);
  }
}

TEST(test_configuration, missing_global_variable_does_not_exist_by_default) {
  configuration c;
  ASSERT_FALSE(c.globals().find(u8"variableDoesNotExist"_sv));
}

TEST(test_configuration, add_new_global_variable) {
  configuration c;

  c.add_global_variable(global_declared_variable{
      .name = u8"myGlobalVariable"_sv,
      .is_writable = true,
      .is_shadowable = true,
  });

  std::optional<global_declared_variable> found_var =
      c.globals().find(u8"myGlobalVariable"_sv);
  EXPECT_TRUE(found_var.has_value());
  EXPECT_EQ(found_var->name, u8"myGlobalVariable"_sv);
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(test_configuration, added_global_variable_shadows_default) {
  {
    configuration c;
    ASSERT_TRUE(c.globals().find(u8"Array"_sv).has_value())
        << "'Array' should be declared by default";
  }

  configuration c;

  c.add_global_variable(global_declared_variable{
      .name = u8"Array"_sv,
      .is_writable = false,
      .is_shadowable = false,
  });

  std::optional<global_declared_variable> found_var =
      c.globals().find(u8"Array"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_FALSE(found_var->is_shadowable);
  EXPECT_FALSE(found_var->is_writable);
}

TEST(test_configuration,
     adding_global_variable_does_not_disable_default_groups) {
  configuration c;

  c.add_global_variable(global_declared_variable{
      .name = u8"testGlobalVariable"_sv,
      .is_writable = true,
      .is_shadowable = true,
  });

  EXPECT_TRUE(c.globals().find(u8"Array"_sv));
  EXPECT_TRUE(c.globals().find(u8"console"_sv));
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
  EXPECT_FALSE(c.globals().find(u8"Array"_sv));
  EXPECT_FALSE(c.globals().find(u8"console"_sv));
}

TEST(test_configuration, add_ecmascript_group) {
  configuration c;
  c.reset_global_groups();
  EXPECT_TRUE(c.add_global_group(u8"ecmascript"_sv));

  EXPECT_TRUE(c.globals().find(u8"Array"_sv));
  EXPECT_FALSE(c.globals().find(u8"console"_sv));
}

TEST(test_configuration, add_node_js_group) {
  configuration c;
  c.reset_global_groups();
  EXPECT_TRUE(c.add_global_group(u8"node.js"_sv));

  EXPECT_FALSE(c.globals().find(u8"Array"_sv));
  EXPECT_TRUE(c.globals().find(u8"console"_sv));
}

TEST(test_configuration, add_invalid_group) {
  configuration c;
  c.reset_global_groups();
  EXPECT_FALSE(c.add_global_group(u8"groupDoesNotExist"_sv));
}

TEST(test_configuration, add_ecmascript_and_node_js_groups) {
  {
    configuration c;
    c.reset_global_groups();

    EXPECT_TRUE(c.add_global_group(u8"node.js"_sv));
    EXPECT_TRUE(c.add_global_group(u8"ecmascript"_sv));

    EXPECT_TRUE(c.globals().find(u8"Array"_sv));
    EXPECT_TRUE(c.globals().find(u8"console"_sv));
  }

  {
    configuration c;
    c.reset_global_groups();

    EXPECT_TRUE(c.add_global_group(u8"ecmascript"_sv));
    EXPECT_TRUE(c.add_global_group(u8"node.js"_sv));

    EXPECT_TRUE(c.globals().find(u8"Array"_sv));
    EXPECT_TRUE(c.globals().find(u8"console"_sv));
  }
}

TEST(test_configuration, literally_anything_group_by_name) {
  configuration c;
  c.reset_global_groups();
  EXPECT_TRUE(c.add_global_group(u8"literally-anything"_sv));

  EXPECT_TRUE(c.globals().find(u8"Array"_sv));
  EXPECT_TRUE(c.globals().find(u8"console"_sv));
  std::optional<global_declared_variable> found_var =
      c.globals().find(u8"thisVariableWasNeverSpecifiedButStillExists"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(test_configuration,
     literally_anything_group_preserves_other_global_properties) {
  configuration c;
  c.reset_global_groups();
  EXPECT_TRUE(c.add_global_group(u8"ecmascript"_sv));
  EXPECT_TRUE(c.add_global_group(u8"literally-anything"_sv));
  EXPECT_TRUE(c.add_global_group(u8"node.js"_sv));
  c.add_global_variable(global_declared_variable{
      .name = u8"testGlobalVariable"_sv,
      .is_writable = false,
      .is_shadowable = false,
  });
  c.add_global_variable(global_declared_variable{
      .name = u8"require"_sv,
      .is_writable = true,
      .is_shadowable = false,
  });

  std::optional<global_declared_variable> found_var;

  found_var = c.globals().find(u8"Array"_sv);
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);

  found_var = c.globals().find(u8"require"_sv);
  EXPECT_FALSE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);

  found_var = c.globals().find(u8"Infinity"_sv);
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_FALSE(found_var->is_writable);

  found_var = c.globals().find(u8"testGlobalVariable"_sv);
  EXPECT_FALSE(found_var->is_shadowable);
  EXPECT_FALSE(found_var->is_writable);
}

TEST(test_configuration, overwrite_global_variable_from_group) {
  string8_view var_name = u8"Infinity"_sv;

  {
    configuration c;
    ASSERT_TRUE(c.globals().find(var_name).has_value())
        << out_string8(var_name)
        << " should be defined in a default global group";
  }

  for (bool is_shadowable : {false, true}) {
    for (bool is_writable : {false, true}) {
      SCOPED_TRACE(is_shadowable ? "shadowable" : "not shadowable");
      SCOPED_TRACE(is_writable ? "writable" : "not writable");
      configuration c;
      c.add_global_variable(global_declared_variable{
          .name = var_name,
          .is_writable = is_writable,
          .is_shadowable = is_shadowable,
      });
      std::optional<global_declared_variable> var = c.globals().find(var_name);
      ASSERT_TRUE(var.has_value());
      EXPECT_EQ(var->is_shadowable, is_shadowable);
      EXPECT_EQ(var->is_writable, is_writable);
    }
  }
}

TEST(test_configuration, overwrite_global_variable) {
  string8_view var_name = u8"testvariable"_sv;

  for (bool original_is_shadowable : {false, true}) {
    for (bool original_is_writable : {false, true}) {
      for (bool override_is_shadowable : {false, true}) {
        for (bool override_is_writable : {false, true}) {
          SCOPED_TRACE(original_is_shadowable ? "original is shadowable"
                                              : "original is not shadowable");
          SCOPED_TRACE(original_is_writable ? "original is writable"
                                            : "original is not writable");
          SCOPED_TRACE(override_is_shadowable ? "override is shadowable"
                                              : "override is not shadowable");
          SCOPED_TRACE(override_is_writable ? "override is writable"
                                            : "override is not writable");
          configuration c;
          c.add_global_variable(global_declared_variable{
              .name = var_name,
              .is_writable = original_is_writable,
              .is_shadowable = original_is_shadowable,
          });
          c.add_global_variable(global_declared_variable{
              .name = var_name,
              .is_writable = override_is_writable,
              .is_shadowable = override_is_shadowable,
          });
          std::optional<global_declared_variable> var =
              c.globals().find(var_name);
          ASSERT_TRUE(var.has_value());
          EXPECT_EQ(var->is_shadowable, override_is_shadowable);
          EXPECT_EQ(var->is_writable, override_is_writable);
        }
      }
    }
  }
}

TEST(test_configuration_json, empty_json_creates_default_config) {
  configuration c;
  load_from_json(c, u8"{}"_sv);

  EXPECT_DEFAULT_CONFIG(c);
}

TEST(test_configuration_json, true_global_groups_leaves_defaults) {
  configuration c;
  load_from_json(c, u8R"({"global-groups": true})"_sv);

  EXPECT_DEFAULT_CONFIG(c);
}

TEST(test_configuration_json, false_global_groups_disables_all_groups) {
  configuration c;
  load_from_json(c, u8R"({"global-groups": false})"_sv);

  EXPECT_FALSE(c.globals().find(u8"Array"_sv));
  EXPECT_FALSE(c.globals().find(u8"console"_sv));
}

TEST(test_configuration_json, empty_global_groups_disables_all_groups) {
  configuration c;
  load_from_json(c, u8R"({"global-groups": []})"_sv);

  EXPECT_FALSE(c.globals().find(u8"Array"_sv));
  EXPECT_FALSE(c.globals().find(u8"console"_sv));
}

TEST(test_configuration_json, global_groups_with_node_js_enables_only_node_js) {
  configuration c;
  load_from_json(c, u8R"({"global-groups": ["node.js"]})"_sv);

  EXPECT_TRUE(c.globals().find(u8"console"_sv));
  EXPECT_FALSE(c.globals().find(u8"Array"_sv));
}

TEST(test_configuration_json, empty_globals_leaves_defaults) {
  configuration c;
  load_from_json(c, u8R"({"globals": {}})"_sv);

  EXPECT_DEFAULT_CONFIG(c);
}

TEST(test_configuration_json, true_global_is_usable) {
  configuration c;
  load_from_json(c, u8R"({"globals": {"myTestGlobalVariable": true}})"_sv);

  std::optional<global_declared_variable> found_var =
      c.globals().find(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(test_configuration_json, empty_object_global_is_usable) {
  configuration c;
  load_from_json(c, u8R"({"globals": {"myTestGlobalVariable": {}}})"_sv);

  std::optional<global_declared_variable> found_var =
      c.globals().find(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(test_configuration_json, unwritable_global_is_not_writable) {
  configuration c;
  load_from_json(
      c, u8R"({"globals": {"myTestGlobalVariable": {"writable": false}}})"_sv);

  std::optional<global_declared_variable> found_var =
      c.globals().find(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_FALSE(found_var->is_writable);
}

TEST(test_configuration_json, unshadowable_global_is_not_shadowable) {
  configuration c;
  load_from_json(
      c,
      u8R"({"globals": {"myTestGlobalVariable": {"shadowable": false}}})"_sv);

  std::optional<global_declared_variable> found_var =
      c.globals().find(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_FALSE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(test_configuration_json, false_global_overrides_global_group) {
  configuration c;
  load_from_json(
      c,
      u8R"({"globals": {"console": false}, "global-groups": ["ecmascript", "node.js"]})"_sv);

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
  // u8R"({"global-groups": {42}})"_sv,
  // u8R"({"globals":{"a":{"shadowable":[}}}})"_sv,
  for (string8_view json_string : {
           u8R"({)"_sv,
           u8R"({"globals)"_sv,
           u8R"({"globals": {42}})"_sv,
           u8"{\"globals\":{\"globals\":\u0000{}}}}"_sv,
           u8R"({"globals":{"G":{":"}}})"_sv,
           u8R"({"globals":}})"_sv,
           u8R"({"global-groups":=)"_sv,
           u8R"({"globals":{"g":f}})"_sv,
           u8R"({"global-groups":[)"_sv,
           u8R"({"global-groups":t)"_sv,
       }) {
    SCOPED_TRACE(out_string8(json_string));
    configuration c;

    padded_string json(json_string);
    diag_collector errors;
    c.load_from_json(&json, &errors);

    // TODO(strager): Check diag_config_json_syntax_error::where.
    EXPECT_THAT(errors.errors,
                ElementsAreArray({DIAG_TYPE(diag_config_json_syntax_error)}));
  }
}

TEST(test_configuration_json, bad_schema_in_globals_reports_error) {
  {
    padded_string json(u8R"({"globals":["myGlobalVariable"]})"_sv);
    configuration c;
    diag_collector errors;
    c.load_from_json(&json, &errors);
    EXPECT_THAT(errors.errors,
                ElementsAreArray({DIAG_TYPE_OFFSETS(
                    &json, diag_config_globals_type_mismatch,  //
                    value, strlen(u8R"({"globals":)"), u8"["_sv)}));
    EXPECT_FALSE(c.globals().find(u8"myGlobalVariable"_sv))
        << "invalid global should be ignored";
  }

  {
    padded_string json(
        u8R"({"globals":{"testBefore":true,"testBad":"string","testAfter":true}})"_sv);
    configuration c;
    diag_collector errors;
    c.load_from_json(&json, &errors);
    EXPECT_THAT(
        errors.errors,
        ElementsAreArray({DIAG_TYPE_OFFSETS(
            &json, diag_config_globals_descriptor_type_mismatch,  //
            descriptor, strlen(u8R"({"globals":{"testBefore":true,"testBad":)"),
            u8R"("string")"_sv)}));

    EXPECT_TRUE(c.globals().find(u8"testBefore"_sv))
        << "valid globals before should work";
    EXPECT_TRUE(c.globals().find(u8"testAfter"_sv))
        << "valid globals after should work";
    EXPECT_FALSE(c.globals().find(u8"testBad"_sv))
        << "invalid global should be ignored";
  }

  {
    padded_string json(
        u8R"({"globals":{"testBefore":true,"testBad":{"writable":false,"shadowable":"string"},"testAfter":true}})"_sv);
    configuration c;
    diag_collector errors;
    c.load_from_json(&json, &errors);
    EXPECT_THAT(
        errors.errors,
        ElementsAreArray({DIAG_TYPE_OFFSETS(
            &json, diag_config_globals_descriptor_shadowable_type_mismatch,  //
            value,
            strlen(
                u8R"({"globals":{"testBefore":true,"testBad":{"writable":false,"shadowable":)"),
            u8R"("string")"_sv)}));

    EXPECT_TRUE(c.globals().find(u8"testBefore"_sv))
        << "valid globals before should work";
    EXPECT_TRUE(c.globals().find(u8"testAfter"_sv))
        << "valid globals after should work";
    std::optional<global_declared_variable> var =
        c.globals().find(u8"testBad"_sv);
    ASSERT_TRUE(var.has_value()) << "broken global should be present";
    EXPECT_FALSE(var->is_writable)
        << "valid property on broken global should work";
    EXPECT_TRUE(var->is_shadowable)
        << "invalid global property should be ignored (default)";
  }

  {
    padded_string json(
        u8R"({"globals":{"testBefore":true,"testBad":{"writable":"string","shadowable":false},"testAfter":true}})"_sv);
    configuration c;
    diag_collector errors;
    c.load_from_json(&json, &errors);
    EXPECT_THAT(
        errors.errors,
        ElementsAreArray({DIAG_TYPE_OFFSETS(
            &json, diag_config_globals_descriptor_writable_type_mismatch,  //
            value,
            strlen(u8R"({"globals":{"testBefore":true,"testBad":{"writable":)"),
            u8R"("string")"_sv)}));

    EXPECT_TRUE(c.globals().find(u8"testBefore"_sv))
        << "valid globals before should work";
    EXPECT_TRUE(c.globals().find(u8"testAfter"_sv))
        << "valid globals after should work";
    std::optional<global_declared_variable> var =
        c.globals().find(u8"testBad"_sv);
    ASSERT_TRUE(var.has_value()) << "broken global should be present";
    EXPECT_TRUE(var->is_writable)
        << "invalid global property should be ignored (default)";
    EXPECT_FALSE(var->is_shadowable)
        << "valid property on broken global should work";
  }
}

TEST(test_configuration_json, bad_schema_in_global_groups_reports_error) {
  {
    padded_string json(u8R"({"global-groups":{"browser":true}})"_sv);
    configuration c;
    diag_collector errors;
    c.load_from_json(&json, &errors);
    EXPECT_THAT(errors.errors,
                ElementsAreArray({DIAG_TYPE_OFFSETS(
                    &json, diag_config_global_groups_type_mismatch,  //
                    value, strlen(u8R"({"global-groups":)"), u8"{"_sv)}));
    EXPECT_TRUE(c.globals().find(u8"Array"_sv))
        << "invalid global-groups should be ignored";
  }

  {
    padded_string json(
        u8R"({"global-groups":["browser",false,"ecmascript"]})"_sv);
    configuration c;
    diag_collector errors;
    c.load_from_json(&json, &errors);
    EXPECT_THAT(errors.errors,
                ElementsAreArray({DIAG_TYPE_OFFSETS(
                    &json, diag_config_global_groups_group_type_mismatch,  //
                    group, strlen(u8R"({"global-groups":["browser",)"),
                    u8"false"_sv)}));

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
  padded_string json(u8"{ \"globals\": { \"a\": \"b\"  \n\t\r }}"_sv);
  configuration c;
  diag_collector errors;
  c.load_from_json(&json, &errors);

  EXPECT_THAT(
      errors.errors,
      ElementsAreArray({DIAG_TYPE_OFFSETS(
          &json, diag_config_globals_descriptor_type_mismatch,  //
          descriptor, strlen(u8R"({ "globals": { "a": )"), u8R"("b")"_sv)}));
}

void load_from_json(configuration& config, padded_string_view json) {
  diag_collector errors;
  config.load_from_json(json, &errors);
  EXPECT_THAT(errors.errors, ::testing::IsEmpty());
}

void load_from_json(configuration& config, string8_view json) {
  padded_string padded_json(json);
  load_from_json(config, &padded_json);
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
