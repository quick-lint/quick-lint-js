// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/port/char8.h>
#include <string_view>
#include <vector>

#define EXPECT_DEFAULT_CONFIG(config)                                          \
  do {                                                                         \
    EXPECT_TRUE((config).globals().find_runtime_or_type(u8"Array"_sv));        \
    EXPECT_TRUE((config).globals().find_runtime_or_type(u8"console"_sv));      \
    EXPECT_FALSE(                                                              \
        (config).globals().find_runtime_or_type(u8"variableDoesNotExist"_sv)); \
  } while (false)

using ::testing::ElementsAreArray;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
void load_from_json(Configuration&, Padded_String_View json);
void load_from_json(Configuration&, String8_View json);

TEST(Test_Configuration, browser_globals_are_present_by_default) {
  Configuration c;

  constexpr String8_View global_variables[] = {
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
  for (String8_View variable_name : global_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    std::optional<Global_Declared_Variable> var =
        c.globals().find_runtime_or_type(variable_name);
    ASSERT_TRUE(var.has_value());
  }
}

TEST(Test_Configuration, ecmascript_globals_are_present_by_default) {
  Configuration c;

  constexpr String8_View writable_global_variables[] = {
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
  for (String8_View variable_name : writable_global_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    std::optional<Global_Declared_Variable> var =
        c.globals().find_runtime_or_type(variable_name);
    ASSERT_TRUE(var.has_value());
    EXPECT_TRUE(var->is_writable);
    EXPECT_TRUE(var->is_shadowable);
  }

  constexpr String8_View non_writable_global_variables[] = {
      // ECMA-262 18.1 Value Properties of the Global Object
      u8"Infinity"_sv,
      u8"NaN"_sv,
      u8"undefined"_sv,
  };
  for (String8_View variable_name : non_writable_global_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    std::optional<Global_Declared_Variable> var =
        c.globals().find_runtime_or_type(variable_name);
    ASSERT_TRUE(var.has_value());
    EXPECT_FALSE(var->is_writable);
    EXPECT_TRUE(var->is_shadowable);
  }
}

TEST(Test_Configuration, node_js_globals_are_present_by_default) {
  Configuration c;

  constexpr String8_View writable_commonjs_variables[] = {
      u8"__dirname"_sv, u8"__filename"_sv, u8"exports"_sv,
      u8"module"_sv,    u8"require"_sv,
  };
  for (String8_View variable_name : writable_commonjs_variables) {
    SCOPED_TRACE(out_string8(variable_name));
    std::optional<Global_Declared_Variable> var =
        c.globals().find_runtime_or_type(variable_name);
    ASSERT_TRUE(var.has_value());
    EXPECT_TRUE(var->is_writable);
  }
}

TEST(Test_Configuration, default_globals_are_all_shadowable) {
  Configuration c;
  for (String8_View variable_name : c.globals().get_all_variable_names()) {
    SCOPED_TRACE(out_string8(variable_name));
    std::optional<Global_Declared_Variable> var =
        c.globals().find_runtime_or_type(variable_name);
    ASSERT_TRUE(var.has_value());
    EXPECT_TRUE(var->is_shadowable);
  }
}

TEST(Test_Configuration, missing_global_variable_does_not_exist_by_default) {
  Configuration c;
  ASSERT_FALSE(c.globals().find_runtime_or_type(u8"variableDoesNotExist"_sv));
}

TEST(Test_Configuration, add_new_global_variable) {
  Configuration c;

  c.add_global_variable(Global_Declared_Variable{
      .name = u8"myGlobalVariable"_sv,
      .is_writable = true,
      .is_shadowable = true,
      .is_type_only = false,
  });

  std::optional<Global_Declared_Variable> found_var =
      c.globals().find_runtime_or_type(u8"myGlobalVariable"_sv);
  EXPECT_TRUE(found_var.has_value());
  EXPECT_EQ(found_var->name, u8"myGlobalVariable"_sv);
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(Test_Configuration, added_global_variable_shadows_default) {
  {
    Configuration c;
    ASSERT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv).has_value())
        << "'Array' should be declared by default";
  }

  Configuration c;

  c.add_global_variable(Global_Declared_Variable{
      .name = u8"Array"_sv,
      .is_writable = false,
      .is_shadowable = false,
      .is_type_only = false,
  });

  std::optional<Global_Declared_Variable> found_var =
      c.globals().find_runtime_or_type(u8"Array"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_FALSE(found_var->is_shadowable);
  EXPECT_FALSE(found_var->is_writable);
}

TEST(Test_Configuration,
     adding_global_variable_does_not_disable_default_groups) {
  Configuration c;

  c.add_global_variable(Global_Declared_Variable{
      .name = u8"testGlobalVariable"_sv,
      .is_writable = true,
      .is_shadowable = true,
      .is_type_only = false,
  });

  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv));
  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"console"_sv));
}

TEST(Test_Configuration, removed_global_variable_shadows_default) {
  Configuration c;
  c.remove_global_variable(u8"Array"_sv);
  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"Array"_sv));
  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"Object"_sv))
      << "defaults should still be defined";
}

TEST(Test_Configuration,
     resetting_global_groups_makes_all_variables_undefined) {
  Configuration c;
  c.reset_global_groups();
  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"Array"_sv));
  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"console"_sv));
}

TEST(Test_Configuration, add_ecmascript_group) {
  Configuration c;
  c.reset_global_groups();
  EXPECT_TRUE(c.add_global_group(u8"ecmascript"_sv));

  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv));
  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"console"_sv));
}

TEST(Test_Configuration, add_node_js_group) {
  Configuration c;
  c.reset_global_groups();
  EXPECT_TRUE(c.add_global_group(u8"node.js"_sv));

  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"Array"_sv));
  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"console"_sv));
}

TEST(Test_Configuration, add_invalid_group) {
  Configuration c;
  c.reset_global_groups();
  EXPECT_FALSE(c.add_global_group(u8"groupDoesNotExist"_sv));
}

TEST(Test_Configuration, add_ecmascript_and_node_js_groups) {
  {
    Configuration c;
    c.reset_global_groups();

    EXPECT_TRUE(c.add_global_group(u8"node.js"_sv));
    EXPECT_TRUE(c.add_global_group(u8"ecmascript"_sv));

    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv));
    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"console"_sv));
  }

  {
    Configuration c;
    c.reset_global_groups();

    EXPECT_TRUE(c.add_global_group(u8"ecmascript"_sv));
    EXPECT_TRUE(c.add_global_group(u8"node.js"_sv));

    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv));
    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"console"_sv));
  }
}

TEST(Test_Configuration, literally_anything_group_by_name) {
  Configuration c;
  c.reset_global_groups();
  EXPECT_TRUE(c.add_global_group(u8"literally-anything"_sv));

  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv));
  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"console"_sv));
  std::optional<Global_Declared_Variable> found_var =
      c.globals().find_runtime_or_type(
          u8"thisVariableWasNeverSpecifiedButStillExists"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(Test_Configuration,
     literally_anything_group_preserves_other_global_properties) {
  Configuration c;
  c.reset_global_groups();
  EXPECT_TRUE(c.add_global_group(u8"ecmascript"_sv));
  EXPECT_TRUE(c.add_global_group(u8"literally-anything"_sv));
  EXPECT_TRUE(c.add_global_group(u8"node.js"_sv));
  c.add_global_variable(Global_Declared_Variable{
      .name = u8"testGlobalVariable"_sv,
      .is_writable = false,
      .is_shadowable = false,
      .is_type_only = false,
  });
  c.add_global_variable(Global_Declared_Variable{
      .name = u8"require"_sv,
      .is_writable = true,
      .is_shadowable = false,
      .is_type_only = false,
  });

  std::optional<Global_Declared_Variable> found_var;

  found_var = c.globals().find_runtime_or_type(u8"Array"_sv);
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);

  found_var = c.globals().find_runtime_or_type(u8"require"_sv);
  EXPECT_FALSE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);

  found_var = c.globals().find_runtime_or_type(u8"Infinity"_sv);
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_FALSE(found_var->is_writable);

  found_var = c.globals().find_runtime_or_type(u8"testGlobalVariable"_sv);
  EXPECT_FALSE(found_var->is_shadowable);
  EXPECT_FALSE(found_var->is_writable);
}

TEST(Test_Configuration, overwrite_global_variable_from_group) {
  String8_View var_name = u8"Infinity"_sv;

  {
    Configuration c;
    ASSERT_TRUE(c.globals().find_runtime_or_type(var_name).has_value())
        << out_string8(var_name)
        << " should be defined in a default global group";
  }

  for (bool is_shadowable : {false, true}) {
    for (bool is_writable : {false, true}) {
      SCOPED_TRACE(is_shadowable ? "shadowable" : "not shadowable");
      SCOPED_TRACE(is_writable ? "writable" : "not writable");
      Configuration c;
      c.add_global_variable(Global_Declared_Variable{
          .name = var_name,
          .is_writable = is_writable,
          .is_shadowable = is_shadowable,
          .is_type_only = false,
      });
      std::optional<Global_Declared_Variable> var =
          c.globals().find_runtime_or_type(var_name);
      ASSERT_TRUE(var.has_value());
      EXPECT_EQ(var->is_shadowable, is_shadowable);
      EXPECT_EQ(var->is_writable, is_writable);
    }
  }
}

TEST(Test_Configuration, overwrite_global_variable) {
  String8_View var_name = u8"testvariable"_sv;

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
          Configuration c;
          c.add_global_variable(Global_Declared_Variable{
              .name = var_name,
              .is_writable = original_is_writable,
              .is_shadowable = original_is_shadowable,
              .is_type_only = false,
          });
          c.add_global_variable(Global_Declared_Variable{
              .name = var_name,
              .is_writable = override_is_writable,
              .is_shadowable = override_is_shadowable,
              .is_type_only = false,
          });
          std::optional<Global_Declared_Variable> var =
              c.globals().find_runtime_or_type(var_name);
          ASSERT_TRUE(var.has_value());
          EXPECT_EQ(var->is_shadowable, override_is_shadowable);
          EXPECT_EQ(var->is_writable, override_is_writable);
        }
      }
    }
  }
}

TEST(Test_Configuration, reset_removes_added_globals) {
  Configuration c;
  c.add_global_variable(Global_Declared_Variable{
      .name = u8"testGlobalVariable"_sv,
      .is_writable = false,
      .is_shadowable = false,
      .is_type_only = false,
  });

  c.reset();

  EXPECT_FALSE(
      c.globals().find_runtime_or_type(u8"testGlobalVariable"_sv).has_value());
}

TEST(Test_Configuration, reset_undoes_removed_global_group) {
  {
    Configuration c;
    ASSERT_TRUE(c.globals().find_runtime_or_type(u8"console"_sv).has_value())
        << "'console' should be declared by default";
  }

  Configuration c;
  c.reset_global_groups();
  c.add_global_group(u8"ecmascript");
  ASSERT_FALSE(c.globals().find_runtime_or_type(u8"console"_sv).has_value())
      << "reset_global_groups should undeclare 'console'";
  ASSERT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv).has_value());

  c.reset();

  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"console"_sv).has_value());
  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv).has_value());
}

TEST(Test_Configuration, reset_removes_literally_anything_group) {
  Configuration c;
  c.add_global_group(u8"literally-anything");
  ASSERT_TRUE(
      c.globals().find_runtime_or_type(u8"testGlobalVariable"_sv).has_value());

  c.reset();

  EXPECT_FALSE(
      c.globals().find_runtime_or_type(u8"testGlobalVariable"_sv).has_value());
}

TEST(Test_Configuration, reset_undoes_removed_global_variables) {
  {
    Configuration c;
    ASSERT_TRUE(c.globals().find_runtime_or_type(u8"console"_sv).has_value())
        << "'console' should be declared by default";
  }

  Configuration c;
  c.remove_global_variable(u8"console");

  c.reset();

  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"console"_sv).has_value());
}

TEST(Test_Configuration_JSON, empty_json_creates_default_config) {
  Configuration c;
  load_from_json(c, u8"{}"_sv);

  EXPECT_DEFAULT_CONFIG(c);
}

TEST(Test_Configuration_JSON, true_global_groups_leaves_defaults) {
  Configuration c;
  load_from_json(c, u8R"({"global-groups": true})"_sv);

  EXPECT_DEFAULT_CONFIG(c);
}

TEST(Test_Configuration_JSON, false_global_groups_disables_all_groups) {
  Configuration c;
  load_from_json(c, u8R"({"global-groups": false})"_sv);

  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"Array"_sv));
  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"console"_sv));
}

TEST(Test_Configuration_JSON, empty_global_groups_disables_all_groups) {
  Configuration c;
  load_from_json(c, u8R"({"global-groups": []})"_sv);

  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"Array"_sv));
  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"console"_sv));
}

TEST(Test_Configuration_JSON, global_groups_with_node_js_enables_only_node_js) {
  Configuration c;
  load_from_json(c, u8R"({"global-groups": ["node.js"]})"_sv);

  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"console"_sv));
  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"Array"_sv));
}

TEST(Test_Configuration_JSON, empty_globals_leaves_defaults) {
  Configuration c;
  load_from_json(c, u8R"({"globals": {}})"_sv);

  EXPECT_DEFAULT_CONFIG(c);
}

TEST(Test_Configuration_JSON, true_global_is_usable) {
  Configuration c;
  load_from_json(c, u8R"({"globals": {"myTestGlobalVariable": true}})"_sv);

  std::optional<Global_Declared_Variable> found_var =
      c.globals().find_runtime_or_type(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(Test_Configuration_JSON, empty_object_global_is_usable) {
  Configuration c;
  load_from_json(c, u8R"({"globals": {"myTestGlobalVariable": {}}})"_sv);

  std::optional<Global_Declared_Variable> found_var =
      c.globals().find_runtime_or_type(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(Test_Configuration_JSON, unwritable_global_is_not_writable) {
  Configuration c;
  load_from_json(
      c, u8R"({"globals": {"myTestGlobalVariable": {"writable": false}}})"_sv);

  std::optional<Global_Declared_Variable> found_var =
      c.globals().find_runtime_or_type(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_TRUE(found_var->is_shadowable);
  EXPECT_FALSE(found_var->is_writable);
}

TEST(Test_Configuration_JSON, unshadowable_global_is_not_shadowable) {
  Configuration c;
  load_from_json(
      c,
      u8R"({"globals": {"myTestGlobalVariable": {"shadowable": false}}})"_sv);

  std::optional<Global_Declared_Variable> found_var =
      c.globals().find_runtime_or_type(u8"myTestGlobalVariable"_sv);
  ASSERT_TRUE(found_var.has_value());
  EXPECT_FALSE(found_var->is_shadowable);
  EXPECT_TRUE(found_var->is_writable);
}

TEST(Test_Configuration_JSON, false_global_overrides_global_group) {
  Configuration c;
  load_from_json(
      c,
      u8R"({"globals": {"console": false}, "global-groups": ["ecmascript", "node.js"]})"_sv);

  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv))
      << "ecmascript group should take effect";
  EXPECT_TRUE(c.globals().find_runtime_or_type(u8"require"_sv))
      << "node.js group should take effect";
  EXPECT_FALSE(c.globals().find_runtime_or_type(u8"console"_sv))
      << "'console' from node.js group should overwritten";
}

TEST(Test_Configuration_JSON, invalid_json_reports_error) {
  Monotonic_Allocator temp_memory("test");

  // TODO(strager): The following are erroneously treated as schema
  // errors, but should be JSON parse errors:
  // u8R"({"global-groups": {42}})"_sv,
  // u8R"({"globals":{"a":{"shadowable":[}}}})"_sv,
  for (String8_View json_string : {
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
    Configuration c;

    Padded_String json(json_string);
    Diag_List diags(&temp_memory);
    c.load_from_json(&json, &diags);

    // TODO(#1154): Remove Diag_Collector and use Diag_List directly.
    Diag_Collector errors;
    errors.report(diags);
    // TODO(strager): Check Diag_Config_Json_Syntax_Error::where.
    EXPECT_THAT(errors.errors,
                ElementsAreArray({DIAG_TYPE(Diag_Config_Json_Syntax_Error)}));
  }
}

TEST(Test_Configuration_JSON, bad_schema_in_globals_reports_error) {
  Monotonic_Allocator temp_memory("test");

  {
    Padded_String json(u8R"({"globals":["myGlobalVariable"]})"_sv);
    Configuration c;
    Diag_List diags(&temp_memory);
    c.load_from_json(&json, &diags);
    // TODO(#1154): Remove Diag_Collector and use Diag_List directly.
    Diag_Collector errors;
    errors.report(diags);
    EXPECT_THAT(errors.errors,
                ElementsAreArray({DIAG_TYPE_OFFSETS(
                    &json, Diag_Config_Globals_Type_Mismatch,  //
                    value, u8R"({"globals":)"_sv.size(), u8"["_sv)}));
    EXPECT_FALSE(c.globals().find_runtime_or_type(u8"myGlobalVariable"_sv))
        << "invalid global should be ignored";
  }

  {
    Padded_String json(
        u8R"({"globals":{"testBefore":true,"testBad":"string","testAfter":true}})"_sv);
    Configuration c;
    Diag_List diags(&temp_memory);
    c.load_from_json(&json, &diags);
    // TODO(#1154): Remove Diag_Collector and use Diag_List directly.
    Diag_Collector errors;
    errors.report(diags);
    EXPECT_THAT(errors.errors,
                ElementsAreArray({DIAG_TYPE_OFFSETS(
                    &json, Diag_Config_Globals_Descriptor_Type_Mismatch,  //
                    descriptor,
                    u8R"({"globals":{"testBefore":true,"testBad":)"_sv.size(),
                    u8R"("string")"_sv)}));

    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"testBefore"_sv))
        << "valid globals before should work";
    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"testAfter"_sv))
        << "valid globals after should work";
    EXPECT_FALSE(c.globals().find_runtime_or_type(u8"testBad"_sv))
        << "invalid global should be ignored";
  }

  {
    Padded_String json(
        u8R"({"globals":{"testBefore":true,"testBad":{"writable":false,"shadowable":"string"},"testAfter":true}})"_sv);
    Configuration c;
    Diag_List diags(&temp_memory);
    c.load_from_json(&json, &diags);
    // TODO(#1154): Remove Diag_Collector and use Diag_List directly.
    Diag_Collector errors;
    errors.report(diags);
    EXPECT_THAT(
        errors.errors,
        ElementsAreArray({DIAG_TYPE_OFFSETS(
            &json, Diag_Config_Globals_Descriptor_Shadowable_Type_Mismatch,  //
            value,
            u8R"({"globals":{"testBefore":true,"testBad":{"writable":false,"shadowable":)"_sv
                .size(),
            u8R"("string")"_sv)}));

    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"testBefore"_sv))
        << "valid globals before should work";
    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"testAfter"_sv))
        << "valid globals after should work";
    std::optional<Global_Declared_Variable> var =
        c.globals().find_runtime_or_type(u8"testBad"_sv);
    ASSERT_TRUE(var.has_value()) << "broken global should be present";
    EXPECT_FALSE(var->is_writable)
        << "valid property on broken global should work";
    EXPECT_TRUE(var->is_shadowable)
        << "invalid global property should be ignored (default)";
  }

  {
    Padded_String json(
        u8R"({"globals":{"testBefore":true,"testBad":{"writable":"string","shadowable":false},"testAfter":true}})"_sv);
    Configuration c;
    Diag_List diags(&temp_memory);
    c.load_from_json(&json, &diags);
    // TODO(#1154): Remove Diag_Collector and use Diag_List directly.
    Diag_Collector errors;
    errors.report(diags);
    EXPECT_THAT(
        errors.errors,
        ElementsAreArray({DIAG_TYPE_OFFSETS(
            &json, Diag_Config_Globals_Descriptor_Writable_Type_Mismatch,  //
            value,
            u8R"({"globals":{"testBefore":true,"testBad":{"writable":)"_sv
                .size(),
            u8R"("string")"_sv)}));

    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"testBefore"_sv))
        << "valid globals before should work";
    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"testAfter"_sv))
        << "valid globals after should work";
    std::optional<Global_Declared_Variable> var =
        c.globals().find_runtime_or_type(u8"testBad"_sv);
    ASSERT_TRUE(var.has_value()) << "broken global should be present";
    EXPECT_TRUE(var->is_writable)
        << "invalid global property should be ignored (default)";
    EXPECT_FALSE(var->is_shadowable)
        << "valid property on broken global should work";
  }
}

TEST(Test_Configuration_JSON, bad_schema_in_global_groups_reports_error) {
  Monotonic_Allocator temp_memory("test");

  {
    Padded_String json(u8R"({"global-groups":{"browser":true}})"_sv);
    Configuration c;
    Diag_List diags(&temp_memory);
    c.load_from_json(&json, &diags);
    // TODO(#1154): Remove Diag_Collector and use Diag_List directly.
    Diag_Collector errors;
    errors.report(diags);
    EXPECT_THAT(errors.errors,
                ElementsAreArray({DIAG_TYPE_OFFSETS(
                    &json, Diag_Config_Global_Groups_Type_Mismatch,  //
                    value, u8R"({"global-groups":)"_sv.size(), u8"{"_sv)}));
    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv))
        << "invalid global-groups should be ignored";
  }

  {
    Padded_String json(
        u8R"({"global-groups":["browser",false,"ecmascript"]})"_sv);
    Configuration c;
    Diag_List diags(&temp_memory);
    c.load_from_json(&json, &diags);
    // TODO(#1154): Remove Diag_Collector and use Diag_List directly.
    Diag_Collector errors;
    errors.report(diags);
    EXPECT_THAT(errors.errors,
                ElementsAreArray({DIAG_TYPE_OFFSETS(
                    &json, Diag_Config_Global_Groups_Group_Type_Mismatch,  //
                    group, u8R"({"global-groups":["browser",)"_sv.size(),
                    u8"false"_sv)}));

    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"Array"_sv))
        << "valid group-groups entries should take effect\n"
           "('Array' is from the 'ecmascript' group)";
    EXPECT_TRUE(c.globals().find_runtime_or_type(u8"document"_sv))
        << "valid group-groups entries should take effect\n"
           "('document' is from the 'browser' group)";
    EXPECT_FALSE(c.globals().find_runtime_or_type(u8"require"_sv))
        << "invalid global-groups entry should be ignored; "
           "it shouldn't cause the entire global-groups array to be ignored\n"
           "('require' is a default)";
  }
}

TEST(Test_Configuration_JSON, bad_global_error_excludes_trailing_whitespace) {
  // simdjson's raw_json_token function returns trailing whitespace by default.
  // Ensure the whitespace is not included in error messages.

  Monotonic_Allocator temp_memory("test");

  // According to RFC 8259, whitespace characters are U+0009, U+000A, U+000D,
  // and U+0020.
  Padded_String json(u8"{ \"globals\": { \"a\": \"b\"  \n\t\r }}"_sv);
  Configuration c;
  Diag_List diags(&temp_memory);
  c.load_from_json(&json, &diags);

  // TODO(#1154): Remove Diag_Collector and use Diag_List directly.
  Diag_Collector errors;
  errors.report(diags);
  EXPECT_THAT(
      errors.errors,
      ElementsAreArray({DIAG_TYPE_OFFSETS(
          &json, Diag_Config_Globals_Descriptor_Type_Mismatch,  //
          descriptor, u8R"({ "globals": { "a": )"_sv.size(), u8R"("b")"_sv)}));
}

void load_from_json(Configuration& config, Padded_String_View json) {
  Monotonic_Allocator temp_memory("test");
  Diag_List diags(&temp_memory);
  config.load_from_json(json, &diags);
  // TODO(#1154): Remove Diag_Collector and use Diag_List directly.
  Diag_Collector errors;
  errors.report(diags);
  EXPECT_THAT(errors.errors, ::testing::IsEmpty());
}

void load_from_json(Configuration& config, String8_View json) {
  Padded_String padded_json(json);
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
