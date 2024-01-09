// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <string_view>

namespace quick_lint_js {
namespace {
class Test_Parse_JSX_React : public Test_Parse_Expression {};

constexpr Parser_Options jsx_react_options{
    .jsx_mode = Parser_JSX_Mode::react,
    .jsx = true,
};

constexpr Parser_Options jsx_auto_detect_options{
    .jsx_mode = Parser_JSX_Mode::auto_detect,
    .jsx = true,
};

constexpr Parser_Options jsx_none_options{
    .jsx_mode = Parser_JSX_Mode::none,
    .jsx = true,
};

TEST_F(Test_Parse_JSX_React, correctly_capitalized_attribute) {
  test_parse_and_visit_module(u8R"(c = <td colSpan="2" />;)"_sv, no_diags,
                              jsx_react_options);

  test_parse_and_visit_module(u8R"(c = <div onClick={handler} />;)"_sv,
                              no_diags, jsx_react_options);
}

TEST_F(Test_Parse_JSX_React, event_attributes_should_be_camel_case) {
  test_parse_and_visit_module(
      u8"c = <div onclick={handler} />;"_sv,  //
      u8"         ^^^^^^^ Diag_JSX_Event_Attribute_Should_Be_Camel_Case.attribute_name"_diag
      u8"{.expected_attribute_name=onClick}"_diag,  //
      jsx_react_options);

  // TODO(strager): Should we also report that the handler's value is missing?
  test_parse_and_visit_module(
      u8"c = <div onclick />;"_sv,  //
      u8"         ^^^^^^^ Diag_JSX_Event_Attribute_Should_Be_Camel_Case.attribute_name"_diag
      u8"{.expected_attribute_name=onClick}"_diag,  //
      jsx_react_options);

  test_parse_and_visit_module(
      u8"c = <div onmouseenter={handler} />;"_sv,  //
      u8"         ^^^^^^^^^^^^ Diag_JSX_Event_Attribute_Should_Be_Camel_Case.attribute_name"_diag
      u8"{.expected_attribute_name=onMouseEnter}"_diag,  //
      jsx_react_options);

  test_parse_and_visit_module(
      u8"c = <div oncustomevent={handler} />;"_sv,  //
      u8"         ^^^^^^^^^^^^^ Diag_JSX_Event_Attribute_Should_Be_Camel_Case.attribute_name"_diag
      u8"{.expected_attribute_name=onCustomevent}"_diag,  //
      jsx_react_options);
}

TEST_F(Test_Parse_JSX_React, miscapitalized_attribute) {
  test_parse_and_visit_module(
      u8"c = <td colspan=\"2\" />;"_sv,  //
      u8"        ^^^^^^^ Diag_JSX_Attribute_Has_Wrong_Capitalization.attribute_name"_diag
      u8"{.expected_attribute_name=colSpan}"_diag,  //
      jsx_react_options);

  test_parse_and_visit_module(
      u8"c = <div onMouseenter={handler} />;"_sv,  //
      u8"         ^^^^^^^^^^^^ Diag_JSX_Attribute_Has_Wrong_Capitalization.attribute_name"_diag
      u8"{.expected_attribute_name=onMouseEnter}"_diag,  //
      jsx_react_options);

  test_parse_and_visit_module(
      u8"c = <div onmouseENTER={handler} />;"_sv,  //
      u8"         ^^^^^^^^^^^^ Diag_JSX_Attribute_Has_Wrong_Capitalization.attribute_name"_diag
      u8"{.expected_attribute_name=onMouseEnter}"_diag,  //
      jsx_react_options);
}

TEST_F(Test_Parse_JSX_React, commonly_misspelled_attribute) {
  test_parse_and_visit_module(
      u8"c = <span class=\"item\"></span>;"_sv,  //
      u8"          ^^^^^ Diag_JSX_Attribute_Renamed_By_React.attribute_name"_diag
      u8"{.react_attribute_name=className}"_diag,  //
      jsx_react_options);
}

TEST_F(Test_Parse_JSX_React, attribute_checking_ignores_namespaced_attributes) {
  test_parse_and_visit_module(u8R"(c = <div ns:onmouseenter={handler} />;)"_sv,
                              no_diags, jsx_react_options);
  test_parse_and_visit_module(
      u8R"(c = <div onmouseenter:onmouseenter={handler} />;)"_sv, no_diags,
      jsx_react_options);
  test_parse_and_visit_module(u8R"(c = <div class:class="my-css-class" />;)"_sv,
                              no_diags, jsx_react_options);
}

TEST_F(Test_Parse_JSX_React, attribute_checking_ignores_namespaced_elements) {
  test_parse_and_visit_module(u8R"(c = <svg:g onmouseenter={handler} />;)"_sv,
                              no_diags, jsx_react_options);
  test_parse_and_visit_module(u8R"(c = <svg:g class="red" />;)"_sv, no_diags,
                              jsx_react_options);
}

TEST_F(Test_Parse_JSX_React, attribute_checking_ignores_user_components) {
  test_parse_and_visit_module(
      u8R"(c = <MyComponent onmouseenter={handler} />;)"_sv, no_diags,
      jsx_react_options);

  test_parse_and_visit_module(u8R"(c = <MyComponent class="red" />;)"_sv,
                              no_diags, jsx_react_options);

  test_parse_and_visit_module(
      u8R"(c = <mymodule.mycomponent onmouseenter={handler} />;)"_sv, no_diags,
      jsx_react_options);

  test_parse_and_visit_module(
      u8R"(c = <mymodule.mycomponent class="red" />;)"_sv, no_diags,
      jsx_react_options);
}

TEST_F(Test_Parse_JSX_React, no_diagnostic_if_not_react_mode) {
  test_parse_and_visit_module(u8"c = <div onclick={handler} class=\"c\" />;"_sv,
                              no_diags, jsx_none_options);
}

TEST_F(Test_Parse_JSX_React,
       no_diagnostic_if_auto_detect_and_react_not_imported) {
  test_parse_and_visit_module(u8"c = <div onclick={handler} class=\"c\" />;"_sv,
                              no_diags, jsx_auto_detect_options);

  for (String8_View import_code : {
           // Non-React modules:
           u8"import React from 'reactive-banana';"_sv,
           u8"import ReactRouter from 'react-router';"_sv,
       }) {
    test_parse_and_visit_module(
        concat(import_code, u8" c = <div onclick={handler} class=\"c\" />;"_sv),
        no_diags, jsx_auto_detect_options);
  }
}

TEST_F(Test_Parse_JSX_React, diagnostic_if_auto_detect_and_react_is_imported) {
  for (String8_View import_code : {
           u8"import React from 'react';"_sv,
           u8"import React from \"react\";"_sv,
           u8"import React from 'react-dom';"_sv,
           u8"import React from 'react-dom/client';"_sv,
           u8"import React from 'react-dom/server';"_sv,
           u8"import someVariable from 'react';"_sv,
           u8"import {someExport} from 'react';"_sv, u8"import 'react';"_sv,
           // TODO(#1159): u8"import React from '\\u{72}eact';"_sv,
           // TODO(strager): u8"const React = require('react');"
       }) {
    test_parse_and_visit_module(
        concat(import_code, u8" c = <div onclick={handler} class=\"c\" />;"_sv),
        u8"Diag_JSX_Event_Attribute_Should_Be_Camel_Case"_diag,
        u8"Diag_JSX_Attribute_Renamed_By_React"_diag, jsx_auto_detect_options);
  }

  test_parse_and_visit_module(
      u8"import React = require('react'); c = <div onclick={handler} class=\"c\" />;"_sv,
      u8"Diag_JSX_Event_Attribute_Should_Be_Camel_Case"_diag,
      u8"Diag_JSX_Attribute_Renamed_By_React"_diag,
      Parser_Options{
          .jsx_mode = Parser_JSX_Mode::auto_detect,
          .jsx = true,
          .typescript = true,
      });

  // import after the attribute should work too:
  test_parse_and_visit_module(
      u8"c = <div onclick={handler} class=\"c\" />; import React from 'react';"_sv,
      u8"Diag_JSX_Event_Attribute_Should_Be_Camel_Case"_diag,
      u8"Diag_JSX_Attribute_Renamed_By_React"_diag, jsx_auto_detect_options);
}

TEST_F(Test_Parse_JSX_React,
       no_diagnostic_if_auto_detect_and_preact_is_imported) {
  // Preact is incompatible with React.
  test_parse_and_visit_module(
      u8"import React from 'react';"_sv
      u8"c = <div onclick={handler} class=\"c\" />;"_sv
      u8"import Preact from 'preact';"_sv,
      no_diags, jsx_auto_detect_options);
}
}
}
