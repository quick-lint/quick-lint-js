// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <string_view>

namespace quick_lint_js {
namespace {
class Test_Parse_JSX_React : public Test_Parse_Expression {};

TEST_F(Test_Parse_JSX_React, correctly_capitalized_attribute) {
  test_parse_and_visit_module(u8R"(c = <td colSpan="2" />;)"_sv, no_diags,
                              jsx_options);

  test_parse_and_visit_module(u8R"(c = <div onClick={handler} />;)"_sv,
                              no_diags, jsx_options);
}

TEST_F(Test_Parse_JSX_React, event_attributes_should_be_camel_case) {
  test_parse_and_visit_module(
      u8"c = <div onclick={handler} />;"_sv,  //
      u8"         ^^^^^^^ Diag_JSX_Event_Attribute_Should_Be_Camel_Case.attribute_name"_diag
      u8"{.expected_attribute_name=onClick}"_diag,  //
      jsx_options);

  // TODO(strager): Should we also report that the handler's value is missing?
  test_parse_and_visit_module(
      u8"c = <div onclick />;"_sv,  //
      u8"         ^^^^^^^ Diag_JSX_Event_Attribute_Should_Be_Camel_Case.attribute_name"_diag
      u8"{.expected_attribute_name=onClick}"_diag,  //
      jsx_options);

  test_parse_and_visit_module(
      u8"c = <div onmouseenter={handler} />;"_sv,  //
      u8"         ^^^^^^^^^^^^ Diag_JSX_Event_Attribute_Should_Be_Camel_Case.attribute_name"_diag
      u8"{.expected_attribute_name=onMouseEnter}"_diag,  //
      jsx_options);

  test_parse_and_visit_module(
      u8"c = <div oncustomevent={handler} />;"_sv,  //
      u8"         ^^^^^^^^^^^^^ Diag_JSX_Event_Attribute_Should_Be_Camel_Case.attribute_name"_diag
      u8"{.expected_attribute_name=onCustomevent}"_diag,  //
      jsx_options);
}

TEST_F(Test_Parse_JSX_React, miscapitalized_attribute) {
  test_parse_and_visit_module(
      u8"c = <td colspan=\"2\" />;"_sv,  //
      u8"        ^^^^^^^ Diag_JSX_Attribute_Has_Wrong_Capitalization.attribute_name"_diag
      u8"{.expected_attribute_name=colSpan}"_diag,  //
      jsx_options);

  test_parse_and_visit_module(
      u8"c = <div onMouseenter={handler} />;"_sv,  //
      u8"         ^^^^^^^^^^^^ Diag_JSX_Attribute_Has_Wrong_Capitalization.attribute_name"_diag
      u8"{.expected_attribute_name=onMouseEnter}"_diag,  //
      jsx_options);

  test_parse_and_visit_module(
      u8"c = <div onmouseENTER={handler} />;"_sv,  //
      u8"         ^^^^^^^^^^^^ Diag_JSX_Attribute_Has_Wrong_Capitalization.attribute_name"_diag
      u8"{.expected_attribute_name=onMouseEnter}"_diag,  //
      jsx_options);
}

TEST_F(Test_Parse_JSX_React, commonly_misspelled_attribute) {
  test_parse_and_visit_module(
      u8"c = <span class=\"item\"></span>;"_sv,  //
      u8"          ^^^^^ Diag_JSX_Attribute_Renamed_By_React.attribute_name"_diag
      u8"{.react_attribute_name=className}"_diag,  //
      jsx_options);
}

TEST_F(Test_Parse_JSX_React, attribute_checking_ignores_namespaced_attributes) {
  test_parse_and_visit_module(u8R"(c = <div ns:onmouseenter={handler} />;)"_sv,
                              no_diags, jsx_options);
  test_parse_and_visit_module(
      u8R"(c = <div onmouseenter:onmouseenter={handler} />;)"_sv, no_diags,
      jsx_options);
  test_parse_and_visit_module(u8R"(c = <div class:class="my-css-class" />;)"_sv,
                              no_diags, jsx_options);
}

TEST_F(Test_Parse_JSX_React, attribute_checking_ignores_namespaced_elements) {
  test_parse_and_visit_module(u8R"(c = <svg:g onmouseenter={handler} />;)"_sv,
                              no_diags, jsx_options);
  test_parse_and_visit_module(u8R"(c = <svg:g class="red" />;)"_sv, no_diags,
                              jsx_options);
}

TEST_F(Test_Parse_JSX_React, attribute_checking_ignores_user_components) {
  test_parse_and_visit_module(
      u8R"(c = <MyComponent onmouseenter={handler} />;)"_sv, no_diags,
      jsx_options);

  test_parse_and_visit_module(u8R"(c = <MyComponent class="red" />;)"_sv,
                              no_diags, jsx_options);

  test_parse_and_visit_module(
      u8R"(c = <mymodule.mycomponent onmouseenter={handler} />;)"_sv, no_diags,
      jsx_options);

  test_parse_and_visit_module(
      u8R"(c = <mymodule.mycomponent class="red" />;)"_sv, no_diags,
      jsx_options);
}
}
}
