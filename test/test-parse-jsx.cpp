// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class Test_Parse_JSX : public Test_Parse_Expression {};

TEST_F(Test_Parse_JSX, jsx_is_not_supported_in_vanilla_javascript) {
  Parser_Options options;
  options.jsx = false;
  Test_Parser p(
      u8"<MyComponent attr={value}><Inner>hello</Inner></MyComponent>"_sv,
      options, capture_diags);
  p.parse_and_visit_module();
  assert_diagnostics(p.code, p.errors,
                     {
                         u8"^ Diag_JSX_Not_Allowed_In_JavaScript"_diag,
                     });
  EXPECT_THAT(p.variable_uses,
              ElementsAreArray({u8"MyComponent", u8"value", u8"Inner"}));
}

TEST_F(Test_Parse_JSX, jsx_is_not_supported_in_vanilla_typescript) {
  Parser_Options options;
  options.jsx = false;
  options.typescript = true;

  {
    Test_Parser p(
        u8"<MyComponent attr={value}><Inner>hello</Inner></MyComponent>"_sv,
        options, capture_diags);
    p.parse_and_visit_module();
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_JSX_Not_Allowed_In_TypeScript"_diag,
                       });
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"MyComponent", u8"value", u8"Inner"}));
  }

  {
    Test_Parser p(u8"<><Inner>hello</Inner><Inner /></>"_sv, options,
                  capture_diags);
    p.parse_and_visit_module();
    assert_diagnostics(p.code, p.errors,
                       {
                           u8"^ Diag_JSX_Not_Allowed_In_TypeScript"_diag,
                       });
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Inner", u8"Inner"}));
  }

  // TODO(strager): Detect more cases. There is syntax overlap with generic
  // arrow functions and with type assertions.
}

TEST_F(Test_Parse_JSX, empty_intrinsic_element) {
  Spy_Visitor p = test_parse_and_visit_module(u8"c = <div></div>;"_sv, no_diags,
                                              jsx_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_assignment",  // c
                            "visit_end_of_module",
                        }));
}

TEST_F(Test_Parse_JSX, empty_user_element) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"c = <MyComponent></MyComponent>;"_sv, no_diags, jsx_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",         // MyComponent
                            "visit_variable_assignment",  // c
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"MyComponent"}));
}

TEST_F(Test_Parse_JSX, member_component) {
  Spy_Visitor p = test_parse_and_visit_module(
      u8"c = <module.submodule.MyComponent></module.submodule.MyComponent>;"_sv,
      no_diags, jsx_options);
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",         // module
                            "visit_variable_assignment",  // c
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"module"}));
}

TEST_F(Test_Parse_JSX, element_child_element) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <outer><INNER></INNER></outer>;"_sv, no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <OUTER><INNER></INNER></OUTER>;"_sv, no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OUTER", u8"INNER"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <NS:OUTER><INNER></INNER></NS:OUTER>;"_sv, no_diags,
        jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <outer.Component><INNER></INNER></outer.Component>;"_sv,
        no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"outer", u8"INNER"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <><INNER></INNER></>;"_sv, no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
  }
}

TEST_F(Test_Parse_JSX, element_child_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <outer>{INNER}</outer>;"_sv, no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <OUTER>{INNER}</OUTER>;"_sv, no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OUTER", u8"INNER"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <NS:OUTER>{INNER}</NS:OUTER>;"_sv, no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <outer.Component>{INNER}</outer.Component>;"_sv, no_diags,
        jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"outer", u8"INNER"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(u8"c = <>{INNER}</>;"_sv,
                                                no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
  }
}

TEST_F(Test_Parse_JSX, element_attribute_expression) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <outer attr={attrValue}></outer>;"_sv, no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"attrValue"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <OUTER attr={attrValue}></OUTER>;"_sv, no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OUTER", u8"attrValue"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <NS:OUTER attr={attrValue}></NS:OUTER>;"_sv, no_diags,
        jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"attrValue"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <outer.Component attr={attrValue}></outer.Component>;"_sv,
        no_diags, jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"outer", u8"attrValue"}));
  }
}

TEST_F(Test_Parse_JSX, attribute_without_name_must_be_spread) {
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <div {attr} />;"_sv,                                   //
        u8"          ` Diag_Missing_Dots_For_Attribute_Spread"_diag,  //
        jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"attr"}));
  }
}

TEST_F(Test_Parse_JSX, begin_and_end_tags_must_match) {
  test_parse_and_visit_module(
      u8"c = <div></span>;"_sv,  //
      u8"     ^^^ Diag_Mismatched_JSX_Tags.opening_tag_name\n"_diag
      u8"           ^^^^ .closing_tag_name"_diag,  //
      jsx_options);

  // opening_tag_name span for normal tag:
  test_parse_and_visit_module(
      u8"c = < div ></span>;"_sv,                                    //
      u8"      ^^^ Diag_Mismatched_JSX_Tags.opening_tag_name"_diag,  //
      jsx_options);

  // opening_tag_name span for fragment tag:
  test_parse_and_visit_module(
      u8"c = <  ></span>;"_sv,                                    //
      u8"     ` Diag_Mismatched_JSX_Tags.opening_tag_name"_diag,  //
      jsx_options);

  // opening_tag_name span for member tag:
  test_parse_and_visit_module(
      u8"c = < module . Component ></span>;"_sv,  //
      u8"      ^^^^^^^^^^^^^^^^^^ Diag_Mismatched_JSX_Tags.opening_tag_name"_diag,  //
      jsx_options);

  // opening_tag_name span for namespaced tag:
  test_parse_and_visit_module(
      u8"c = < svg : path ></span>;"_sv,                                    //
      u8"      ^^^^^^^^^^ Diag_Mismatched_JSX_Tags.opening_tag_name"_diag,  //
      jsx_options);

  // closing_tag_name span for normal tag:
  test_parse_and_visit_module(
      u8"c = <div></ span >;"_sv,                                           //
      u8"            ^^^^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag,  //
      jsx_options);

  // closing_tag_name span for fragment tag:
  test_parse_and_visit_module(
      u8"c = <div></  >;"_sv,                                             //
      u8"             ` Diag_Mismatched_JSX_Tags.closing_tag_name"_diag,  //
      jsx_options);

  // closing_tag_name span for member tag:
  test_parse_and_visit_module(
      u8"c = <div></ module . Component >;"_sv,  //
      u8"            ^^^^^^^^^^^^^^^^^^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag,  //
      jsx_options);

  // closing_tag_name span for namespaced tag:
  test_parse_and_visit_module(
      u8"c = <div></ svg : path >;"_sv,  //
      u8"            ^^^^^^^^^^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag,  //
      jsx_options);

  // opening_tag_name_pretty for normal tag:
  test_parse_and_visit_module(
      u8"c = <div></span>;"_sv,  //
      u8"           ^^^^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag
      u8"{.opening_tag_name_pretty=div}"_diag,  //
      jsx_options);

  // opening_tag_name_pretty for fragment tag:
  test_parse_and_visit_module(
      u8"c = <  ></span>;"_sv,  //
      u8"          ^^^^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag
      u8"{.opening_tag_name_pretty=}"_diag,  //
      jsx_options);

  // opening_tag_name_pretty for member tag:
  test_parse_and_visit_module(
      u8"c = <module.Component></span>;"_sv,  //
      u8"                        ^^^^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag
      u8"{.opening_tag_name_pretty=module.Component}"_diag,  //
      jsx_options);

  // opening_tag_name_pretty for namespaced tag:
  test_parse_and_visit_module(
      u8"c = <svg:path></span>;"_sv,  //
      u8"                ^^^^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag
      u8"{.opening_tag_name_pretty=svg:path}"_diag,  //
      jsx_options);

  for (String8_View jsx : {
           u8"<div></span>"_sv,
           u8"<></span>"_sv,
           u8"<div></>"_sv,

           u8"<svg:g></svg:path>"_sv,
           u8"<svg:g></png:g>"_sv,
           u8"<svg:g></png:path>"_sv,

           u8"<svg></svg:path>"_sv,
           u8"<svg:path></svg>"_sv,
           u8"<path></svg:path>"_sv,
           u8"<svg:path></path>"_sv,

           u8"<svg:path></>"_sv,
           u8"<></svg:path>"_sv,

           u8"<module.Link></module.Route>"_sv,
           u8"<module.Link></mod.Link>"_sv,
           u8"<Link></module.Link>"_sv,
           u8"<module.Link></Link>"_sv,
           u8"<Module></Module.Link>"_sv,
           u8"<Module.Link></Module>"_sv,

           u8"<module.Link></>"_sv,
           u8"<></module.Link>"_sv,

           u8"<module.submodule.Link></module.submodule.Route>"_sv,
           u8"<module.submodule.Link></submodule.module.Link>"_sv,

           u8"<module.submodule.Link></module.Link>"_sv,
           u8"<module.Link></module.submodule.Link>"_sv,

           u8"<a:a></a.a>"_sv,
           u8"<a.a></a:a>"_sv,
           u8"<A></A.A>"_sv,
           u8"<A.A></A>"_sv,
           u8"<A></A:A>"_sv,
           u8"<A:A></A>"_sv,
       }) {
    test_parse_and_visit_module(concat(u8"c = "_sv, jsx, u8";"_sv),
                                u8"Diag_Mismatched_JSX_Tags"_diag, jsx_options);
  }
}

TEST_F(Test_Parse_JSX,
       begin_and_end_tag_mismatch_message_excludes_comments_and_whitespace) {
  test_parse_and_visit_module(
      u8"c = < div ></x>;"_sv,  //
      u8"             ^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag
      u8"{.opening_tag_name_pretty=div}"_diag,  //
      jsx_options);

  test_parse_and_visit_module(
      u8"c = < my . /* hello */ Component ></x>;"_sv,  //
      u8"                                    ^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag
      u8"{.opening_tag_name_pretty=my.Component}"_diag,  //
      jsx_options);

  test_parse_and_visit_module(
      u8"c = < svg /* */ : path ></x>;"_sv,  //
      u8"                          ^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag
      u8"{.opening_tag_name_pretty=svg:path}"_diag,  //
      jsx_options);
}

TEST_F(Test_Parse_JSX,
       begin_and_end_tag_mismatch_message_include_unicode_escapes) {
  test_parse_and_visit_module(
      u8"c = <d\\u{69}v></x>;"_sv,  //
      u8"                 ^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag
      u8"{.opening_tag_name_pretty=d\\u{69}v}"_diag,  //
      jsx_options);

  test_parse_and_visit_module(
      u8"c = <s\\u{76}g:p\\u{69}th></x>;"_sv,  //
      u8"                            ^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag
      u8"{.opening_tag_name_pretty=s\\u{76}g:p\\u{69}th}"_diag,  //
      jsx_options);

  test_parse_and_visit_module(
      u8"c = <m\\u{79}.Com\\u{70}onent></x>;"_sv,  //
      u8"                                ^ Diag_Mismatched_JSX_Tags.closing_tag_name"_diag
      u8"{.opening_tag_name_pretty=m\\u{79}.Com\\u{70}onent}"_diag,  //
      jsx_options);
}

TEST_F(Test_Parse_JSX, begin_and_end_tags_match_after_normalization) {
  // Shouldn't report Diag_Mismatched_JSX_Tags.
  test_parse_and_visit_module(u8R"(c = <div></\u{64}\u{69}\u{76}>;)"_sv,
                              no_diags, jsx_options);
}

TEST_F(Test_Parse_JSX, adjacent_tags_without_outer_fragment) {
  test_parse_and_visit_module(
      u8"c = <div></div> <div></div>;"_sv,  //
      u8"    ` Diag_Adjacent_JSX_Without_Parent.begin\n"_diag
      u8"                ` .begin_of_second_element\n"_diag
      u8"                           ` .end"_diag,  //
      jsx_options);

  test_parse_and_visit_module(
      u8"c = <div></div> <div></div> <div></div>;"_sv,  //
      u8"    ` Diag_Adjacent_JSX_Without_Parent.begin\n"_diag
      u8"                ` .begin_of_second_element\n"_diag
      u8"                                       ` .end"_diag,  //
      jsx_options);

  // Second element should be visited like normal.
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8R"(c = <FirstComponent></FirstComponent> <SecondComponent>{child}</SecondComponent>;)"_sv,
        u8"Diag_Adjacent_JSX_Without_Parent"_diag, jsx_options);
    EXPECT_THAT(
        p.variable_uses,
        ElementsAreArray({u8"FirstComponent", u8"SecondComponent", u8"child"}));
  }

  // Because the second element is on its own line, ASI should kick in, and the
  // following example is syntactically valid. However, Babel, Espree, Flow, and
  // TypeScript all agree that ASI does not kick in. Therefore, we report a
  // syntax error, despite what the specification says.
  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <FirstComponent></FirstComponent>\n<SecondComponent></SecondComponent>;"_sv,  //
        u8"Diag_Adjacent_JSX_Without_Parent"_diag,  //
        jsx_options);
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"FirstComponent", u8"SecondComponent"}));
  }

  // The following code looks like adjacent JSX elements, but it's actually a
  // JSX element followed by some legal operators. However, Babel, Espree, Flow,
  // and TypeScript all agree that '<' is not allowed after a JSX element.
  // Therefore, we report a syntax error, despite what the specification says.
  // https://github.com/facebook/jsx/issues/120
  {
    //                  binary operators  v v           v (according to spec)
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <div></div> <i>/{child}</i>\ndone"_sv,  //
        u8"Diag_Adjacent_JSX_Without_Parent"_diag,     //
        jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"child", u8"done"}));
  }

  {
    Spy_Visitor p = test_parse_and_visit_module(
        u8"c = <First></First><Second attr='value'></Second>;"_sv,  //
        u8"Diag_Adjacent_JSX_Without_Parent"_diag,                  //
        jsx_options);
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"First", u8"Second"}));
  }
}

TEST_F(Test_Parse_JSX, correctly_capitalized_attribute) {
  test_parse_and_visit_module(u8R"(c = <td colSpan="2" />;)"_sv, no_diags,
                              jsx_options);

  test_parse_and_visit_module(u8R"(c = <div onClick={handler} />;)"_sv,
                              no_diags, jsx_options);
}

TEST_F(Test_Parse_JSX, event_attributes_should_be_camel_case) {
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

TEST_F(Test_Parse_JSX, miscapitalized_attribute) {
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

TEST_F(Test_Parse_JSX, commonly_misspelled_attribute) {
  test_parse_and_visit_module(
      u8"c = <span class=\"item\"></span>;"_sv,  //
      u8"          ^^^^^ Diag_JSX_Attribute_Renamed_By_React.attribute_name"_diag
      u8"{.react_attribute_name=className}"_diag,  //
      jsx_options);
}

TEST_F(Test_Parse_JSX, attribute_checking_ignores_namespaced_attributes) {
  test_parse_and_visit_module(u8R"(c = <div ns:onmouseenter={handler} />;)"_sv,
                              no_diags, jsx_options);
  test_parse_and_visit_module(
      u8R"(c = <div onmouseenter:onmouseenter={handler} />;)"_sv, no_diags,
      jsx_options);
  test_parse_and_visit_module(u8R"(c = <div class:class="my-css-class" />;)"_sv,
                              no_diags, jsx_options);
}

TEST_F(Test_Parse_JSX, attribute_checking_ignores_namespaced_elements) {
  test_parse_and_visit_module(u8R"(c = <svg:g onmouseenter={handler} />;)"_sv,
                              no_diags, jsx_options);
  test_parse_and_visit_module(u8R"(c = <svg:g class="red" />;)"_sv, no_diags,
                              jsx_options);
}

TEST_F(Test_Parse_JSX, attribute_checking_ignores_user_components) {
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

TEST_F(Test_Parse_JSX, prop_needs_an_expression) {
  test_parse_and_visit_module(
      u8"c = <MyComponent custom={}></MyComponent>;"_sv,  //
      u8"                        ^^ Diag_JSX_Prop_Is_Missing_Expression"_diag,  //
      jsx_options);

  test_parse_and_visit_module(
      u8"c = <MyComponent custom={ }></MyComponent>;"_sv,  //
      u8"                        ^^^ Diag_JSX_Prop_Is_Missing_Expression"_diag,  //
      jsx_options);
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
