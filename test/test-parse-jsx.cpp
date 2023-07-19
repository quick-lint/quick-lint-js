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

using ::testing::Contains;
using ::testing::ElementsAre;
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
  EXPECT_THAT(
      p.errors,
      ElementsAreArray({
          DIAG_TYPE_OFFSETS(p.code, Diag_JSX_Not_Allowed_In_JavaScript,  //
                            jsx_start, 0, u8"<"_sv),
      }));
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
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_JSX_Not_Allowed_In_TypeScript,  //
                              jsx_start, 0, u8"<"_sv),
        }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"MyComponent", u8"value", u8"Inner"}));
  }

  {
    Test_Parser p(u8"<><Inner>hello</Inner><Inner /></>"_sv, options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_JSX_Not_Allowed_In_TypeScript,  //
                              jsx_start, 0, u8"<"_sv),
        }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Inner", u8"Inner"}));
  }

  // TODO(strager): Detect more cases. There is syntax overlap with generic
  // arrow functions and with type assertions.
}

TEST_F(Test_Parse_JSX, empty_intrinsic_element) {
  Test_Parser p(u8"c = <div></div>;"_sv, jsx_options, capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_assignment",  // c
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(Test_Parse_JSX, empty_user_element) {
  Test_Parser p(u8"c = <MyComponent></MyComponent>;"_sv, jsx_options,
                capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",         // MyComponent
                            "visit_variable_assignment",  // c
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"MyComponent"}));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(Test_Parse_JSX, member_component) {
  Test_Parser p(
      u8"c = <module.submodule.MyComponent></module.submodule.MyComponent>;"_sv,
      jsx_options, capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_use",         // module
                            "visit_variable_assignment",  // c
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"module"}));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(Test_Parse_JSX, element_child_element) {
  {
    Test_Parser p(u8"c = <outer><INNER></INNER></outer>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"c = <OUTER><INNER></INNER></OUTER>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OUTER", u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"c = <NS:OUTER><INNER></INNER></NS:OUTER>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(
        u8"c = <outer.Component><INNER></INNER></outer.Component>;"_sv,
        jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"outer", u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"c = <><INNER></INNER></>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(Test_Parse_JSX, element_child_expression) {
  {
    Test_Parser p(u8"c = <outer>{INNER}</outer>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"c = <OUTER>{INNER}</OUTER>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OUTER", u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"c = <NS:OUTER>{INNER}</NS:OUTER>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"c = <outer.Component>{INNER}</outer.Component>;"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"outer", u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"c = <>{INNER}</>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(Test_Parse_JSX, element_attribute_expression) {
  {
    Test_Parser p(u8"c = <outer attr={attrValue}></outer>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"attrValue"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"c = <OUTER attr={attrValue}></OUTER>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OUTER", u8"attrValue"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8"c = <NS:OUTER attr={attrValue}></NS:OUTER>;"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"attrValue"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(
        u8"c = <outer.Component attr={attrValue}></outer.Component>;"_sv,
        jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"outer", u8"attrValue"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(Test_Parse_JSX, attribute_without_name_must_be_spread) {
  {
    Test_Parser p(u8"c = <div {attr} />;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"attr"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, Diag_Missing_Dots_For_Attribute_Spread,  //
                        expected_dots, u8"c = <div {"_sv.size(), u8""_sv),
                }));
  }
}

TEST_F(Test_Parse_JSX, begin_and_end_tags_must_match) {
  {
    Test_Parser p(u8"c = <div></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(p.code, Diag_Mismatched_JSX_Tags,  //
                                        opening_tag_name, u8"c = <"_sv.size(),
                                        u8"div"_sv, closing_tag_name,
                                        u8"c = <div></"_sv.size(), u8"span"_sv),
                }));
  }

  // opening_tag_name span for normal tag:
  {
    Test_Parser p(u8"c = < div ></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, Diag_Mismatched_JSX_Tags,  //
                                      opening_tag_name, u8"c = < "_sv.size(),
                                      u8"div"_sv),
                }));
  }

  // opening_tag_name span for fragment tag:
  {
    Test_Parser p(u8"c = <  ></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Mismatched_JSX_Tags,  //
                              opening_tag_name, u8"c = <"_sv.size(), u8""_sv),
        }));
  }

  // opening_tag_name span for member tag:
  {
    Test_Parser p(u8"c = < module . Component ></span>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, Diag_Mismatched_JSX_Tags,  //
                                      opening_tag_name, u8"c = < "_sv.size(),
                                      u8"module . Component"_sv),
                }));
  }

  // opening_tag_name span for namespaced tag:
  {
    Test_Parser p(u8"c = < svg : path ></span>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, Diag_Mismatched_JSX_Tags,  //
                                      opening_tag_name, u8"c = < "_sv.size(),
                                      u8"svg : path"_sv),
                }));
  }

  // closing_tag_name span for normal tag:
  {
    Test_Parser p(u8"c = <div></ span >;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, Diag_Mismatched_JSX_Tags,  //
                                      closing_tag_name,
                                      u8"c = <div></ "_sv.size(), u8"span"_sv),
                }));
  }

  // closing_tag_name span for fragment tag:
  {
    Test_Parser p(u8"c = <div></  >;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, Diag_Mismatched_JSX_Tags,  //
                                      closing_tag_name,
                                      u8"c = <div></  "_sv.size(), u8""_sv),
                }));
  }

  // closing_tag_name span for member tag:
  {
    Test_Parser p(u8"c = <div></ module . Component >;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Mismatched_JSX_Tags,  //
                              closing_tag_name, u8"c = <div></ "_sv.size(),
                              u8"module . Component"_sv),
        }));
  }

  // closing_tag_name span for namespaced tag:
  {
    Test_Parser p(u8"c = <div></ svg : path >;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, Diag_Mismatched_JSX_Tags,  //
                              closing_tag_name, u8"c = <div></ "_sv.size(),
                              u8"svg : path"_sv),
        }));
  }

  // opening_tag_name_pretty for normal tag:
  {
    Test_Parser p(u8"c = <div></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_FIELD(Diag_Mismatched_JSX_Tags,
                                    opening_tag_name_pretty, u8"div"_sv),
                }));
  }

  // opening_tag_name_pretty for fragment tag:
  {
    Test_Parser p(u8"c = <  ></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(Diag_Mismatched_JSX_Tags,
                                              opening_tag_name_pretty, u8""_sv),
                          }));
  }

  // opening_tag_name_pretty for member tag:
  {
    Test_Parser p(u8"c = <module.Component></span>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(Diag_Mismatched_JSX_Tags,
                                              opening_tag_name_pretty,
                                              u8"module.Component"_sv),
                          }));
  }

  // opening_tag_name_pretty for namespaced tag:
  {
    Test_Parser p(u8"c = <svg:path></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_FIELD(Diag_Mismatched_JSX_Tags,
                                    opening_tag_name_pretty, u8"svg:path"_sv),
                }));
  }

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
    Test_Parser p(concat(u8"c = "_sv, jsx, u8";"_sv), jsx_options,
                  capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Mismatched_JSX_Tags),
                          }));
  }
}

TEST_F(Test_Parse_JSX,
       begin_and_end_tag_mismatch_message_excludes_comments_and_whitespace) {
  {
    Test_Parser p(u8"c = < div ></x>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_FIELD(Diag_Mismatched_JSX_Tags,
                                    opening_tag_name_pretty, u8"div"_sv),
                }));
  }

  {
    Test_Parser p(u8"c = < my . /* hello */ Component ></x>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(Diag_Mismatched_JSX_Tags,
                                              opening_tag_name_pretty,
                                              u8"my.Component"_sv),
                          }));
  }

  {
    Test_Parser p(u8"c = < svg /* */ : path ></x>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_FIELD(Diag_Mismatched_JSX_Tags,
                                    opening_tag_name_pretty, u8"svg:path"_sv),
                }));
  }
}

TEST_F(Test_Parse_JSX,
       begin_and_end_tag_mismatch_message_include_unicode_escapes) {
  {
    Test_Parser p(u8R"(c = <d\u{69}v></x>;)"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(Diag_Mismatched_JSX_Tags,
                                              opening_tag_name_pretty,
                                              u8R"(d\u{69}v)"_sv),
                          }));
  }

  {
    Test_Parser p(u8R"(c = <s\u{76}g:p\u{69}th></x>;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(Diag_Mismatched_JSX_Tags,
                                              opening_tag_name_pretty,
                                              u8R"(s\u{76}g:p\u{69}th)"_sv),
                          }));
  }

  {
    Test_Parser p(u8R"(c = <m\u{79}.Com\u{70}onent></x>;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(Diag_Mismatched_JSX_Tags,
                                              opening_tag_name_pretty,
                                              u8R"(m\u{79}.Com\u{70}onent)"_sv),
                          }));
  }
}

TEST_F(Test_Parse_JSX, begin_and_end_tags_match_after_normalization) {
  {
    Test_Parser p(u8R"(c = <div></\u{64}\u{69}\u{76}>;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty())
        << "shouldn't report Diag_Mismatched_JSX_Tags";
  }
}

TEST_F(Test_Parse_JSX, adjacent_tags_without_outer_fragment) {
  {
    Test_Parser p(u8R"(c = <div></div> <div></div>;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_3_OFFSETS(
                p.code, Diag_Adjacent_JSX_Without_Parent,  //
                begin, u8"c = "_sv.size(), u8""_sv,        //
                begin_of_second_element, u8"c = <div></div> "_sv.size(),
                u8""_sv,  //
                end, u8"c = <div></div> <div></div>"_sv.size(), u8""_sv),
        }));
  }

  {
    Test_Parser p(u8R"(c = <div></div> <div></div> <div></div>;)"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_3_OFFSETS(
                p.code, Diag_Adjacent_JSX_Without_Parent,  //
                begin, u8"c = "_sv.size(), u8""_sv,        //
                begin_of_second_element, u8"c = <div></div> "_sv.size(),
                u8""_sv,  //
                end, u8"c = <div></div> <div></div> <div></div>"_sv.size(),
                u8""_sv),
        }));
  }

  // Second element should be visited like normal.
  {
    Test_Parser p(
        u8R"(c = <FirstComponent></FirstComponent> <SecondComponent>{child}</SecondComponent>;)"_sv,
        jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.variable_uses,
        ElementsAreArray({u8"FirstComponent", u8"SecondComponent", u8"child"}));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Adjacent_JSX_Without_Parent),
                          }));
  }

  // Because the second element is on its own line, ASI should kick in, and the
  // following example is syntactically valid. However, Babel, Espree, Flow, and
  // TypeScript all agree that ASI does not kick in. Therefore, we report a
  // syntax error, despite what the specification says.
  {
    Test_Parser p(
        u8"c = <FirstComponent></FirstComponent>\n<SecondComponent></SecondComponent>;"_sv,
        jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"FirstComponent", u8"SecondComponent"}));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Adjacent_JSX_Without_Parent),
                          }));
  }

  // The following code looks like adjacent JSX elements, but it's actually a
  // JSX element followed by some legal operators. However, Babel, Espree, Flow,
  // and TypeScript all agree that '<' is not allowed after a JSX element.
  // Therefore, we report a syntax error, despite what the specification says.
  // https://github.com/facebook/jsx/issues/120
  {
    //                  binary operators  v v           v (according to spec)
    Test_Parser p(u8"c = <div></div> <i>/{child}</i>\ndone"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"child", u8"done"}));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Adjacent_JSX_Without_Parent),
                          }));
  }

  {
    Test_Parser p(u8"c = <First></First><Second attr='value'></Second>;"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"First", u8"Second"}));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(Diag_Adjacent_JSX_Without_Parent),
                          }));
  }
}

TEST_F(Test_Parse_JSX, correctly_capitalized_attribute) {
  {
    Test_Parser p(u8R"(c = <td colSpan="2" />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8R"(c = <div onClick={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(Test_Parse_JSX, event_attributes_should_be_camel_case) {
  {
    Test_Parser p(u8R"(c = <div onclick={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_FIELDS(
                        Diag_JSX_Event_Attribute_Should_Be_Camel_Case,  //
                        attribute_name,
                        Offsets_Matcher(p.code, u8"c = <div "_sv.size(),
                                        u8"onclick"_sv),  //
                        expected_attribute_name, u8"onClick"_sv),
                }));
  }

  // TODO(strager): Should we also report that the handler's value is missing?
  {
    Test_Parser p(u8R"(c = <div onclick />;)"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_FIELDS(
                        Diag_JSX_Event_Attribute_Should_Be_Camel_Case,  //
                        attribute_name,
                        Offsets_Matcher(p.code, u8"c = <div "_sv.size(),
                                        u8"onclick"_sv),  //
                        expected_attribute_name, u8"onClick"_sv),
                }));
  }

  {
    Test_Parser p(u8R"(c = <div onmouseenter={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_FIELDS(
                        Diag_JSX_Event_Attribute_Should_Be_Camel_Case,  //
                        attribute_name,
                        Offsets_Matcher(p.code, u8"c = <div "_sv.size(),
                                        u8"onmouseenter"_sv),  //
                        expected_attribute_name, u8"onMouseEnter"_sv),
                }));
  }

  {
    Test_Parser p(u8R"(c = <div oncustomevent={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_FIELDS(
                        Diag_JSX_Event_Attribute_Should_Be_Camel_Case,  //
                        attribute_name,
                        Offsets_Matcher(p.code, u8"c = <div "_sv.size(),
                                        u8"oncustomevent"_sv),  //
                        expected_attribute_name, u8"onCustomevent"_sv),
                }));
  }
}

TEST_F(Test_Parse_JSX, miscapitalized_attribute) {
  {
    Test_Parser p(u8R"(c = <td colspan="2" />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(Diag_JSX_Attribute_Has_Wrong_Capitalization,  //
                               attribute_name,
                               Offsets_Matcher(p.code, u8"c = <td "_sv.size(),
                                               u8"colspan"_sv),  //
                               expected_attribute_name, u8"colSpan"_sv),
        }));
  }

  {
    Test_Parser p(u8R"(c = <div onMouseenter={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(Diag_JSX_Attribute_Has_Wrong_Capitalization,  //
                               attribute_name,
                               Offsets_Matcher(p.code, u8"c = <div "_sv.size(),
                                               u8"onmouseenter"_sv),  //
                               expected_attribute_name, u8"onMouseEnter"_sv),
        }));
  }

  {
    Test_Parser p(u8R"(c = <div onmouseENTER={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(Diag_JSX_Attribute_Has_Wrong_Capitalization,  //
                               attribute_name,
                               Offsets_Matcher(p.code, u8"c = <div "_sv.size(),
                                               u8"onmouseENTER"_sv),  //
                               expected_attribute_name, u8"onMouseEnter"_sv),
        }));
  }
}

TEST_F(Test_Parse_JSX, commonly_misspelled_attribute) {
  {
    Test_Parser p(u8R"(c = <span class="item"></span>;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(Diag_JSX_Attribute_Renamed_By_React,  //
                               attribute_name,
                               Offsets_Matcher(p.code, u8"c = <span "_sv.size(),
                                               u8"class"_sv),  //
                               react_attribute_name, u8"className"_sv),
        }));
  }
}

TEST_F(Test_Parse_JSX, attribute_checking_ignores_namespaced_attributes) {
  {
    Test_Parser p(u8R"(c = <div ns:onmouseenter={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8R"(c = <div onmouseenter:onmouseenter={handler} />;)"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8R"(c = <div class:class="my-css-class" />;)"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(Test_Parse_JSX, attribute_checking_ignores_namespaced_elements) {
  {
    Test_Parser p(u8R"(c = <svg:g onmouseenter={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8R"(c = <svg:g class="red" />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(Test_Parse_JSX, attribute_checking_ignores_user_components) {
  {
    Test_Parser p(u8R"(c = <MyComponent onmouseenter={handler} />;)"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8R"(c = <MyComponent class="red" />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(
        u8R"(c = <mymodule.mycomponent onmouseenter={handler} />;)"_sv,
        jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    Test_Parser p(u8R"(c = <mymodule.mycomponent class="red" />;)"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(Test_Parse_JSX, prop_needs_an_expression) {
  {
    Test_Parser p(u8"c = <MyComponent custom={}></MyComponent>;"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, Diag_JSX_Prop_Is_Missing_Expression,
                        left_brace_to_right_brace,
                        u8"c = <MyComponent custom="_sv.size(), u8"{}"_sv),
                }));
  }

  {
    Test_Parser p(u8"c = <MyComponent custom={ }></MyComponent>;"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, Diag_JSX_Prop_Is_Missing_Expression,
                        left_brace_to_right_brace,
                        u8"c = <MyComponent custom="_sv.size(), u8"{ }"_sv),
                }));
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
