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
#include <quick-lint-js/fe/diagnostic-types.h>
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
class test_parse_jsx : public test_parse_expression {};

TEST_F(test_parse_jsx, jsx_is_not_supported_in_vanilla_javascript) {
  parser_options options;
  options.jsx = false;
  test_parser p(
      u8"<MyComponent attr={value}><Inner>hello</Inner></MyComponent>"_sv,
      options, capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(
      p.errors,
      ElementsAreArray({
          DIAG_TYPE_OFFSETS(p.code, diag_jsx_not_allowed_in_javascript,  //
                            jsx_start, 0, u8"<"_sv),
      }));
  EXPECT_THAT(p.variable_uses,
              ElementsAreArray({u8"MyComponent", u8"value", u8"Inner"}));
}

TEST_F(test_parse_jsx, jsx_is_not_supported_in_vanilla_typescript) {
  parser_options options;
  options.jsx = false;
  options.typescript = true;

  {
    test_parser p(
        u8"<MyComponent attr={value}><Inner>hello</Inner></MyComponent>"_sv,
        options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_jsx_not_allowed_in_typescript,  //
                              jsx_start, 0, u8"<"_sv),
        }));
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"MyComponent", u8"value", u8"Inner"}));
  }

  {
    test_parser p(u8"<><Inner>hello</Inner><Inner /></>"_sv, options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_jsx_not_allowed_in_typescript,  //
                              jsx_start, 0, u8"<"_sv),
        }));
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"Inner", u8"Inner"}));
  }

  // TODO(strager): Detect more cases. There is syntax overlap with generic
  // arrow functions and with type assertions.
}

TEST_F(test_parse_jsx, empty_intrinsic_element) {
  test_parser p(u8"c = <div></div>;"_sv, jsx_options, capture_diags);
  p.parse_and_visit_module();
  EXPECT_THAT(p.visits, ElementsAreArray({
                            "visit_variable_assignment",  // c
                            "visit_end_of_module",
                        }));
  EXPECT_THAT(p.errors, IsEmpty());
}

TEST_F(test_parse_jsx, empty_user_element) {
  test_parser p(u8"c = <MyComponent></MyComponent>;"_sv, jsx_options,
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

TEST_F(test_parse_jsx, member_component) {
  test_parser p(
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

TEST_F(test_parse_jsx, element_child_element) {
  {
    test_parser p(u8"c = <outer><INNER></INNER></outer>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"c = <OUTER><INNER></INNER></OUTER>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OUTER", u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"c = <NS:OUTER><INNER></INNER></NS:OUTER>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(
        u8"c = <outer.Component><INNER></INNER></outer.Component>;"_sv,
        jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"outer", u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"c = <><INNER></INNER></>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_jsx, element_child_expression) {
  {
    test_parser p(u8"c = <outer>{INNER}</outer>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"c = <OUTER>{INNER}</OUTER>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OUTER", u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"c = <NS:OUTER>{INNER}</NS:OUTER>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"c = <outer.Component>{INNER}</outer.Component>;"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"outer", u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"c = <>{INNER}</>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"INNER"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_jsx, element_attribute_expression) {
  {
    test_parser p(u8"c = <outer attr={attrValue}></outer>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"attrValue"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"c = <OUTER attr={attrValue}></OUTER>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"OUTER", u8"attrValue"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8"c = <NS:OUTER attr={attrValue}></NS:OUTER>;"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"attrValue"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(
        u8"c = <outer.Component attr={attrValue}></outer.Component>;"_sv,
        jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"outer", u8"attrValue"}));
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_jsx, attribute_without_name_must_be_spread) {
  {
    test_parser p(u8"c = <div {attr} />;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"attr"}));
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(
                        p.code, diag_missing_dots_for_attribute_spread,  //
                        expected_dots, strlen(u8"c = <div {"), u8""_sv),
                }));
  }
}

TEST_F(test_parse_jsx, begin_and_end_tags_must_match) {
  {
    test_parser p(u8"c = <div></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_OFFSETS(p.code, diag_mismatched_jsx_tags,  //
                                        opening_tag_name, strlen(u8"c = <"),
                                        u8"div"_sv, closing_tag_name,
                                        strlen(u8"c = <div></"), u8"span"_sv),
                }));
  }

  // opening_tag_name span for normal tag:
  {
    test_parser p(u8"c = < div ></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_mismatched_jsx_tags,  //
                              opening_tag_name, strlen(u8"c = < "), u8"div"_sv),
        }));
  }

  // opening_tag_name span for fragment tag:
  {
    test_parser p(u8"c = <  ></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_mismatched_jsx_tags,  //
                              opening_tag_name, strlen(u8"c = <"), u8""_sv),
        }));
  }

  // opening_tag_name span for member tag:
  {
    test_parser p(u8"c = < module . Component ></span>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_mismatched_jsx_tags,  //
                                      opening_tag_name, strlen(u8"c = < "),
                                      u8"module . Component"_sv),
                }));
  }

  // opening_tag_name span for namespaced tag:
  {
    test_parser p(u8"c = < svg : path ></span>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_mismatched_jsx_tags,  //
                                      opening_tag_name, strlen(u8"c = < "),
                                      u8"svg : path"_sv),
                }));
  }

  // closing_tag_name span for normal tag:
  {
    test_parser p(u8"c = <div></ span >;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_mismatched_jsx_tags,  //
                                      closing_tag_name,
                                      strlen(u8"c = <div></ "), u8"span"_sv),
                }));
  }

  // closing_tag_name span for fragment tag:
  {
    test_parser p(u8"c = <div></  >;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_OFFSETS(p.code, diag_mismatched_jsx_tags,  //
                                      closing_tag_name,
                                      strlen(u8"c = <div></  "), u8""_sv),
                }));
  }

  // closing_tag_name span for member tag:
  {
    test_parser p(u8"c = <div></ module . Component >;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_mismatched_jsx_tags,  //
                              closing_tag_name, strlen(u8"c = <div></ "),
                              u8"module . Component"_sv),
        }));
  }

  // closing_tag_name span for namespaced tag:
  {
    test_parser p(u8"c = <div></ svg : path >;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_OFFSETS(p.code, diag_mismatched_jsx_tags,  //
                              closing_tag_name, strlen(u8"c = <div></ "),
                              u8"svg : path"_sv),
        }));
  }

  // opening_tag_name_pretty for normal tag:
  {
    test_parser p(u8"c = <div></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_FIELD(diag_mismatched_jsx_tags,
                                    opening_tag_name_pretty, u8"div"sv),
                }));
  }

  // opening_tag_name_pretty for fragment tag:
  {
    test_parser p(u8"c = <  ></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(diag_mismatched_jsx_tags,
                                              opening_tag_name_pretty, u8""sv),
                          }));
  }

  // opening_tag_name_pretty for member tag:
  {
    test_parser p(u8"c = <module.Component></span>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(diag_mismatched_jsx_tags,
                                              opening_tag_name_pretty,
                                              u8"module.Component"sv),
                          }));
  }

  // opening_tag_name_pretty for namespaced tag:
  {
    test_parser p(u8"c = <svg:path></span>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_FIELD(diag_mismatched_jsx_tags,
                                    opening_tag_name_pretty, u8"svg:path"sv),
                }));
  }

  for (string8_view jsx : {
           u8"<div></span>"sv,
           u8"<></span>"sv,
           u8"<div></>"sv,

           u8"<svg:g></svg:path>"sv,
           u8"<svg:g></png:g>"sv,
           u8"<svg:g></png:path>"sv,

           u8"<svg></svg:path>"sv,
           u8"<svg:path></svg>"sv,
           u8"<path></svg:path>"sv,
           u8"<svg:path></path>"sv,

           u8"<svg:path></>"sv,
           u8"<></svg:path>"sv,

           u8"<module.Link></module.Route>"sv,
           u8"<module.Link></mod.Link>"sv,
           u8"<Link></module.Link>"sv,
           u8"<module.Link></Link>"sv,
           u8"<Module></Module.Link>"sv,
           u8"<Module.Link></Module>"sv,

           u8"<module.Link></>"sv,
           u8"<></module.Link>"sv,

           u8"<module.submodule.Link></module.submodule.Route>"sv,
           u8"<module.submodule.Link></submodule.module.Link>"sv,

           u8"<module.submodule.Link></module.Link>"sv,
           u8"<module.Link></module.submodule.Link>"sv,

           u8"<a:a></a.a>"sv,
           u8"<a.a></a:a>"sv,
           u8"<A></A.A>"sv,
           u8"<A.A></A>"sv,
           u8"<A></A:A>"sv,
           u8"<A:A></A>"sv,
       }) {
    test_parser p(concat(u8"c = "_sv, jsx, u8";"_sv), jsx_options,
                  capture_diags);
    SCOPED_TRACE(p.code);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(diag_mismatched_jsx_tags),
                          }));
  }
}

TEST_F(test_parse_jsx,
       begin_and_end_tag_mismatch_message_excludes_comments_and_whitespace) {
  {
    test_parser p(u8"c = < div ></x>;"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_FIELD(diag_mismatched_jsx_tags,
                                    opening_tag_name_pretty, u8"div"sv),
                }));
  }

  {
    test_parser p(u8"c = < my . /* hello */ Component ></x>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(diag_mismatched_jsx_tags,
                                              opening_tag_name_pretty,
                                              u8"my.Component"sv),
                          }));
  }

  {
    test_parser p(u8"c = < svg /* */ : path ></x>;"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_FIELD(diag_mismatched_jsx_tags,
                                    opening_tag_name_pretty, u8"svg:path"sv),
                }));
  }
}

TEST_F(test_parse_jsx,
       begin_and_end_tag_mismatch_message_include_unicode_escapes) {
  {
    test_parser p(u8R"(c = <d\u{69}v></x>;)"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_FIELD(diag_mismatched_jsx_tags,
                                    opening_tag_name_pretty, u8R"(d\u{69}v)"sv),
                }));
  }

  {
    test_parser p(u8R"(c = <s\u{76}g:p\u{69}th></x>;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(diag_mismatched_jsx_tags,
                                              opening_tag_name_pretty,
                                              u8R"(s\u{76}g:p\u{69}th)"sv),
                          }));
  }

  {
    test_parser p(u8R"(c = <m\u{79}.Com\u{70}onent></x>;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE_FIELD(diag_mismatched_jsx_tags,
                                              opening_tag_name_pretty,
                                              u8R"(m\u{79}.Com\u{70}onent)"sv),
                          }));
  }
}

TEST_F(test_parse_jsx, begin_and_end_tags_match_after_normalization) {
  {
    test_parser p(u8R"(c = <div></\u{64}\u{69}\u{76}>;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty())
        << "shouldn't report diag_mismatched_jsx_tags";
  }
}

TEST_F(test_parse_jsx, adjacent_tags_without_outer_fragment) {
  {
    test_parser p(u8R"(c = <div></div> <div></div>;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_3_OFFSETS(
                p.code, diag_adjacent_jsx_without_parent,                     //
                begin, strlen(u8"c = "), u8"",                                //
                begin_of_second_element, strlen(u8"c = <div></div> "), u8"",  //
                end, strlen(u8"c = <div></div> <div></div>"), u8""),
        }));
  }

  {
    test_parser p(u8R"(c = <div></div> <div></div> <div></div>;)"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_3_OFFSETS(
                p.code, diag_adjacent_jsx_without_parent,                     //
                begin, strlen(u8"c = "), u8"",                                //
                begin_of_second_element, strlen(u8"c = <div></div> "), u8"",  //
                end, strlen(u8"c = <div></div> <div></div> <div></div>"), u8""),
        }));
  }

  // Second element should be visited like normal.
  {
    test_parser p(
        u8R"(c = <FirstComponent></FirstComponent> <SecondComponent>{child}</SecondComponent>;)"_sv,
        jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.variable_uses,
        ElementsAreArray({u8"FirstComponent", u8"SecondComponent", u8"child"}));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(diag_adjacent_jsx_without_parent),
                          }));
  }

  // Because the second element is on its own line, ASI should kick in, and the
  // following example is syntactically valid. However, Babel, Espree, Flow, and
  // TypeScript all agree that ASI does not kick in. Therefore, we report a
  // syntax error, despite what the specification says.
  {
    test_parser p(
        u8"c = <FirstComponent></FirstComponent>\n<SecondComponent></SecondComponent>;"_sv,
        jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses,
                ElementsAreArray({u8"FirstComponent", u8"SecondComponent"}));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(diag_adjacent_jsx_without_parent),
                          }));
  }

  // The following code looks like adjacent JSX elements, but it's actually a
  // JSX element followed by some legal operators. However, Babel, Espree, Flow,
  // and TypeScript all agree that '<' is not allowed after a JSX element.
  // Therefore, we report a syntax error, despite what the specification says.
  // https://github.com/facebook/jsx/issues/120
  {
    //                  binary operators  v v           v (according to spec)
    test_parser p(u8"c = <div></div> <i>/{child}</i>\ndone"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"child", u8"done"}));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(diag_adjacent_jsx_without_parent),
                          }));
  }

  {
    test_parser p(u8"c = <First></First><Second attr='value'></Second>;"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"First", u8"Second"}));
    EXPECT_THAT(p.errors, ElementsAreArray({
                              DIAG_TYPE(diag_adjacent_jsx_without_parent),
                          }));
  }
}

TEST_F(test_parse_jsx, correctly_capitalized_attribute) {
  {
    test_parser p(u8R"(c = <td colSpan="2" />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8R"(c = <div onClick={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_jsx, event_attributes_should_be_camel_case) {
  {
    test_parser p(u8R"(c = <div onclick={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_FIELDS(
                        diag_jsx_event_attribute_should_be_camel_case,  //
                        attribute_name,
                        offsets_matcher(p.code, strlen(u8"c = <div "),
                                        u8"onclick"_sv),  //
                        expected_attribute_name, u8"onClick"sv),
                }));
  }

  // TODO(strager): Should we also report that the handler's value is missing?
  {
    test_parser p(u8R"(c = <div onclick />;)"_sv, jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_FIELDS(
                        diag_jsx_event_attribute_should_be_camel_case,  //
                        attribute_name,
                        offsets_matcher(p.code, strlen(u8"c = <div "),
                                        u8"onclick"_sv),  //
                        expected_attribute_name, u8"onClick"sv),
                }));
  }

  {
    test_parser p(u8R"(c = <div onmouseenter={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_FIELDS(
                        diag_jsx_event_attribute_should_be_camel_case,  //
                        attribute_name,
                        offsets_matcher(p.code, strlen(u8"c = <div "),
                                        u8"onmouseenter"_sv),  //
                        expected_attribute_name, u8"onMouseEnter"sv),
                }));
  }

  {
    test_parser p(u8R"(c = <div oncustomevent={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors,
                ElementsAreArray({
                    DIAG_TYPE_2_FIELDS(
                        diag_jsx_event_attribute_should_be_camel_case,  //
                        attribute_name,
                        offsets_matcher(p.code, strlen(u8"c = <div "),
                                        u8"oncustomevent"_sv),  //
                        expected_attribute_name, u8"onCustomevent"sv),
                }));
  }
}

TEST_F(test_parse_jsx, miscapitalized_attribute) {
  {
    test_parser p(u8R"(c = <td colspan="2" />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(diag_jsx_attribute_has_wrong_capitalization,  //
                               attribute_name,
                               offsets_matcher(p.code, strlen(u8"c = <td "),
                                               u8"colspan"_sv),  //
                               expected_attribute_name, u8"colSpan"sv),
        }));
  }

  {
    test_parser p(u8R"(c = <div onMouseenter={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(diag_jsx_attribute_has_wrong_capitalization,  //
                               attribute_name,
                               offsets_matcher(p.code, strlen(u8"c = <div "),
                                               u8"onmouseenter"_sv),  //
                               expected_attribute_name, u8"onMouseEnter"sv),
        }));
  }

  {
    test_parser p(u8R"(c = <div onmouseENTER={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(diag_jsx_attribute_has_wrong_capitalization,  //
                               attribute_name,
                               offsets_matcher(p.code, strlen(u8"c = <div "),
                                               u8"onmouseENTER"_sv),  //
                               expected_attribute_name, u8"onMouseEnter"sv),
        }));
  }
}

TEST_F(test_parse_jsx, commonly_misspelled_attribute) {
  {
    test_parser p(u8R"(c = <span class="item"></span>;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(
        p.errors,
        ElementsAreArray({
            DIAG_TYPE_2_FIELDS(diag_jsx_attribute_renamed_by_react,  //
                               attribute_name,
                               offsets_matcher(p.code, strlen(u8"c = <span "),
                                               u8"class"_sv),  //
                               react_attribute_name, u8"className"sv),
        }));
  }
}

TEST_F(test_parse_jsx, attribute_checking_ignores_namespaced_attributes) {
  {
    test_parser p(u8R"(c = <div ns:onmouseenter={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8R"(c = <div onmouseenter:onmouseenter={handler} />;)"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8R"(c = <div class:class="my-css-class" />;)"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_jsx, attribute_checking_ignores_namespaced_elements) {
  {
    test_parser p(u8R"(c = <svg:g onmouseenter={handler} />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8R"(c = <svg:g class="red" />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }
}

TEST_F(test_parse_jsx, attribute_checking_ignores_user_components) {
  {
    test_parser p(u8R"(c = <MyComponent onmouseenter={handler} />;)"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8R"(c = <MyComponent class="red" />;)"_sv, jsx_options,
                  capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(
        u8R"(c = <mymodule.mycomponent onmouseenter={handler} />;)"_sv,
        jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
  }

  {
    test_parser p(u8R"(c = <mymodule.mycomponent class="red" />;)"_sv,
                  jsx_options, capture_diags);
    p.parse_and_visit_module();
    EXPECT_THAT(p.errors, IsEmpty());
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
