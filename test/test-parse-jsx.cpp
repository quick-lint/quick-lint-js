// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::Contains;
using ::testing::ElementsAre;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(test_parse_jsx, jsx_is_not_supported_in_vanilla_javascript) {
  // If parsing was not started with
  // parse_and_visit_module_catching_fatal_parse_errors, then we can't halt
  // parsing at the '<'. Error recovery will do a bad job.
  padded_string code(u8"<MyComponent attr={value}>hello</MyComponent>"_sv);
  spy_visitor v;
  parser_options options;
  options.jsx = false;
  parser p(&code, &v, options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.errors, Contains(ERROR_TYPE_OFFSETS(
                            &code, diag_jsx_not_yet_implemented,  //
                            jsx_start, 0, u8"<")));
}

#if QLJS_HAVE_SETJMP
TEST(test_parse_jsx, parsing_stops_on_jsx_in_vanilla_javascript) {
  padded_string code(u8"<MyComponent attr={value}>hello</MyComponent>"_sv);
  spy_visitor v;
  parser_options options;
  options.jsx = false;
  parser p(&code, &v, options);
  bool ok = p.parse_and_visit_module_catching_fatal_parse_errors(v);
  EXPECT_FALSE(ok);
  EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                            &code, diag_jsx_not_yet_implemented,  //
                            jsx_start, 0, u8"<")));
}
#endif

TEST(test_parse_jsx, empty_intrinsic_element) {
  padded_string code(u8"c = <div></div>;"_sv);
  spy_visitor v;
  parser p(&code, &v, jsx_options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_assignment",  // c
                                    "visit_end_of_module"));
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse_jsx, empty_user_element) {
  padded_string code(u8"c = <MyComponent></MyComponent>;"_sv);
  spy_visitor v;
  parser p(&code, &v, jsx_options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         // MyComponent
                                    "visit_variable_assignment",  // c
                                    "visit_end_of_module"));
  EXPECT_THAT(v.variable_uses,
              ElementsAre(spy_visitor::visited_variable_use{u8"MyComponent"}));
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse_jsx, member_component) {
  padded_string code(
      u8"c = <module.submodule.MyComponent></module.submodule.MyComponent>;"_sv);
  spy_visitor v;
  parser p(&code, &v, jsx_options);
  p.parse_and_visit_module(v);
  EXPECT_THAT(v.visits, ElementsAre("visit_variable_use",         // module
                                    "visit_variable_assignment",  // c
                                    "visit_end_of_module"));
  EXPECT_THAT(v.variable_uses,
              ElementsAre(spy_visitor::visited_variable_use{u8"module"}));
  EXPECT_THAT(v.errors, IsEmpty());
}

TEST(test_parse_jsx, element_child_element) {
  {
    padded_string code(u8"c = <outer><INNER></INNER></outer>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"INNER"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"c = <OUTER><INNER></INNER></OUTER>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"OUTER"},
                            spy_visitor::visited_variable_use{u8"INNER"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"c = <NS:OUTER><INNER></INNER></NS:OUTER>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"INNER"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(
        u8"c = <outer.Component><INNER></INNER></outer.Component>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"outer"},
                            spy_visitor::visited_variable_use{u8"INNER"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"c = <><INNER></INNER></>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"INNER"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse_jsx, element_child_expression) {
  {
    padded_string code(u8"c = <outer>{INNER}</outer>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"INNER"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"c = <OUTER>{INNER}</OUTER>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"OUTER"},
                            spy_visitor::visited_variable_use{u8"INNER"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"c = <NS:OUTER>{INNER}</NS:OUTER>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"INNER"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"c = <outer.Component>{INNER}</outer.Component>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"outer"},
                            spy_visitor::visited_variable_use{u8"INNER"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"c = <>{INNER}</>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"INNER"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse_jsx, element_attribute_expression) {
  {
    padded_string code(u8"c = <outer attr={attrValue}></outer>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"attrValue"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"c = <OUTER attr={attrValue}></OUTER>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"OUTER"},
                            spy_visitor::visited_variable_use{u8"attrValue"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8"c = <NS:OUTER attr={attrValue}></NS:OUTER>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"attrValue"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(
        u8"c = <outer.Component attr={attrValue}></outer.Component>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"outer"},
                            spy_visitor::visited_variable_use{u8"attrValue"}));
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse_jsx, attribute_without_name_must_be_spread) {
  {
    padded_string code(u8"c = <div {attr} />;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"attr"}));
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_missing_dots_for_attribute_spread,  //
                              expected_dots, strlen(u8"c = <div {"), u8"")));
  }
}

TEST(test_parse_jsx, begin_and_end_tags_must_match) {
  {
    padded_string code(u8"c = <div></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_OFFSETS(
                    &code, diag_mismatched_jsx_tags,               //
                    opening_tag_name, strlen(u8"c = <"), u8"div",  //
                    closing_tag_name, strlen(u8"c = <div></"), u8"span")));
  }

  // opening_tag_name span for normal tag:
  {
    padded_string code(u8"c = < div ></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_mismatched_jsx_tags,  //
                              opening_tag_name, strlen(u8"c = < "), u8"div")));
  }

  // opening_tag_name span for fragment tag:
  {
    padded_string code(u8"c = <  ></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_mismatched_jsx_tags,  //
                              opening_tag_name, strlen(u8"c = <"), u8"")));
  }

  // opening_tag_name span for member tag:
  {
    padded_string code(u8"c = < module . Component ></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_mismatched_jsx_tags,  //
                              opening_tag_name, strlen(u8"c = < "),
                              u8"module . Component")));
  }

  // opening_tag_name span for namespaced tag:
  {
    padded_string code(u8"c = < svg : path ></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_mismatched_jsx_tags,  //
                    opening_tag_name, strlen(u8"c = < "), u8"svg : path")));
  }

  // closing_tag_name span for normal tag:
  {
    padded_string code(u8"c = <div></ span >;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_mismatched_jsx_tags,  //
                    closing_tag_name, strlen(u8"c = <div></ "), u8"span")));
  }

  // closing_tag_name span for fragment tag:
  {
    padded_string code(u8"c = <div></  >;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, diag_mismatched_jsx_tags,  //
                    closing_tag_name, strlen(u8"c = <div></  "), u8"")));
  }

  // closing_tag_name span for member tag:
  {
    padded_string code(u8"c = <div></ module . Component >;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_mismatched_jsx_tags,  //
                              closing_tag_name, strlen(u8"c = <div></ "),
                              u8"module . Component")));
  }

  // closing_tag_name span for namespaced tag:
  {
    padded_string code(u8"c = <div></ svg : path >;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, diag_mismatched_jsx_tags,  //
                              closing_tag_name, strlen(u8"c = <div></ "),
                              u8"svg : path")));
  }

  // opening_tag_name_pretty for normal tag:
  {
    padded_string code(u8"c = <div></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(diag_mismatched_jsx_tags,
                                                       opening_tag_name_pretty,
                                                       u8"div"sv)));
  }

  // opening_tag_name_pretty for fragment tag:
  {
    padded_string code(u8"c = <  ></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_FIELD(diag_mismatched_jsx_tags,
                                             opening_tag_name_pretty, u8""sv)));
  }

  // opening_tag_name_pretty for member tag:
  {
    padded_string code(u8"c = <module.Component></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              diag_mismatched_jsx_tags, opening_tag_name_pretty,
                              u8"module.Component"sv)));
  }

  // opening_tag_name_pretty for namespaced tag:
  {
    padded_string code(u8"c = <svg:path></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(diag_mismatched_jsx_tags,
                                                       opening_tag_name_pretty,
                                                       u8"svg:path"sv)));
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
    padded_string code(u8"c = " + string8(jsx) + u8";");
    SCOPED_TRACE(code);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE(diag_mismatched_jsx_tags)));
  }
}

TEST(test_parse_jsx,
     begin_and_end_tag_mismatch_message_excludes_comments_and_whitespace) {
  {
    padded_string code(u8"c = < div ></x>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(diag_mismatched_jsx_tags,
                                                       opening_tag_name_pretty,
                                                       u8"div"sv)));
  }

  {
    padded_string code(u8"c = < my . /* hello */ Component ></x>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(diag_mismatched_jsx_tags,
                                                       opening_tag_name_pretty,
                                                       u8"my.Component"sv)));
  }

  {
    padded_string code(u8"c = < svg /* */ : path ></x>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(diag_mismatched_jsx_tags,
                                                       opening_tag_name_pretty,
                                                       u8"svg:path"sv)));
  }
}

TEST(test_parse_jsx,
     begin_and_end_tag_mismatch_message_include_unicode_escapes) {
  {
    padded_string code(u8R"(c = <d\u{69}v></x>;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(diag_mismatched_jsx_tags,
                                                       opening_tag_name_pretty,
                                                       u8R"(d\u{69}v)"sv)));
  }

  {
    padded_string code(u8R"(c = <s\u{76}g:p\u{69}th></x>;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              diag_mismatched_jsx_tags, opening_tag_name_pretty,
                              u8R"(s\u{76}g:p\u{69}th)"sv)));
  }

  {
    padded_string code(u8R"(c = <m\u{79}.Com\u{70}onent></x>;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_FIELD(
                              diag_mismatched_jsx_tags, opening_tag_name_pretty,
                              u8R"(m\u{79}.Com\u{70}onent)"sv)));
  }
}

TEST(test_parse_jsx, begin_and_end_tags_match_after_normalization) {
  {
    padded_string code(u8R"(c = <div></\u{64}\u{69}\u{76}>;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty())
        << "shouldn't report diag_mismatched_jsx_tags";
  }
}

TEST(test_parse_jsx, adjacent_tags_without_outer_fragment) {
  {
    padded_string code(u8R"(c = <div></div> <div></div>;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_3_OFFSETS(
            &code, diag_adjacent_jsx_without_parent,                      //
            begin, strlen(u8"c = "), u8"",                                //
            begin_of_second_element, strlen(u8"c = <div></div> "), u8"",  //
            end, strlen(u8"c = <div></div> <div></div>"), u8"")));
  }

  {
    padded_string code(u8R"(c = <div></div> <div></div> <div></div>;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_3_OFFSETS(
            &code, diag_adjacent_jsx_without_parent,                      //
            begin, strlen(u8"c = "), u8"",                                //
            begin_of_second_element, strlen(u8"c = <div></div> "), u8"",  //
            end, strlen(u8"c = <div></div> <div></div> <div></div>"), u8"")));
  }

  // Second element should be visited like normal.
  {
    padded_string code(
        u8R"(c = <FirstComponent></FirstComponent> <SecondComponent>{child}</SecondComponent>;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.variable_uses,
        ElementsAre(spy_visitor::visited_variable_use{u8"FirstComponent"},
                    spy_visitor::visited_variable_use{u8"SecondComponent"},
                    spy_visitor::visited_variable_use{u8"child"}));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE(diag_adjacent_jsx_without_parent)));
  }

  // Because the second element is on its own line, ASI should kick in, and the
  // following example is syntactically valid. However, Babel, Espree, Flow, and
  // TypeScript all agree that ASI does not kick in. Therefore, we report a
  // syntax error, despite what the specification says.
  {
    padded_string code(
        u8"c = <FirstComponent></FirstComponent>\n<SecondComponent></SecondComponent>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.variable_uses,
        ElementsAre(spy_visitor::visited_variable_use{u8"FirstComponent"},
                    spy_visitor::visited_variable_use{u8"SecondComponent"}));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE(diag_adjacent_jsx_without_parent)));
  }

  // The following code looks like adjacent JSX elements, but it's actually a
  // JSX element followed by some legal operators. However, Babel, Espree, Flow,
  // and TypeScript all agree that '<' is not allowed after a JSX element.
  // Therefore, we report a syntax error, despite what the specification says.
  // https://github.com/facebook/jsx/issues/120
  {
    //                  binary operators  v v           v (according to spec)
    padded_string code(u8"c = <div></div> <i>/{child}</i>\ndone"_sv);
    //                               regexp  ^^^^^^^^     (according to spec)
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"child"},
                            spy_visitor::visited_variable_use{u8"done"}));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE(diag_adjacent_jsx_without_parent)));
  }

  {
    padded_string code(
        u8"c = <First></First><Second attr='value'></Second>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.variable_uses,
                ElementsAre(spy_visitor::visited_variable_use{u8"First"},
                            spy_visitor::visited_variable_use{u8"Second"}));
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE(diag_adjacent_jsx_without_parent)));
  }
}

TEST(test_parse_jsx, correctly_capitalized_attribute) {
  {
    padded_string code(u8R"(c = <td colSpan="2" />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8R"(c = <div onClick={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse_jsx, event_attributes_should_be_camel_case) {
  {
    padded_string code(u8R"(c = <div onclick={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            diag_jsx_event_attribute_should_be_camel_case,  //
            attribute_name,
            offsets_matcher(&code, strlen(u8"c = <div "), u8"onclick"),  //
            expected_attribute_name, u8"onClick"sv)));
  }

  // TODO(strager): Should we also report that the handler's value is missing?
  {
    padded_string code(u8R"(c = <div onclick />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    diag_jsx_event_attribute_should_be_camel_case,  //
                    attribute_name,
                    offsets_matcher(&code, strlen(u8"c = <div "),
                                    u8"onclick"),  //
                    expected_attribute_name, u8"onClick"sv)));
  }

  {
    padded_string code(u8R"(c = <div onmouseenter={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            diag_jsx_event_attribute_should_be_camel_case,  //
            attribute_name,
            offsets_matcher(&code, strlen(u8"c = <div "), u8"onmouseenter"),  //
            expected_attribute_name, u8"onMouseEnter"sv)));
  }

  {
    padded_string code(u8R"(c = <div oncustomevent={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_2_FIELDS(
                    diag_jsx_event_attribute_should_be_camel_case,  //
                    attribute_name,
                    offsets_matcher(&code, strlen(u8"c = <div "),
                                    u8"oncustomevent"),  //
                    expected_attribute_name, u8"onCustomevent"sv)));
  }
}

TEST(test_parse_jsx, miscapitalized_attribute) {
  {
    padded_string code(u8R"(c = <td colspan="2" />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            diag_jsx_attribute_has_wrong_capitalization,  //
            attribute_name,
            offsets_matcher(&code, strlen(u8"c = <td "), u8"colspan"),  //
            expected_attribute_name, u8"colSpan"sv)));
  }

  {
    padded_string code(u8R"(c = <div onMouseenter={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            diag_jsx_attribute_has_wrong_capitalization,  //
            attribute_name,
            offsets_matcher(&code, strlen(u8"c = <div "), u8"onmouseenter"),  //
            expected_attribute_name, u8"onMouseEnter"sv)));
  }

  {
    padded_string code(u8R"(c = <div onmouseENTER={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            diag_jsx_attribute_has_wrong_capitalization,  //
            attribute_name,
            offsets_matcher(&code, strlen(u8"c = <div "), u8"onmouseENTER"),  //
            expected_attribute_name, u8"onMouseEnter"sv)));
  }
}

TEST(test_parse_jsx, commonly_misspelled_attribute) {
  {
    padded_string code(u8R"(c = <span class="item"></span>;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(
        v.errors,
        ElementsAre(ERROR_TYPE_2_FIELDS(
            diag_jsx_attribute_renamed_by_react,  //
            attribute_name,
            offsets_matcher(&code, strlen(u8"c = <span "), u8"class"),  //
            react_attribute_name, u8"className"sv)));
  }
}

TEST(test_parse_jsx, attribute_checking_ignores_namespaced_attributes) {
  {
    padded_string code(u8R"(c = <div ns:onmouseenter={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(
        u8R"(c = <div onmouseenter:onmouseenter={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8R"(c = <div class:class="my-css-class" />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse_jsx, attribute_checking_ignores_namespaced_elements) {
  {
    padded_string code(u8R"(c = <svg:g onmouseenter={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8R"(c = <svg:g class="red" />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }
}

TEST(test_parse_jsx, attribute_checking_ignores_user_components) {
  {
    padded_string code(u8R"(c = <MyComponent onmouseenter={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8R"(c = <MyComponent class="red" />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(
        u8R"(c = <mymodule.mycomponent onmouseenter={handler} />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
  }

  {
    padded_string code(u8R"(c = <mymodule.mycomponent class="red" />;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty());
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
