// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cli-location.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/error.h>
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
                            &code, error_jsx_not_yet_implemented,  //
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
                            &code, error_jsx_not_yet_implemented,  //
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
    EXPECT_THAT(v.errors,
                ElementsAre(ERROR_TYPE_OFFSETS(
                    &code, error_missing_dots_for_attribute_spread,  //
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
                    &code, error_mismatched_jsx_tags,              //
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
                              &code, error_mismatched_jsx_tags,  //
                              opening_tag_name, strlen(u8"c = < "), u8"div")));
  }

  // opening_tag_name span for fragment tag:
  {
    padded_string code(u8"c = <  ></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, error_mismatched_jsx_tags,  //
                              opening_tag_name, strlen(u8"c = <"), u8"")));
  }

  // opening_tag_name span for member tag:
  {
    padded_string code(u8"c = < module . Component ></span>;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, error_mismatched_jsx_tags,  //
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
                    &code, error_mismatched_jsx_tags,  //
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
                    &code, error_mismatched_jsx_tags,  //
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
                    &code, error_mismatched_jsx_tags,  //
                    closing_tag_name, strlen(u8"c = <div></  "), u8"")));
  }

  // closing_tag_name span for member tag:
  {
    padded_string code(u8"c = <div></ module . Component >;"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE_OFFSETS(
                              &code, error_mismatched_jsx_tags,  //
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
                              &code, error_mismatched_jsx_tags,  //
                              closing_tag_name, strlen(u8"c = <div></ "),
                              u8"svg : path")));
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
    EXPECT_THAT(v.errors, ElementsAre(ERROR_TYPE(error_mismatched_jsx_tags)));
  }
}

TEST(test_parse_jsx, begin_and_end_tags_match_after_normalization) {
  {
    padded_string code(u8R"(c = <div></\u{64}\u{69}\u{76}>;)"_sv);
    spy_visitor v;
    parser p(&code, &v, jsx_options);
    p.parse_and_visit_module(v);
    EXPECT_THAT(v.errors, IsEmpty())
        << "shouldn't report error_mismatched_jsx_tags";
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
