// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/fe/expression.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <string>

namespace quick_lint_js {
String8 escape_first_character_in_keyword(String8_View keyword) {
  constexpr Char8 alphabet[] = u8"0123456789abcdef";
  String8 result;
  std::size_t expected_size = keyword.size() + 6 - 1;
  result.reserve(expected_size);
  result += u8"\\u{";
  result += alphabet[(keyword[0] >> 4) & 0xf];
  result += alphabet[(keyword[0] >> 0) & 0xf];
  result += u8'}';
  result += keyword.substr(1);
  QLJS_ASSERT(result.size() == expected_size);
  return result;
}

void summarize(const Expression& expression, std::string& out) {
  auto children = [&] {
    bool need_comma = false;
    for (int i = 0; i < expression.child_count(); ++i) {
      if (need_comma) {
        out += ", ";
      }
      summarize(expression.child(i), out);
      need_comma = true;
    }
  };
  auto function_attributes = [&]() -> std::string_view {
    switch (expression.attributes()) {
    case Function_Attributes::normal:
      return ""sv;
    case Function_Attributes::async:
      return "async"sv;
    case Function_Attributes::async_generator:
      return "asyncgenerator"sv;
    case Function_Attributes::generator:
      return "generator"sv;
    }
    QLJS_UNREACHABLE();
  };
  switch (expression.kind()) {
  case Expression_Kind::Class:
    out += "class";
    break;
  case Expression_Kind::Delete:
    out += "delete(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Invalid:
    out += "invalid";
    break;
  case Expression_Kind::Missing:
    out += "missing";
    break;
  case Expression_Kind::New:
    out += "new(";
    children();
    out += ")";
    break;
  case Expression_Kind::Template:
    out += "template(";
    children();
    out += ")";
    break;
  case Expression_Kind::Typeof:
    out += "typeof(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Angle_Type_Assertion:
    out += "typeassert(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Array:
    out += "array(";
    children();
    out += ")";
    break;
  case Expression_Kind::Arrow_Function:
    out += function_attributes();
    out += "arrowfunc(";
    children();
    out += ")";
    break;
  case Expression_Kind::As_Type_Assertion:
    out += "as(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Assignment:
    out += "assign(";
    children();
    out += ")";
    break;
  case Expression_Kind::Await:
    out += "await(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Call:
    out += "call(";
    children();
    out += ")";
    break;
  case Expression_Kind::Conditional:
    out += "cond(";
    summarize(expression.child_0(), out);
    out += ", ";
    summarize(expression.child_1(), out);
    out += ", ";
    summarize(expression.child_2(), out);
    out += ")";
    break;
  case Expression_Kind::Dot:
    out += "dot(";
    summarize(expression.child_0(), out);
    out += ", ";
    out += to_string_view(expression.variable_identifier().normalized_name());
    out += ")";
    break;
  case Expression_Kind::Function:
    out += "function";
    break;
  case Expression_Kind::Import:
    out += "import";
    break;
  case Expression_Kind::Index:
    out += "index(";
    children();
    out += ")";
    break;
  case Expression_Kind::JSX_Element: {
    const auto& jsx =
        static_cast<const quick_lint_js::Expression::JSX_Element&>(expression);
    out += "jsxelement(";
    out += to_string_view(jsx.tag.normalized_name());
    if (jsx.child_count() != 0) {
      out += ", ";
      children();
    }
    out += ")";
    break;
  }
  case Expression_Kind::JSX_Element_With_Members: {
    const auto& jsx =
        static_cast<const quick_lint_js::Expression::JSX_Element_With_Members&>(
            expression);
    out += "jsxmemberelement((";
    bool need_comma = false;
    for (int i = 0; i < jsx.members.size(); ++i) {
      if (need_comma) {
        out += ", ";
      }
      out += to_string_view(jsx.members[i].normalized_name());
      need_comma = true;
    }
    out += ")";

    if (jsx.child_count() != 0) {
      out += ", ";
      children();
    }
    out += ")";
    break;
  }
  case Expression_Kind::JSX_Element_With_Namespace: {
    const auto& jsx = static_cast<
        const quick_lint_js::Expression::JSX_Element_With_Namespace&>(
        expression);
    out += "jsxnselement(";
    out += to_string_view(jsx.ns.normalized_name());
    out += ", ";
    out += to_string_view(jsx.tag.normalized_name());
    if (jsx.child_count() != 0) {
      out += ", ";
      children();
    }
    out += ")";
    break;
  }
  case Expression_Kind::JSX_Fragment:
    out += "jsxfragment(";
    children();
    out += ")";
    break;
  case Expression_Kind::Literal:
    out += "literal";
    break;
  case Expression_Kind::Named_Function:
    out += "function ";
    out += to_string_view(expression.variable_identifier().normalized_name());
    break;
  case Expression_Kind::New_Target:
    out += "newtarget";
    break;
  case Expression_Kind::Non_Null_Assertion:
    out += "nonnull(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Object: {
    out += "object(";
    bool need_comma = false;
    for (int i = 0; i < expression.object_entry_count(); ++i) {
      if (need_comma) {
        out += ", ";
      }
      auto entry = expression.object_entry(i);
      if (entry.property) {
        summarize(entry.property, out);
        out += ": ";
      }
      summarize(entry.value, out);
      if (entry.init) {
        out += " = ";
        summarize(entry.init, out);
      }
      need_comma = true;
    }
    out += ")";
    break;
  }
  case Expression_Kind::Optional:
    out += "optional(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Paren:
    out += "paren(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Paren_Empty:
    out += "parenempty";
    break;
  case Expression_Kind::Private_Variable:
    out += "privatevar ";
    out += to_string_view(expression.variable_identifier().normalized_name());
    break;
  case Expression_Kind::RW_Unary_Prefix:
    out += "rwunary(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::RW_Unary_Suffix:
    out += "rwunarysuffix(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Satisfies:
    out += "satisfies(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Spread:
    out += "spread(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Super:
    out += "super";
    break;
  case Expression_Kind::Tagged_Template_Literal:
    out += "taggedtemplate(";
    children();
    out += ")";
    break;
  case Expression_Kind::This_Variable:
    out += "this";
    break;
  case Expression_Kind::Trailing_Comma:
    out += "trailingcomma(";
    children();
    out += ")";
    break;
  case Expression_Kind::Type_Annotated:
    out += "typed(";
    children();
    out += ")";
    break;
  case Expression_Kind::Unary_Operator:
    out += "unary(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Compound_Assignment:
    out += "upassign(";
    children();
    out += ")";
    break;
  case Expression_Kind::Conditional_Assignment:
    out += "condassign(";
    children();
    out += ")";
    break;
  case Expression_Kind::Variable:
    out += "var ";
    out += to_string_view(expression.variable_identifier().normalized_name());
    break;
  case Expression_Kind::Binary_Operator:
    out += "binary(";
    children();
    out += ")";
    break;
  case Expression_Kind::Yield_Many:
    out += "yieldmany(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case Expression_Kind::Yield_None:
    out += "yieldnone";
    break;
  case Expression_Kind::Yield_One:
    out += "yield(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  }
}

void summarize(Expression* expression, std::string& out) {
  return summarize(*expression, out);
}

std::string summarize(Expression* expression) {
  std::string result;
  // At the time of writing, the biggest string in practice is 84 bytes. Let's
  // reserve more than that to avoid string copies.
  result.reserve(128);
  summarize(expression, result);
  return result;
}

std::string summarize(std::optional<Expression*> expression) {
  if (expression.has_value()) {
    return summarize(*expression);
  } else {
    return "(null)";
  }
}

void Test_Parser::assert_diagnostics(Span<const Diagnostic_Assertion> diags,
                                     Source_Location caller) {
  quick_lint_js::assert_diagnostics(this->code, this->errors_.errors, diags,
                                    caller);
}

Spy_Visitor test_parse_and_visit_statement(String8_View input, No_Diags_Tag,
                                           Parser_Options options,
                                           Source_Location caller) {
  return test_parse_and_visit_statement(
      input, Span<const Diagnostic_Assertion>(), options, caller);
}

Spy_Visitor test_parse_and_visit_statement(String8_View input,
                                           Diagnostic_Assertion diag0,
                                           Parser_Options options,
                                           Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0};
  return test_parse_and_visit_statement(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_statement(String8_View input,
                                           Diagnostic_Assertion diag0,
                                           Diagnostic_Assertion diag1,
                                           Parser_Options options,
                                           Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1};
  return test_parse_and_visit_statement(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_statement(String8_View input,
                                           Diagnostic_Assertion diag0,
                                           Diagnostic_Assertion diag1,
                                           Diagnostic_Assertion diag2,
                                           Parser_Options options,
                                           Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1, diag2};
  return test_parse_and_visit_statement(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_statement(
    String8_View input, Diagnostic_Assertion diag0, Diagnostic_Assertion diag1,
    Diagnostic_Assertion diag2, Diagnostic_Assertion diag3,
    Diagnostic_Assertion diag4, Diagnostic_Assertion diag5,
    Diagnostic_Assertion diag6, Diagnostic_Assertion diag7,
    Diagnostic_Assertion diag8, Parser_Options options,
    Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1, diag2, diag3, diag4,
                                       diag5, diag6, diag7, diag8};
  return test_parse_and_visit_statement(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_statement(
    String8_View input, Span<const Diagnostic_Assertion> diags,
    Parser_Options options, Source_Location caller) {
  SCOPED_TRACE(out_string8(input));
  Test_Parser p(input, options, capture_diags);
  p.parse_and_visit_statement();
  p.assert_diagnostics(diags, caller);
  return p.spy_visitor();
}

Spy_Visitor test_parse_and_visit_module(String8_View input, No_Diags_Tag,
                                        Parser_Options options,
                                        Source_Location caller) {
  return test_parse_and_visit_module(input, Span<const Diagnostic_Assertion>(),
                                     options, caller);
}

Spy_Visitor test_parse_and_visit_module(String8_View input,
                                        Diagnostic_Assertion diag0,
                                        Parser_Options options,
                                        Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0};
  return test_parse_and_visit_module(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_module(String8_View input,
                                        Diagnostic_Assertion diag0,
                                        Diagnostic_Assertion diag1,
                                        Parser_Options options,
                                        Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1};
  return test_parse_and_visit_module(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_module(String8_View input,
                                        Diagnostic_Assertion diag0,
                                        Diagnostic_Assertion diag1,
                                        Diagnostic_Assertion diag2,
                                        Parser_Options options,
                                        Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1, diag2};
  return test_parse_and_visit_module(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_module(String8_View input,
                                        Span<const Diagnostic_Assertion> diags,
                                        Parser_Options options,
                                        Source_Location caller) {
  SCOPED_TRACE(out_string8(input));
  Test_Parser p(input, options, capture_diags);
  p.parse_and_visit_module();
  p.assert_diagnostics(diags, caller);
  return p.spy_visitor();
}

Spy_Visitor test_parse_and_visit_expression(String8_View input, No_Diags_Tag,
                                            Parser_Options options,
                                            Source_Location caller) {
  return test_parse_and_visit_expression(
      input, Span<const Diagnostic_Assertion>(), options, caller);
}

Spy_Visitor test_parse_and_visit_expression(String8_View input,
                                            Diagnostic_Assertion diag0,
                                            Parser_Options options,
                                            Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0};
  return test_parse_and_visit_expression(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_expression(String8_View input,
                                            Diagnostic_Assertion diag0,
                                            Diagnostic_Assertion diag1,
                                            Parser_Options options,
                                            Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1};
  return test_parse_and_visit_expression(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_expression(String8_View input,
                                            Diagnostic_Assertion diag0,
                                            Diagnostic_Assertion diag1,
                                            Diagnostic_Assertion diag2,
                                            Parser_Options options,
                                            Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1, diag2};
  return test_parse_and_visit_expression(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_expression(
    String8_View input, Span<const Diagnostic_Assertion> diags,
    Parser_Options options, Source_Location caller) {
  Test_Parser p(input, options, capture_diags);
  p.parse_and_visit_expression();
  p.assert_diagnostics(diags, caller);
  return p.spy_visitor();
}

Spy_Visitor test_parse_and_visit_typescript_type_expression(
    String8_View input, No_Diags_Tag, Parser_Options options,
    Source_Location caller) {
  return test_parse_and_visit_typescript_type_expression(
      input, Span<const Diagnostic_Assertion>(), options, caller);
}

Spy_Visitor test_parse_and_visit_typescript_type_expression(
    String8_View input, Diagnostic_Assertion diag0, Parser_Options options,
    Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0};
  return test_parse_and_visit_typescript_type_expression(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_typescript_type_expression(
    String8_View input, Diagnostic_Assertion diag0, Diagnostic_Assertion diag1,
    Parser_Options options, Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1};
  return test_parse_and_visit_typescript_type_expression(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_typescript_type_expression(
    String8_View input, Diagnostic_Assertion diag0, Diagnostic_Assertion diag1,
    Diagnostic_Assertion diag2, Parser_Options options,
    Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1, diag2};
  return test_parse_and_visit_typescript_type_expression(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_typescript_type_expression(
    String8_View input, Span<const Diagnostic_Assertion> diags,
    Parser_Options options, Source_Location caller) {
  Test_Parser p(input, options, capture_diags);
  p.parse_and_visit_typescript_type_expression();
  p.assert_diagnostics(diags, caller);
  return p.spy_visitor();
}

Spy_Visitor test_parse_and_visit_typescript_generic_parameters(
    String8_View input, No_Diags_Tag, Parser_Options options,
    Source_Location caller) {
  return test_parse_and_visit_typescript_generic_parameters(
      input, Span<const Diagnostic_Assertion>(), options, caller);
}

Spy_Visitor test_parse_and_visit_typescript_generic_parameters(
    String8_View input, Diagnostic_Assertion diag0, Parser_Options options,
    Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0};
  return test_parse_and_visit_typescript_generic_parameters(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_typescript_generic_parameters(
    String8_View input, Diagnostic_Assertion diag0, Diagnostic_Assertion diag1,
    Parser_Options options, Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1};
  return test_parse_and_visit_typescript_generic_parameters(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_typescript_generic_parameters(
    String8_View input, Diagnostic_Assertion diag0, Diagnostic_Assertion diag1,
    Diagnostic_Assertion diag2, Parser_Options options,
    Source_Location caller) {
  Diagnostic_Assertion assertions[] = {diag0, diag1, diag2};
  return test_parse_and_visit_typescript_generic_parameters(
      input, Span<const Diagnostic_Assertion>(assertions), options, caller);
}

Spy_Visitor test_parse_and_visit_typescript_generic_parameters(
    String8_View input, Span<const Diagnostic_Assertion> diags,
    Parser_Options options, Source_Location caller) {
  Test_Parser p(input, options, capture_diags);
  p.parse_and_visit_typescript_generic_parameters();
  p.assert_diagnostics(diags, caller);
  return p.spy_visitor();
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
