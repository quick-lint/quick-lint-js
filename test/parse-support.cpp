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
string8 escape_first_character_in_keyword(string8_view keyword) {
  constexpr char8 alphabet[] = u8"0123456789abcdef";
  string8 result;
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

void summarize(const expression& expression, std::string& out) {
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
    case function_attributes::normal:
      return ""sv;
    case function_attributes::async:
      return "async"sv;
    case function_attributes::async_generator:
      return "asyncgenerator"sv;
    case function_attributes::generator:
      return "generator"sv;
    }
    QLJS_UNREACHABLE();
  };
  switch (expression.kind()) {
  case expression_kind::_class:
    out += "class";
    break;
  case expression_kind::_delete:
    out += "delete(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::_invalid:
    out += "invalid";
    break;
  case expression_kind::_missing:
    out += "missing";
    break;
  case expression_kind::_new:
    out += "new(";
    children();
    out += ")";
    break;
  case expression_kind::_template:
    out += "template(";
    children();
    out += ")";
    break;
  case expression_kind::_typeof:
    out += "typeof(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::angle_type_assertion:
    out += "typeassert(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::array:
    out += "array(";
    children();
    out += ")";
    break;
  case expression_kind::arrow_function:
    out += function_attributes();
    out += "arrowfunc(";
    children();
    out += ")";
    break;
  case expression_kind::as_type_assertion:
    out += "as(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::assignment:
    out += "assign(";
    children();
    out += ")";
    break;
  case expression_kind::await:
    out += "await(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::call:
    out += "call(";
    children();
    out += ")";
    break;
  case expression_kind::conditional:
    out += "cond(";
    summarize(expression.child_0(), out);
    out += ", ";
    summarize(expression.child_1(), out);
    out += ", ";
    summarize(expression.child_2(), out);
    out += ")";
    break;
  case expression_kind::dot:
    out += "dot(";
    summarize(expression.child_0(), out);
    out += ", ";
    out += to_string_view(expression.variable_identifier().normalized_name());
    out += ")";
    break;
  case expression_kind::function:
    out += "function";
    break;
  case expression_kind::import:
    out += "import";
    break;
  case expression_kind::index:
    out += "index(";
    children();
    out += ")";
    break;
  case expression_kind::jsx_element: {
    const auto& jsx =
        static_cast<const quick_lint_js::expression::jsx_element&>(expression);
    out += "jsxelement(";
    out += to_string_view(jsx.tag.normalized_name());
    if (jsx.child_count() != 0) {
      out += ", ";
      children();
    }
    out += ")";
    break;
  }
  case expression_kind::jsx_element_with_members: {
    const auto& jsx =
        static_cast<const quick_lint_js::expression::jsx_element_with_members&>(
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
  case expression_kind::jsx_element_with_namespace: {
    const auto& jsx = static_cast<
        const quick_lint_js::expression::jsx_element_with_namespace&>(
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
  case expression_kind::jsx_fragment:
    out += "jsxfragment(";
    children();
    out += ")";
    break;
  case expression_kind::literal:
    out += "literal";
    break;
  case expression_kind::named_function:
    out += "function ";
    out += to_string_view(expression.variable_identifier().normalized_name());
    break;
  case expression_kind::new_target:
    out += "newtarget";
    break;
  case expression_kind::non_null_assertion:
    out += "nonnull(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::object: {
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
  case expression_kind::optional:
    out += "optional(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::paren:
    out += "paren(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::paren_empty:
    out += "parenempty";
    break;
  case expression_kind::private_variable:
    out += "var ";
    out += to_string_view(expression.variable_identifier().normalized_name());
    break;
  case expression_kind::rw_unary_prefix:
    out += "rwunary(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::rw_unary_suffix:
    out += "rwunarysuffix(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::spread:
    out += "spread(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::super:
    out += "super";
    break;
  case expression_kind::tagged_template_literal:
    out += "taggedtemplate(";
    children();
    out += ")";
    break;
  case expression_kind::this_variable:
    out += "this";
    break;
  case expression_kind::trailing_comma:
    out += "trailingcomma(";
    children();
    out += ")";
    break;
  case expression_kind::type_annotated:
    out += "typed(";
    children();
    out += ")";
    break;
  case expression_kind::unary_operator:
    out += "unary(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::compound_assignment:
    out += "upassign(";
    children();
    out += ")";
    break;
  case expression_kind::conditional_assignment:
    out += "condassign(";
    children();
    out += ")";
    break;
  case expression_kind::variable:
    out += "var ";
    out += to_string_view(expression.variable_identifier().normalized_name());
    break;
  case expression_kind::binary_operator:
    out += "binary(";
    children();
    out += ")";
    break;
  case expression_kind::yield_many:
    out += "yieldmany(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  case expression_kind::yield_none:
    out += "yieldnone";
    break;
  case expression_kind::yield_one:
    out += "yield(";
    summarize(expression.child_0(), out);
    out += ")";
    break;
  }
}

void summarize(expression* expression, std::string& out) {
  return summarize(*expression, out);
}

std::string summarize(expression* expression) {
  std::string result;
  // At the time of writing, the biggest string in practice is 84 bytes. Let's
  // reserve more than that to avoid string copies.
  result.reserve(128);
  summarize(expression, result);
  return result;
}

std::string summarize(std::optional<expression*> expression) {
  if (expression.has_value()) {
    return summarize(*expression);
  } else {
    return "(null)";
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
