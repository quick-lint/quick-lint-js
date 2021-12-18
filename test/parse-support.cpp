// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/expression.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/parse.h>
#include <string>

namespace quick_lint_js {
template void parser::parse_and_visit_module<spy_visitor>(spy_visitor& v);
template bool parser::parse_and_visit_statement<spy_visitor>(
    spy_visitor& v, parser::parse_statement_type);

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

std::string summarize(const expression& expression) {
  auto children = [&] {
    std::string result;
    bool need_comma = false;
    for (int i = 0; i < expression.child_count(); ++i) {
      if (need_comma) {
        result += ", ";
      }
      result += summarize(expression.child(i));
      need_comma = true;
    }
    return result;
  };
  auto function_attributes = [&]() -> std::string {
    switch (expression.attributes()) {
    case function_attributes::normal:
      return "";
    case function_attributes::async:
      return "async";
    case function_attributes::async_generator:
      return "asyncgenerator";
    case function_attributes::generator:
      return "generator";
    }
    QLJS_UNREACHABLE();
  };
  switch (expression.kind()) {
  case expression_kind::_class:
    return "class";
  case expression_kind::_delete:
    return "delete(" + summarize(expression.child_0()) + ")";
  case expression_kind::_invalid:
    return "invalid";
  case expression_kind::_missing:
    return "missing";
  case expression_kind::_new:
    return "new(" + children() + ")";
  case expression_kind::_template:
    return "template(" + children() + ")";
  case expression_kind::_typeof:
    return "typeof(" + summarize(expression.child_0()) + ")";
  case expression_kind::array:
    return "array(" + children() + ")";
  case expression_kind::arrow_function_with_expression: {
    auto& arrow =
        static_cast<const expression::arrow_function_with_expression&>(
            expression);
    std::string result = function_attributes() + "arrowexpr(" + children();
    if (arrow.child_count() != 0) {
      result += ", ";
    }
    result += summarize(arrow.body_) + ")";
    return result;
  }
  case expression_kind::arrow_function_with_statements:
    return function_attributes() + "arrowblock(" + children() + ")";
  case expression_kind::assignment:
    return "assign(" + children() + ")";
  case expression_kind::await:
    return "await(" + summarize(expression.child_0()) + ")";
  case expression_kind::call:
    return "call(" + children() + ")";
  case expression_kind::conditional:
    return "cond(" + summarize(expression.child_0()) + ", " +
           summarize(expression.child_1()) + ", " +
           summarize(expression.child_2()) + ")";
  case expression_kind::dot:
    return "dot(" + summarize(expression.child_0()) + ", " +
           to_string(expression.variable_identifier().normalized_name()) + ")";
  case expression_kind::function:
    return "function";
  case expression_kind::import:
    return "import";
  case expression_kind::index:
    return "index(" + children() + ")";
  case expression_kind::literal:
    return "literal";
  case expression_kind::named_function:
    return "function " +
           to_string(expression.variable_identifier().normalized_name());
  case expression_kind::new_target:
    return "newtarget";
  case expression_kind::object: {
    std::string result = "object(";
    bool need_comma = false;
    for (int i = 0; i < expression.object_entry_count(); ++i) {
      if (need_comma) {
        result += ", ";
      }
      auto entry = expression.object_entry(i);
      result += summarize(entry.property);
      result += ", ";
      result += summarize(entry.value);
      need_comma = true;
    }
    result += ")";
    return result;
  }
  case expression_kind::private_variable:
    return "var " +
           to_string(expression.variable_identifier().normalized_name());
  case expression_kind::rw_unary_prefix:
    return "rwunary(" + summarize(expression.child_0()) + ")";
  case expression_kind::rw_unary_suffix:
    return "rwunarysuffix(" + summarize(expression.child_0()) + ")";
  case expression_kind::spread:
    return "spread(" + summarize(expression.child_0()) + ")";
  case expression_kind::super:
    return "super";
  case expression_kind::tagged_template_literal:
    return "taggedtemplate(" + children() + ")";
  case expression_kind::trailing_comma:
    return "trailingcomma(" + children() + ")";
  case expression_kind::unary_operator:
    return "unary(" + summarize(expression.child_0()) + ")";
  case expression_kind::compound_assignment:
    return "upassign(" + children() + ")";
  case expression_kind::conditional_assignment:
    return "condassign(" + children() + ")";
  case expression_kind::variable:
    return "var " +
           to_string(expression.variable_identifier().normalized_name());
  case expression_kind::binary_operator:
    return "binary(" + children() + ")";
  case expression_kind::yield_many:
    return "yieldmany(" + summarize(expression.child_0()) + ")";
  case expression_kind::yield_none:
    return "yieldnone";
  case expression_kind::yield_one:
    return "yield(" + summarize(expression.child_0()) + ")";
  }
  QLJS_UNREACHABLE();
}

std::string summarize(expression* expression) { return summarize(*expression); }

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
