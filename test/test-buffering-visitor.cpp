// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/spy-visitor.h>

using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
template <std::size_t N>
source_code_span span_of(const char8 (&code)[N]) {
  return source_code_span(&code[0], &code[N]);
}

template <std::size_t N>
identifier identifier_of(const char8 (&name)[N]) {
  return identifier(span_of(name));
}

TEST(test_buffering_visitor, buffers_all_visits) {
  const char8 function_name[] = u8"function";
  const char8 property_name[] = u8"property";
  const char8 variable_name[] = u8"variable";

  buffering_visitor v;
  v.visit_end_of_module();
  v.visit_enter_block_scope();
  v.visit_enter_class_scope();
  v.visit_enter_for_scope();
  v.visit_enter_named_function_scope(identifier_of(function_name));
  v.visit_enter_function_scope();
  v.visit_exit_block_scope();
  v.visit_exit_class_scope();
  v.visit_exit_for_scope();
  v.visit_exit_function_scope();
  v.visit_property_declaration(identifier_of(property_name));
  v.visit_variable_assignment(identifier_of(variable_name));
  v.visit_variable_declaration(identifier_of(variable_name),
                               variable_kind::_var);
  v.visit_variable_use(identifier_of(variable_name));

  spy_visitor spy;
  v.move_into(spy);
  EXPECT_THAT(spy.visits,
              ElementsAre("visit_end_of_module",               //
                          "visit_enter_block_scope",           //
                          "visit_enter_class_scope",           //
                          "visit_enter_for_scope",             //
                          "visit_enter_named_function_scope",  //
                          "visit_enter_function_scope",        //
                          "visit_exit_block_scope",            //
                          "visit_exit_class_scope",            //
                          "visit_exit_for_scope",              //
                          "visit_exit_function_scope",         //
                          "visit_property_declaration",        //
                          "visit_variable_assignment",         //
                          "visit_variable_declaration",        //
                          "visit_variable_use"));
}
}
}
