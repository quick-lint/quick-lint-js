// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/spy-visitor.h>

using boost::container::pmr::new_delete_resource;
using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
TEST(test_buffering_visitor, buffers_all_visits) {
  const char8 delete_keyword[] = u8"delete";
  const char8 function_name[] = u8"function";
  const char8 property_name[] = u8"property";
  const char8 variable_name[] = u8"variable";

  buffering_visitor v(new_delete_resource());
  v.visit_end_of_module();
  v.visit_enter_block_scope();
  v.visit_enter_with_scope();
  v.visit_enter_class_scope();
  v.visit_enter_for_scope();
  v.visit_enter_named_function_scope(identifier_of(function_name));
  v.visit_enter_function_scope();
  v.visit_enter_function_scope_body();
  v.visit_exit_block_scope();
  v.visit_exit_with_scope();
  v.visit_exit_class_scope();
  v.visit_exit_for_scope();
  v.visit_exit_function_scope();
  v.visit_keyword_variable_use(identifier_of(variable_name));
  v.visit_property_declaration(std::nullopt);
  v.visit_property_declaration(identifier_of(property_name));
  v.visit_variable_assignment(identifier_of(variable_name));
  v.visit_variable_declaration(identifier_of(variable_name),
                               variable_kind::_var,
                               variable_init_kind::initialized_with_equals);
  v.visit_variable_delete_use(identifier_of(variable_name),
                              span_of(delete_keyword));
  v.visit_variable_export_use(identifier_of(variable_name));
  v.visit_variable_typeof_use(identifier_of(variable_name));
  v.visit_variable_use(identifier_of(variable_name));

  spy_visitor spy;
  v.move_into(spy);
  EXPECT_THAT(spy.visits,
              ElementsAre("visit_end_of_module",               //
                          "visit_enter_block_scope",           //
                          "visit_enter_with_scope",            //
                          "visit_enter_class_scope",           //
                          "visit_enter_for_scope",             //
                          "visit_enter_named_function_scope",  //
                          "visit_enter_function_scope",        //
                          "visit_enter_function_scope_body",   //
                          "visit_exit_block_scope",            //
                          "visit_exit_with_scope",             //
                          "visit_exit_class_scope",            //
                          "visit_exit_for_scope",              //
                          "visit_exit_function_scope",         //
                          "visit_keyword_variable_use",        //
                          "visit_property_declaration",        //
                          "visit_property_declaration",        //
                          "visit_variable_assignment",         //
                          "visit_variable_declaration",        //
                          "visit_variable_delete_use",         //
                          "visit_variable_export_use",         //
                          "visit_variable_typeof_use",         //
                          "visit_variable_use"));
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
