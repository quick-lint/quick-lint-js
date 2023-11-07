// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/fe/buffering-visitor.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/spy-visitor.h>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(Test_Buffering_Visitor, buffers_all_visits) {
  const Char8 delete_keyword[] = u8"delete";
  const Char8 function_name[] = u8"function";
  const Char8 property_name[] = u8"property";
  const Char8 variable_name[] = u8"variable";

  Buffering_Visitor v(new_delete_resource());
  v.visit_end_of_module();
  v.visit_enter_block_scope();
  v.visit_enter_with_scope();
  v.visit_enter_class_scope();
  v.visit_enter_class_scope_body(std::nullopt);
  v.visit_enter_class_scope_body(identifier_of(variable_name));
  v.visit_enter_enum_scope();
  v.visit_enter_for_scope();
  v.visit_enter_named_function_scope(identifier_of(function_name));
  v.visit_enter_function_scope();
  v.visit_enter_function_scope_body();
  v.visit_enter_index_signature_scope();
  v.visit_enter_interface_scope();
  v.visit_exit_block_scope();
  v.visit_exit_with_scope();
  v.visit_exit_class_scope();
  v.visit_exit_enum_scope();
  v.visit_exit_for_scope();
  v.visit_exit_function_scope();
  v.visit_exit_index_signature_scope();
  v.visit_exit_interface_scope();
  v.visit_keyword_variable_use(identifier_of(variable_name));
  v.visit_property_declaration(std::nullopt);
  v.visit_property_declaration(identifier_of(property_name));
  v.visit_variable_assignment(identifier_of(variable_name));
  v.visit_variable_declaration(
      identifier_of(variable_name), Variable_Kind::_var,
      Variable_Declaration_Flags::initialized_with_equals);
  v.visit_variable_assertion_signature_use(identifier_of(variable_name));
  v.visit_variable_delete_use(identifier_of(variable_name),
                              span_of(delete_keyword));
  v.visit_variable_export_use(identifier_of(variable_name));
  v.visit_variable_type_predicate_use(identifier_of(variable_name));
  v.visit_variable_type_use(identifier_of(variable_name));
  v.visit_variable_typeof_use(identifier_of(variable_name));
  v.visit_variable_use(identifier_of(variable_name));

  Spy_Visitor spy;
  v.move_into(spy);
  EXPECT_THAT(spy.visits, ElementsAreArray({
                              "visit_end_of_module",                     //
                              "visit_enter_block_scope",                 //
                              "visit_enter_with_scope",                  //
                              "visit_enter_class_scope",                 //
                              "visit_enter_class_scope_body",            //
                              "visit_enter_class_scope_body",            //
                              "visit_enter_enum_scope",                  //
                              "visit_enter_for_scope",                   //
                              "visit_enter_named_function_scope",        //
                              "visit_enter_function_scope",              //
                              "visit_enter_function_scope_body",         //
                              "visit_enter_index_signature_scope",       //
                              "visit_enter_interface_scope",             //
                              "visit_exit_block_scope",                  //
                              "visit_exit_with_scope",                   //
                              "visit_exit_class_scope",                  //
                              "visit_exit_enum_scope",                   //
                              "visit_exit_for_scope",                    //
                              "visit_exit_function_scope",               //
                              "visit_exit_index_signature_scope",        //
                              "visit_exit_interface_scope",              //
                              "visit_keyword_variable_use",              //
                              "visit_property_declaration",              //
                              "visit_property_declaration",              //
                              "visit_variable_assignment",               //
                              "visit_variable_declaration",              //
                              "visit_variable_assertion_signature_use",  //
                              "visit_variable_delete_use",               //
                              "visit_variable_export_use",               //
                              "visit_variable_type_predicate_use",       //
                              "visit_variable_type_use",                 //
                              "visit_variable_typeof_use",               //
                              "visit_variable_use",                      //
                          }));
}

TEST(Test_Buffering_Visitor, copy_to_repeats_buffered_visits) {
  Buffering_Visitor v(new_delete_resource());
  v.visit_end_of_module();

  Spy_Visitor spy_1;
  v.copy_into(spy_1);
  EXPECT_THAT(spy_1.visits, ElementsAreArray({"visit_end_of_module"}));

  Spy_Visitor spy_2;
  v.copy_into(spy_2);
  EXPECT_THAT(spy_2.visits, ElementsAreArray({"visit_end_of_module"}));
}

TEST(Test_Buffering_Visitor, move_to_clears_buffered_visits) {
  Buffering_Visitor v(new_delete_resource());
  v.visit_end_of_module();

  Spy_Visitor spy_1;
  v.move_into(spy_1);
  EXPECT_THAT(spy_1.visits, ElementsAreArray({"visit_end_of_module"}));

  Spy_Visitor spy_2;
  v.move_into(spy_2);
  EXPECT_THAT(spy_2.visits, IsEmpty());
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
