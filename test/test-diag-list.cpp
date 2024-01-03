// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/container/linked-bump-allocator.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/identifier-support.h>
#include <type_traits>

namespace quick_lint_js {
namespace {
TEST(Test_Diag_List, saves_all_data) {
  static Padded_String let_code(u8"let"_sv);
  static Padded_String expression_code(u8"2+2==5"_sv);

  Linked_Bump_Allocator memory("test");
  Diag_List diags(&memory);
  diags.add(Diag_Let_With_No_Bindings{.where = span_of(let_code)});
  diags.add(Diag_Expected_Parenthesis_Around_If_Condition{
      .where = span_of(expression_code),
      .token = u8'(',
  });

  int report_count = 0;
  diags.for_each([&](Diag_Type type, void* diag) -> void {
    report_count += 1;
    switch (report_count) {
    case 1: {
      ASSERT_EQ(type, Diag_Type::Diag_Let_With_No_Bindings);
      const auto* d = static_cast<const Diag_Let_With_No_Bindings*>(diag);
      EXPECT_TRUE(same_pointers(d->where, span_of(let_code)));
      break;
    }
    case 2: {
      ASSERT_EQ(type, Diag_Type::Diag_Expected_Parenthesis_Around_If_Condition);
      const auto* d =
          static_cast<const Diag_Expected_Parenthesis_Around_If_Condition*>(
              diag);
      EXPECT_TRUE(same_pointers(d->where, span_of(expression_code)));
      EXPECT_EQ(d->token, u8'(');
      break;
    }
    default:
      ADD_FAILURE() << "expected at most two calls to report_impl";
      break;
    }
  });
  EXPECT_EQ(report_count, 2);
}

TEST(Test_Diag_List, reported_any_diagnostic_except_diag_types) {
  static Padded_String code(u8"let"_sv);
  Linked_Bump_Allocator memory("test");
  Diag_List diags(&memory);
  Diag_List::Rewind_State begin_rewind_state = diags.prepare_for_rewind();

  EXPECT_FALSE(diags.reported_any_diagnostic_except_since(
      {
          Diag_Type::Diag_Assignment_Before_Variable_Declaration,
      },
      begin_rewind_state));
  diags.add(Diag_Assignment_Before_Variable_Declaration{
      .assignment = span_of(code),
      .declaration = span_of(code),
  });
  EXPECT_FALSE(diags.reported_any_diagnostic_except_since(
      {
          Diag_Type::Diag_Assignment_Before_Variable_Declaration,
      },
      begin_rewind_state));
  EXPECT_TRUE(diags.reported_any_diagnostic_except_since(
      {
          Diag_Type::Diag_Assignment_To_Const_Global_Variable,
      },
      begin_rewind_state));
  EXPECT_FALSE(diags.reported_any_diagnostic_except_since(
      {
          Diag_Type::Diag_Assignment_To_Const_Global_Variable,
          Diag_Type::Diag_Assignment_Before_Variable_Declaration,
      },
      begin_rewind_state));
  EXPECT_FALSE(diags.reported_any_diagnostic_except_since(
      {
          Diag_Type::Diag_Assignment_Before_Variable_Declaration,
          Diag_Type::Diag_Assignment_To_Const_Global_Variable,
      },
      begin_rewind_state));
}

TEST(Test_Diag_List,
     reported_any_diagnostic_except_diag_types_since_rewind_state) {
  static Padded_String code(u8"let"_sv);
  Linked_Bump_Allocator memory("test");
  Diag_List diags(&memory);
  diags.add(Diag_Assignment_Before_Variable_Declaration{
      .assignment = span_of(code),
      .declaration = span_of(code),
  });
  Diag_List::Rewind_State middle_rewind_state = diags.prepare_for_rewind();

  EXPECT_FALSE(diags.reported_any_diagnostic_except_since(
      {
          Diag_Type::Diag_Assignment_Before_Variable_Declaration,
      },
      middle_rewind_state));
  diags.add(Diag_Assignment_Before_Variable_Declaration{
      .assignment = span_of(code),
      .declaration = span_of(code),
  });
  EXPECT_FALSE(diags.reported_any_diagnostic_except_since(
      {
          Diag_Type::Diag_Assignment_Before_Variable_Declaration,
      },
      middle_rewind_state));
  EXPECT_TRUE(diags.reported_any_diagnostic_except_since(
      {
          Diag_Type::Diag_Assignment_To_Const_Global_Variable,
      },
      middle_rewind_state));
  EXPECT_FALSE(diags.reported_any_diagnostic_except_since(
      {
          Diag_Type::Diag_Assignment_To_Const_Global_Variable,
          Diag_Type::Diag_Assignment_Before_Variable_Declaration,
      },
      middle_rewind_state));
  EXPECT_FALSE(diags.reported_any_diagnostic_except_since(
      {
          Diag_Type::Diag_Assignment_Before_Variable_Declaration,
          Diag_Type::Diag_Assignment_To_Const_Global_Variable,
      },
      middle_rewind_state));
}

TEST(Test_Diag_List, not_destructing_does_not_leak) {
  // This test relies on a leak checker such as Valgrind's memtest or
  // Clang's LeakSanitizer.

  Linked_Bump_Allocator memory("test");
  alignas(Diag_List) std::byte diags_storage[sizeof(Diag_List)];
  Diag_List* diags = new (&diags_storage) Diag_List(&memory);

  Padded_String let_code(u8"let"_sv);
  diags->add(Diag_Let_With_No_Bindings{.where = span_of(let_code)});

  // Destruct memory, but don't destruct *diags.
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
