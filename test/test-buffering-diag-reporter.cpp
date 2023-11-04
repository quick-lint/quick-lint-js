// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/container/linked-bump-allocator.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/buffering-diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/identifier-support.h>
#include <type_traits>

namespace quick_lint_js {
namespace {
TEST(Test_Buffering_Diag_Reporter, buffers_all_visits) {
  static Padded_String let_code(u8"let"_sv);
  static Padded_String expression_code(u8"2+2==5"_sv);

  Linked_Bump_Allocator memory("test");
  Buffering_Diag_Reporter diag_reporter(&memory);
  diag_reporter.report(Diag_Let_With_No_Bindings{.where = span_of(let_code)});
  diag_reporter.report(Diag_Expected_Parenthesis_Around_If_Condition{
      .where = span_of(expression_code),
      .token = u8'(',
  });

  struct Test_Diag_Reporter : public Diag_Reporter {
    void report_impl(Diag_Type type, void* diag) override {
      this->report_count += 1;
      switch (this->report_count) {
      case 1: {
        ASSERT_EQ(type, Diag_Type::Diag_Let_With_No_Bindings);
        const auto* d = static_cast<const Diag_Let_With_No_Bindings*>(diag);
        EXPECT_TRUE(same_pointers(d->where, span_of(let_code)));
        break;
      }
      case 2: {
        ASSERT_EQ(type,
                  Diag_Type::Diag_Expected_Parenthesis_Around_If_Condition);
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
    }
    int report_count = 0;
  };
  Test_Diag_Reporter test;
  diag_reporter.move_into(&test);
  EXPECT_EQ(test.report_count, 2);
}

TEST(Test_Buffering_Diag_Reporter, not_destructing_does_not_leak) {
  // This test relies on a leak checker such as Valgrind's memtest or
  // Clang's LeakSanitizer.

  Linked_Bump_Allocator memory("test");
  alignas(Buffering_Diag_Reporter)
      std::byte diag_reporter_storage[sizeof(Buffering_Diag_Reporter)];
  Buffering_Diag_Reporter* diag_reporter =
      new (&diag_reporter_storage) Buffering_Diag_Reporter(&memory);

  Padded_String let_code(u8"let"_sv);
  diag_reporter->report(Diag_Let_With_No_Bindings{.where = span_of(let_code)});

  // Destruct memory, but don't destruct *diag_reporter.
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
