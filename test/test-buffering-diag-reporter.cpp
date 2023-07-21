// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
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

using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
TEST(Test_Buffering_Diag_Reporter, buffers_all_visits) {
  Padded_String let_code(u8"let"_sv);
  Padded_String expression_code(u8"2+2==5"_sv);

  Linked_Bump_Allocator<alignof(void*)> memory("test");
  Buffering_Diag_Reporter diag_reporter(&memory);
  diag_reporter.report(Diag_Let_With_No_Bindings{.where = span_of(let_code)});
  diag_reporter.report(Diag_Expected_Parenthesis_Around_If_Condition{
      .where = span_of(expression_code),
      .token = u8'(',
  });

  Diag_Collector collector;
  diag_reporter.move_into(&collector);
  EXPECT_THAT(
      collector.errors,
      ElementsAre(DIAG_TYPE_FIELD(Diag_Let_With_No_Bindings, where,
                                  Source_Code_Span_Matcher(span_of(let_code))),
                  DIAG_TYPE_2_FIELDS(
                      Diag_Expected_Parenthesis_Around_If_Condition, where,
                      Source_Code_Span_Matcher(span_of(expression_code)),  //
                      token, u8'(')));
}

TEST(Test_Buffering_Diag_Reporter, not_destructing_does_not_leak) {
  // This test relies on a leak checker such as Valgrind's memtest or
  // Clang's LeakSanitizer.

  Linked_Bump_Allocator<alignof(void*)> memory("test");
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
