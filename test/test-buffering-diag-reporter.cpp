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
using ::testing::ElementsAreArray;

namespace quick_lint_js {
namespace {
TEST(test_buffering_diag_reporter, buffers_all_visits) {
  padded_string let_code(u8"let"_sv);
  padded_string expression_code(u8"2+2==5"_sv);

  linked_bump_allocator<alignof(void*)> memory("test");
  buffering_diag_reporter diag_reporter(&memory);
  diag_reporter.report(diag_let_with_no_bindings{.where = span_of(let_code)});
  diag_reporter.report(diag_expected_parenthesis_around_if_condition{
      .where = span_of(expression_code),
      .token = u8'(',
  });

  diag_collector collector;
  diag_reporter.move_into(&collector);
  EXPECT_THAT(
      collector.errors,
      ElementsAre(DIAG_TYPE_FIELD(diag_let_with_no_bindings, where,
                                  source_code_span_matcher(span_of(let_code))),
                  DIAG_TYPE_2_FIELDS(
                      diag_expected_parenthesis_around_if_condition, where,
                      source_code_span_matcher(span_of(expression_code)),  //
                      token, u8'(')));
}

TEST(test_buffering_diag_reporter, not_destructing_does_not_leak) {
  // This test relies on a leak checker such as Valgrind's memtest or
  // Clang's LeakSanitizer.

  linked_bump_allocator<alignof(void*)> memory("test");
  alignas(buffering_diag_reporter)
      std::byte diag_reporter_storage[sizeof(buffering_diag_reporter)];
  buffering_diag_reporter* diag_reporter =
      new (&diag_reporter_storage) buffering_diag_reporter(&memory);

  padded_string let_code(u8"let"_sv);
  diag_reporter->report(diag_let_with_no_bindings{.where = span_of(let_code)});

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
