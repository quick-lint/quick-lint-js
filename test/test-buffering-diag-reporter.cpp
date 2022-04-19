// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/buffering-diag-reporter.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-matcher.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/linked-bump-allocator.h>
#include <quick-lint-js/padded-string.h>
#include <type_traits>

using ::testing::ElementsAre;

namespace quick_lint_js {
namespace {
source_code_span span_of(const padded_string& code) {
  return source_code_span(code.data(), code.null_terminator());
}

TEST(test_buffering_diag_reporter, buffers_all_visits) {
  padded_string let_code(u8"let"_sv);
  padded_string expression_code(u8"2+2==5"_sv);

  linked_bump_allocator<alignof(void*)> memory;
  buffering_diag_reporter diag_reporter(&memory);
  diag_reporter.report(diag_let_with_no_bindings{.where = span_of(let_code)});
  diag_reporter.report(diag_expected_parenthesis_around_if_condition{
      .where = span_of(expression_code),
      .token = u8'(',
  });

  error_collector collector;
  diag_reporter.move_into(&collector);
  EXPECT_THAT(
      collector.errors,
      ElementsAre(
          ERROR_TYPE_FIELD(diag_let_with_no_bindings, where, span_of(let_code)),
          ERROR_TYPE_2_FIELDS(diag_expected_parenthesis_around_if_condition,
                              where,
                              span_of(expression_code),  //
                              token, u8'(')));
}

TEST(test_buffering_diag_reporter, not_destructing_does_not_leak) {
  // This test relies on a leak checker such as Valgrind's memtest or
  // Clang's LeakSanitizer.

  linked_bump_allocator<alignof(void*)> memory;
  std::aligned_union_t<0, buffering_diag_reporter> error_reporter_storage;
  buffering_diag_reporter* diag_reporter =
      new (&error_reporter_storage) buffering_diag_reporter(&memory);

  padded_string let_code(u8"let"_sv);
  diag_reporter->report(diag_let_with_no_bindings{.where = span_of(let_code)});

  // Destruct memory, but don't destruct error_reporter_storage.diag_reporter.
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
