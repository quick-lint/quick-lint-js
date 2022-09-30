// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <csetjmp>
#include <gtest/gtest.h>
#include <quick-lint-js/fe/buffering-visitor-stack.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/tracking-memory-resource.h>

namespace quick_lint_js {
namespace {
// All of these tests fail if the buffering_visitor_stack leaks memory.

// NOTE[setjmp-in-tests]: setjmp requires that all local variables in the
// current function be marked 'volatile' if they are modified after the first
// call to setjmp. This is inconvenient for class objects (such as
// buffering_visitor_stack), so instead of marking variables as 'volatile' we
// use a lambda to make our mutated variables not technically local.

class test_buffering_visitor_stack : public ::testing::Test {
 protected:
  void TearDown() override {
    EXPECT_EQ(this->leak_detecting_memory_.alive_bytes(), 0)
        << "test should not have leaked memory";
  }

  tracking_memory_resource leak_detecting_memory_;
};

TEST_F(test_buffering_visitor_stack, empty) {
  buffering_visitor_stack stack(&this->leak_detecting_memory_);
}

TEST_F(test_buffering_visitor_stack, push_one_pop_one) {
  buffering_visitor_stack stack(&this->leak_detecting_memory_);
  {
    stacked_buffering_visitor v = stack.push();
    // pop
  }
}

TEST_F(test_buffering_visitor_stack, push_two_pop_two) {
  buffering_visitor_stack stack(&this->leak_detecting_memory_);
  {
    stacked_buffering_visitor outer_v = stack.push();
    {
      stacked_buffering_visitor inner_v = stack.push();
      // pop
    }
    // pop
  }
}

TEST_F(test_buffering_visitor_stack, push_pop_push_pop) {
  buffering_visitor_stack stack(&this->leak_detecting_memory_);
  {
    stacked_buffering_visitor v = stack.push();
    // pop
  }
  {
    stacked_buffering_visitor v = stack.push();
    // pop
  }
}

TEST_F(test_buffering_visitor_stack, longjmp_around_pop) {
  buffering_visitor_stack stack(&this->leak_detecting_memory_);

  bool pushed = false;
  [&] {  // See NOTE[setjmp-in-tests].
    std::jmp_buf buf;
    if (setjmp(buf) == 0) {
      stacked_buffering_visitor v = stack.push();
      pushed = true;
      std::longjmp(buf, 1);
      // pop (doesn't execute)
    }
  }();
  ASSERT_TRUE(pushed);
}

TEST_F(test_buffering_visitor_stack, longjmp_around_pop_then_pop) {
  buffering_visitor_stack stack(&this->leak_detecting_memory_);

  {
    stacked_buffering_visitor outer_v = stack.push();

    bool pushed_inner = false;
    [&] {  // See NOTE[setjmp-in-tests].
      std::jmp_buf buf;
      if (setjmp(buf) == 0) {
        stacked_buffering_visitor inner_v = stack.push();
        pushed_inner = true;
        std::longjmp(buf, 1);
        // pop (doesn't execute)
      }
    }();
    ASSERT_TRUE(pushed_inner);
    // pop outer_v
  }
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
