// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gtest/gtest.h>
#include <quick-lint-js/container/allocator.h>
#include <quick-lint-js/tracking-memory-resource.h>

namespace quick_lint_js {
namespace {
TEST(test_tracking_memory_resource, new_has_no_allocations) {
  tracking_memory_resource memory;
  EXPECT_EQ(memory.alive_bytes(), 0);
  EXPECT_EQ(memory.allocated_bytes(), 0);
  EXPECT_EQ(memory.deallocated_bytes(), 0);
}

TEST(test_tracking_memory_resource, allocating_increases_live_and_allocated) {
  tracking_memory_resource memory;
  int* p = new_object<int>(&memory, 42);
  EXPECT_EQ(memory.alive_bytes(), sizeof(int));
  EXPECT_EQ(memory.allocated_bytes(), sizeof(int));
  EXPECT_EQ(memory.deallocated_bytes(), 0);
  delete_object<int>(&memory, p);
}

TEST(test_tracking_memory_resource, deallocating_resets_live_to_zero) {
  tracking_memory_resource memory;
  int* p1 = new_object<int>(&memory, 42);
  int* p2 = new_object<int>(&memory, 69);
  EXPECT_EQ(memory.alive_bytes(), sizeof(int) * 2);
  delete_object<int>(&memory, p1);
  EXPECT_EQ(memory.alive_bytes(), sizeof(int));
  delete_object<int>(&memory, p2);
  EXPECT_EQ(memory.alive_bytes(), 0);
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
