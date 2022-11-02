// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <memory>
#include <quick-lint-js/util/instance-tracker.h>
#include <vector>

namespace quick_lint_js {
namespace {
// Helper class for tests only.
struct trackable {
  static inline int destruct_count = 0;

  ~trackable() { destruct_count += 1; }
};

TEST(test_instance_tracker, instances_shows_new_instance) {
  std::shared_ptr<trackable> o = std::make_shared<trackable>();
  instance_tracker<trackable>::track(o);
  EXPECT_THAT(instance_tracker<trackable>::instances(), ::testing::Contains(o));
}

TEST(test_instance_tracker, destroying_removes_from_instances) {
  std::shared_ptr<trackable> o = std::make_shared<trackable>();
  instance_tracker<trackable>::track(o);
  trackable* o_raw = o.get();
  o.reset();

  for (std::shared_ptr<trackable>& instance :
       instance_tracker<trackable>::instances()) {
    EXPECT_NE(instance.get(), o_raw);
  }
}

TEST(test_instance_tracker,
     resetting_tracked_pointer_destructs_tracked_object) {
  std::shared_ptr<trackable> o = std::make_shared<trackable>();
  int old_destruct_count = trackable::destruct_count;
  instance_tracker<trackable>::track(o);
  o.reset();
  EXPECT_EQ(trackable::destruct_count, old_destruct_count + 1);
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
