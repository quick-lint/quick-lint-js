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
struct Trackable {
  static inline int destruct_count = 0;

  ~Trackable() { destruct_count += 1; }
};

TEST(Test_Instance_Tracker, instances_shows_new_instance) {
  std::shared_ptr<Trackable> o = std::make_shared<Trackable>();
  Instance_Tracker<Trackable>::track(o);
  EXPECT_THAT(Instance_Tracker<Trackable>::instances(), ::testing::Contains(o));
}

TEST(Test_Instance_Tracker, destroying_removes_from_instances) {
  std::shared_ptr<Trackable> o = std::make_shared<Trackable>();
  Instance_Tracker<Trackable>::track(o);
  Trackable* o_raw = o.get();
  o.reset();

  for (std::shared_ptr<Trackable>& instance :
       Instance_Tracker<Trackable>::instances()) {
    EXPECT_NE(instance.get(), o_raw);
  }
}

TEST(Test_Instance_Tracker,
     resetting_tracked_pointer_destructs_tracked_object) {
  std::shared_ptr<Trackable> o = std::make_shared<Trackable>();
  int old_destruct_count = Trackable::destruct_count;
  Instance_Tracker<Trackable>::track(o);
  o.reset();
  EXPECT_EQ(Trackable::destruct_count, old_destruct_count + 1);
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
