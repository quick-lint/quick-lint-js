// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <memory>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/port/vector-erase.h>
#include <quick-lint-js/util/synchronized.h>

namespace quick_lint_js {
// Maintains a global list of instances of Tracked. Each Tracked must be managed
// using std::shared_ptr. Instances are manually tracked and automatically
// untracked.
//
// Instance_Tracker is thread-safe.
template <class Tracked>
class Instance_Tracker {
 public:
  static void track(std::shared_ptr<Tracked> instance) {
    Lock_Ptr<Raw_Vector<std::weak_ptr<Tracked>>> weak_instances =
        weak_instances_.lock();
    sanitize_instances(weak_instances);
    weak_instances->push_back(std::move(instance));
  }

  static Raw_Vector<std::shared_ptr<Tracked>> instances() {
    Raw_Vector<std::shared_ptr<Tracked>> instances(new_delete_resource());
    {
      Lock_Ptr<Raw_Vector<std::weak_ptr<Tracked>>> weak_instances =
          weak_instances_.lock();
      sanitize_instances(weak_instances);
      instances.reserve(weak_instances->size());
      for (const std::weak_ptr<Tracked>& weak_instance : *weak_instances) {
        std::shared_ptr<Tracked> instance = weak_instance.lock();
        if (instance) {
          instances.emplace_back(std::move(instance));
        }
      }
    }
    // NOTE(strager): We cannot wink (e.g. use Linked_Bump_Allocator and return
    // a Span) because std::shared_ptr destructors need to be called.
    return instances;
  }

 private:
  static void sanitize_instances(
      Lock_Ptr<Raw_Vector<std::weak_ptr<Tracked>>>& weak_instances) {
    erase_if(*weak_instances, [](const std::weak_ptr<Tracked>& weak_instance) {
      return weak_instance.expired();
    });
  }

  static void sanitize_instances() {
    sanitize_instances(weak_instances_.lock());
  }

  // NOTE(strager): We use Raw_Vector here instead of Vector. Otherwise, in
  // vector instrumented builds, weak_instances_ might be initialized before the
  // vector profiler is initialized, causing use-before-init issues.
  static inline Synchronized<Raw_Vector<std::weak_ptr<Tracked>>>
      weak_instances_{new_delete_resource()};
};
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
