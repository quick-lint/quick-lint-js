// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_UTIL_INSTANCE_TRACKER_H
#define QUICK_LINT_JS_UTIL_INSTANCE_TRACKER_H

#include <memory>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/port/vector-erase.h>
#include <vector>

namespace quick_lint_js {
// Maintains a global list of instances of Tracked. Each Tracked must be managed
// using std::shared_ptr. Instances are manually tracked and automatically
// untracked.
//
// instance_tracker is thread-safe.
template <class Tracked>
class instance_tracker {
 public:
  static void track(std::shared_ptr<Tracked> instance) {
    std::lock_guard lock(mutex_);
    sanitize_instances(lock);
    weak_instances_.push_back(std::move(instance));
  }

  static std::vector<std::shared_ptr<Tracked>> instances() {
    std::vector<std::shared_ptr<Tracked>> instances;
    {
      std::lock_guard lock(mutex_);
      sanitize_instances(lock);
      instances.reserve(weak_instances_.size());
      for (const std::weak_ptr<Tracked>& weak_instance : weak_instances_) {
        std::shared_ptr<Tracked> instance = weak_instance.lock();
        if (instance) {
          instances.emplace_back(std::move(instance));
        }
      }
    }
    return instances;
  }

 private:
  static void sanitize_instances(std::lock_guard<mutex>&) {
    erase_if(weak_instances_, [](const std::weak_ptr<Tracked>& weak_instance) {
      return weak_instance.expired();
    });
  }

  static void sanitize_instances() {
    std::lock_guard lock(mutex_);
    sanitize_instances(lock);
  }

  static inline mutex mutex_;
  // Protected by mutex_:
  static inline std::vector<std::weak_ptr<Tracked>> weak_instances_;
};
}

#endif

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
