// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FILESYSTEM_TEST_H
#define QUICK_LINT_JS_FILESYSTEM_TEST_H

#include <optional>
#include <quick-lint-js/temporary-directory.h>
#include <string>
#include <vector>

namespace quick_lint_js {
// Mixin for tests which manipulate the filesystem.
class filesystem_test {
 public:
  ~filesystem_test() { this->clean_up_filesystem(); }

  std::string make_temporary_directory() {
    std::string temp_dir = quick_lint_js::make_temporary_directory();
    this->temporary_directories_.emplace_back(temp_dir);
    return temp_dir;
  }

  void set_current_working_directory(const std::string& path) {
    this->set_current_working_directory(path.c_str());
  }

  void set_current_working_directory(const char* path) {
    if (!this->old_working_directory_.has_value()) {
      this->old_working_directory_ = get_current_working_directory();
    }
    quick_lint_js::set_current_working_directory(path);
  }

  void clean_up_filesystem() {
    if (this->old_working_directory_.has_value()) {
      set_current_working_directory(*this->old_working_directory_);
    }
    for (const std::string& temp_dir : this->temporary_directories_) {
      delete_directory_recursive(temp_dir);
    }
  }

 private:
  std::vector<std::string> temporary_directories_;
  std::optional<std::string> old_working_directory_;
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
