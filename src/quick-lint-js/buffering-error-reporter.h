// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_BUFFERING_ERROR_REPORTER_H
#define QUICK_LINT_JS_BUFFERING_ERROR_REPORTER_H

#include <memory>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/token.h>

namespace quick_lint_js {
class buffering_error_reporter final : public new_style_error_reporter {
 public:
  explicit buffering_error_reporter();

  buffering_error_reporter(buffering_error_reporter &&);
  buffering_error_reporter &operator=(buffering_error_reporter &&);

  ~buffering_error_reporter() override;

  void report_impl(error_type type, void *error) override;

  void copy_into(error_reporter *other) const;
  void move_into(error_reporter *other);

  bool empty() const noexcept;

  void clear() noexcept;

 private:
  struct impl;

  std::unique_ptr<impl> impl_;
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
