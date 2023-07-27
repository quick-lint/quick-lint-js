// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAG_BUFFERING_DIAG_REPORTER_H
#define QUICK_LINT_JS_DIAG_BUFFERING_DIAG_REPORTER_H

#include <memory>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/memory-resource.h>

namespace quick_lint_js {
class Buffering_Diag_Reporter final : public Diag_Reporter {
 public:
  explicit Buffering_Diag_Reporter(Memory_Resource *);

  Buffering_Diag_Reporter(Buffering_Diag_Reporter &&);
  Buffering_Diag_Reporter &operator=(Buffering_Diag_Reporter &&);

  ~Buffering_Diag_Reporter() override;

  void report_impl(Diag_Type type, void *diag) override;

  void copy_into(Diag_Reporter *other) const;
  void move_into(Diag_Reporter *other);

  bool empty() const;

  void clear();

 private:
  struct Impl;

  struct Impl_Deleter {
    void operator()(Impl *);
  };

  std::unique_ptr<Impl, Impl_Deleter> impl_;
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
