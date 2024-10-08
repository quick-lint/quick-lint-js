// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/cli/text-diag-reporter.h>
#include <quick-lint-js/cli/vim-qflist-json-diag-reporter.h>
#include <quick-lint-js/diag/diag-code-list.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/token.h>

namespace quick_lint_js {
template <typename T>
class Reported_Diag_Statistics final : public Diag_Reporter {
 public:
  explicit Reported_Diag_Statistics(T reporter,
                                    const Compiled_Diag_Code_List *predicate)
      : reporter_(reporter), predicate_(predicate) {}

  T *get_reporter() { return &(this->reporter_); }

  bool found_matching_diag() const { return this->found_matching_diag_; }

  void report(const Diag_List &diags) override final {
    diags.for_each([&](Diag_Type type, [[maybe_unused]] void *diag) -> void {
      if (this->predicate_->is_present(type)) {
        this->found_matching_diag_ = true;
      }
    });
    this->reporter_.report(diags);
  }

 private:
  T reporter_;

  const Compiled_Diag_Code_List *predicate_;
  bool found_matching_diag_ = false;
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
