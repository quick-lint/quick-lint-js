// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <iosfwd>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <utility>
#include <vector>

namespace quick_lint_js {
struct Diag_Collector : public Diag_Reporter {
  void report_impl(Diag_Type type, void *diag) override;

  // Like std::variant<(diag types)>, but with much faster compilation.
  class Diag {
   public:
#define QLJS_DIAG_TYPE_NAME(name) explicit Diag(const name &);
    QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME

    Diag_Type type() const;
    const void *data() const;

    friend void PrintTo(const Diag &, std::ostream *);

   private:
    Diag_Type type_;
    union {
#define QLJS_DIAG_TYPE_NAME(name) name variant_##name##_;
      QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME
    };
  };

  std::vector<Diag> errors;
};

void PrintTo(const Diag_Collector::Diag &, std::ostream *);
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
