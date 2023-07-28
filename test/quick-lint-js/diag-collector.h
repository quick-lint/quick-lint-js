// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAG_COLLECTOR_H
#define QUICK_LINT_JS_DIAG_COLLECTOR_H

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

    template <class Diag_Type>
    friend const Diag_Type &get(const Diag &);

    template <class Diag_Type>
    friend bool holds_alternative(const Diag &);

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

template <class Diag_Type>
const Diag_Type &get(const Diag_Collector::Diag &);

template <class Diag_Type>
bool holds_alternative(const Diag_Collector::Diag &);

void PrintTo(const Diag_Collector::Diag &, std::ostream *);

#define QLJS_DIAG_TYPE_NAME(name) void PrintTo(const name &, std::ostream *);
QLJS_X_DIAG_TYPE_NAMES
#undef QLJS_DIAG_TYPE_NAME
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
