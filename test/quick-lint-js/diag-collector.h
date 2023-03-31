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
struct diag_collector : public diag_reporter {
  void report_impl(diag_type type, void *diag) override;

  // Like std::variant<(diag types)>, but with much faster compilation.
  class diag {
   public:
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  explicit diag(const name &);
    QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE

    diag_type type() const noexcept;
    const char *error_code() const noexcept;
    const void *data() const noexcept;

    template <class Diag>
    friend const Diag &get(const diag &) noexcept;

    template <class Diag>
    friend bool holds_alternative(const diag &) noexcept;

    friend void PrintTo(const diag &, std::ostream *);

   private:
    diag_type type_;
    union {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  name variant_##name##_;
      QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
    };
  };

  std::vector<diag> errors;
};

template <class Diag>
const Diag &get(const diag_collector::diag &) noexcept;

template <class Diag>
bool holds_alternative(const diag_collector::diag &) noexcept;

void PrintTo(const diag_collector::diag &, std::ostream *);

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  void PrintTo(const name &, std::ostream *);
QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
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
