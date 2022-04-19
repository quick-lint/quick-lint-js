// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ERROR_COLLECTOR_H
#define QUICK_LINT_JS_ERROR_COLLECTOR_H

#include <iosfwd>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/diag-reporter.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/token.h>
#include <utility>
#include <vector>

namespace quick_lint_js {
struct error_collector : public diag_reporter {
  void report_impl(diag_type type, void *error) override;

  // Like std::variant<(error types)>, but with much faster compilation.
  class error {
   public:
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  explicit error(const name &);
    QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE

    diag_type type() const noexcept;
    const char *error_code() const noexcept;
    const void *data() const noexcept;

    template <class Error>
    friend const Error &get(const error &) noexcept;

    template <class Error>
    friend bool holds_alternative(const error &) noexcept;

    friend void PrintTo(const error &, std::ostream *);

   private:
    diag_type type_;
    union {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  name variant_##name##_;
      QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
    };
  };

  std::vector<error> errors;
};

template <class Error>
const Error &get(const error_collector::error &) noexcept;

template <class Error>
bool holds_alternative(const error_collector::error &) noexcept;

void PrintTo(const error_collector::error &, std::ostream *);

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
