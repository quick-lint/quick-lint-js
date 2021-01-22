// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef QUICK_LINT_JS_ERROR_FORMATTER_H
#define QUICK_LINT_JS_ERROR_FORMATTER_H

#include <cstddef>
#include <initializer_list>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/gmo.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/translation.h>
#include <quick-lint-js/unreachable.h>
#include <utility>

namespace quick_lint_js {
template <class Error>
struct error_formatter_detail;

#define QLJS_ERROR_TYPE(name, struct_body, format_call)     \
  template <>                                               \
  struct error_formatter_detail<name> : public name {       \
    template <class Formatter>                              \
    void format(Formatter &&formatter) const {              \
      std::forward<Formatter>(formatter) format_call.end(); \
    }                                                       \
  };
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

template <class Error, class Formatter>
inline void format_error(const Error &e, Formatter &&formatter) {
  // HACK(strager): This cast invokes undefined behavior. But it's probably
  // fine...
  const auto &f = static_cast<const error_formatter_detail<Error> &>(e);
  f.format(std::forward<Formatter>(formatter));
}

template <class Derived>
class error_formatter {
 public:
  // Assumed member functions in Derived:
  // void write_before_message(severity, const source_code_span &origin);
  // void write_message_part(severity, string8_view);
  // void write_after_message(severity, const source_code_span &origin);

  enum class severity {
    error,
    note,
    warning,
  };

  template <class... Args>
  error_formatter &warning(const gmo_message &message, Args... parameters) {
    this->add(severity::warning, message, std::forward<Args>(parameters)...);
    return *this;
  }

  template <class... Args>
  error_formatter &error(const gmo_message &message, Args... parameters) {
    this->add(severity::error, message, std::forward<Args>(parameters)...);
    return *this;
  }

  template <class... Args>
  error_formatter &note(const gmo_message &message, Args &&... parameters) {
    this->add(severity::note, message, std::forward<Args>(parameters)...);
    return *this;
  }

  void end() {}

 private:
  template <class Origin, class... Args>
  void add(severity sev, const gmo_message &message, const Origin &origin,
           Args &&... parameters) {
    this->add(sev, message, this->to_span(origin),
              {this->to_string_view(origin),
               this->to_string_view(std::forward<Args>(parameters))...});
  }

  void add(severity, const gmo_message &message,
           const source_code_span &origin_span,
           std::initializer_list<string8_view> parameters);

  static const source_code_span &to_span(const source_code_span &span) {
    return span;
  }

  static source_code_span to_span(identifier ident) { return ident.span(); }

  static string8_view to_string_view(string8_view s) { return s; }

  static string8_view to_string_view(const source_code_span &span) {
    return span.string_view();
  }

  static string8_view to_string_view(identifier ident) {
    return ident.span().string_view();
  }
};

template <class Derived>
inline void error_formatter<Derived>::add(
    severity sev, const gmo_message &message,
    const source_code_span &origin_span,
    std::initializer_list<string8_view> parameters) {
  static constexpr auto npos = string8_view::npos;
  using string8_pos = string8_view::size_type;

  Derived *self = static_cast<Derived *>(this);

  self->write_before_message(sev, origin_span);

  string8_view remaining_message(translate(message));
  string8_pos left_curly_index;
  while ((left_curly_index = remaining_message.find(u8'{')) != npos) {
    self->write_message_part(sev,
                             remaining_message.substr(0, left_curly_index));

    string8_pos right_curly_index =
        remaining_message.find(u8'}', left_curly_index + 1);
    QLJS_ASSERT(right_curly_index != npos &&
                "invalid message format: missing }");
    string8_view curly_content = remaining_message.substr(
        left_curly_index + 1, right_curly_index - (left_curly_index + 1));
    std::size_t index;
    if (curly_content == u8"0") {
      index = 0;
    } else if (curly_content == u8"1") {
      index = 1;
    } else if (curly_content == u8"2") {
      index = 2;
    } else {
      QLJS_ASSERT(false && "invalid message format: unrecognized placeholder");
      QLJS_UNREACHABLE();
    }

    self->write_message_part(sev, *(parameters.begin() + index));
    remaining_message = remaining_message.substr(right_curly_index + 1);
  }
  self->write_message_part(sev, remaining_message);

  self->write_after_message(sev, origin_span);
}
}

#endif
