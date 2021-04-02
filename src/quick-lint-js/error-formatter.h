// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_ERROR_FORMATTER_H
#define QUICK_LINT_JS_ERROR_FORMATTER_H

#include <cstddef>
#include <initializer_list>
#include <iterator>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/gmo.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/translation.h>
#include <quick-lint-js/unreachable.h>
#include <type_traits>
#include <utility>

namespace quick_lint_js {
template <class Error>
struct error_formatter_detail;

string8_view translated_headlinese_statement_kind(statement_kind) noexcept;
string8_view translated_singular_statement_kind(statement_kind) noexcept;

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  template <>                                                 \
  struct error_formatter_detail<name> : public name {         \
    template <class Formatter>                                \
    void format(Formatter &&formatter) const {                \
      std::forward<Formatter>(formatter) format_call.end();   \
    }                                                         \
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

class error_formatter_base {
 public:
  enum class severity {
    error,
    note,
    warning,
  };

  enum class parameter_kind {
    statement_kind,
    string_view,
  };

  struct parameter {
    explicit parameter(statement_kind sk)
        : kind(parameter_kind::statement_kind), kind_of_statement(sk) {}

    explicit parameter(string8_view string_view)
        : kind(parameter_kind::string_view), string_view(string_view) {}

    statement_kind get_statement_kind() const noexcept {
      QLJS_ASSERT(this->kind == parameter_kind::statement_kind);
      return this->kind_of_statement;
    }

    string8_view get_string_view() const noexcept {
      QLJS_ASSERT(this->kind == parameter_kind::string_view);
      return this->string_view;
    }

    parameter_kind kind;
    union {
      statement_kind kind_of_statement;

      string8_view string_view;
      static_assert(std::is_trivially_copyable_v<string8_view>);
      static_assert(std::is_trivially_destructible_v<string8_view>);
    };
  };

 protected:
  static const source_code_span &to_span(const source_code_span &span) {
    return span;
  }

  static source_code_span to_span(identifier ident) { return ident.span(); }

  static parameter to_parameter(statement_kind sk) { return parameter(sk); }

  static parameter to_parameter(string8_view s) { return parameter(s); }

  static parameter to_parameter(const source_code_span &span) {
    return parameter(span.string_view());
  }

  static parameter to_parameter(identifier ident) {
    return parameter(ident.span().string_view());
  }
};

template <class Derived>
class error_formatter : public error_formatter_base {
 public:
  // Assumed member functions in Derived:
  // void write_before_message(severity, const source_code_span &origin);
  // void write_message_part(severity, string8_view);
  // void write_after_message(severity, const source_code_span &origin);

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
              {this->to_parameter(origin),
               this->to_parameter(std::forward<Args>(parameters))...});
  }

  void add(severity, const gmo_message &message,
           const source_code_span &origin_span,
           std::initializer_list<parameter> parameters);
};

template <class Derived>
inline void error_formatter<Derived>::add(
    severity sev, const gmo_message &message,
    const source_code_span &origin_span,
    std::initializer_list<parameter> parameters) {
  static constexpr auto npos = string8_view::npos;
  using string8_pos = string8_view::size_type;

  Derived *self = static_cast<Derived *>(this);

  self->write_before_message(sev, origin_span);

  string8_view remaining_message(translate(message));
  string8_pos left_curly_index;
  while ((left_curly_index = remaining_message.find(u8'{')) != npos) {
    QLJS_ASSERT(left_curly_index != remaining_message.size() &&
                "invalid message format: { at end of string has no matching }");

    if (remaining_message[left_curly_index + 1] == '{') {
      // "{{"; the '{' is escaped.
      self->write_message_part(
          sev, remaining_message.substr(0, left_curly_index + 1));
      remaining_message = remaining_message.substr(left_curly_index + 2);
      continue;
    }

    self->write_message_part(sev,
                             remaining_message.substr(0, left_curly_index));

    string8_pos right_curly_index =
        remaining_message.find(u8'}', left_curly_index + 1);
    QLJS_ASSERT(right_curly_index != npos &&
                "invalid message format: missing }");
    string8_view curly_content = remaining_message.substr(
        left_curly_index + 1, right_curly_index - (left_curly_index + 1));

    string8_view expanded_parameter;
    if (curly_content == u8"0") {
      expanded_parameter = (parameters.begin() + 0)->get_string_view();
    } else if (curly_content == u8"1") {
      expanded_parameter = (parameters.begin() + 1)->get_string_view();
    } else if (curly_content == u8"1:headlinese") {
      expanded_parameter = translated_headlinese_statement_kind(
          (parameters.begin() + 1)->get_statement_kind());
    } else if (curly_content == u8"1:singular") {
      expanded_parameter = translated_singular_statement_kind(
          (parameters.begin() + 1)->get_statement_kind());
    } else if (curly_content == u8"2") {
      expanded_parameter = (parameters.begin() + 2)->get_string_view();
    } else {
      QLJS_ASSERT(false && "invalid message format: unrecognized placeholder");
      QLJS_UNREACHABLE();
    }

    self->write_message_part(sev, expanded_parameter);
    remaining_message = remaining_message.substr(right_curly_index + 1);
  }
  self->write_message_part(sev, remaining_message);

  self->write_after_message(sev, origin_span);
}
}

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
