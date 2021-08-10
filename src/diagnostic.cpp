// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/gmo.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/narrow-cast.h>
#include <utility>

namespace quick_lint_js {
// Convert a legacy error_formatter user into a diagnostic_info.
template <class Error>
class diagnostic_info_builder {
 public:
  constexpr explicit diagnostic_info_builder(const char* code,
                                             const void* diagnostic)
      : diagnostic_(diagnostic) {
    std::strcpy(this->info_.code, code);
  }

  template <class... Args>
  constexpr diagnostic_info_builder& warning(const gmo_message& message,
                                             Args&... parameters) {
    this->add(diagnostic_severity::warning, message,
              std::forward<Args>(parameters)...);
    return *this;
  }

  template <class... Args>
  constexpr diagnostic_info_builder& error(const gmo_message& message,
                                           Args&... parameters) {
    this->add(diagnostic_severity::error, message,
              std::forward<Args>(parameters)...);
    return *this;
  }

  template <class... Args>
  constexpr diagnostic_info_builder& note(const gmo_message& message,
                                          Args&... parameters) {
    this->add(diagnostic_severity::note, message,
              std::forward<Args>(parameters)...);
    return *this;
  }

  constexpr diagnostic_info build() noexcept { return this->info_; }

 private:
  template <class... Args>
  void add(diagnostic_severity sev, const gmo_message& message, Args&... args) {
    diagnostic_message_info& message_info =
        this->info_.messages[this->current_message_index_++];
    message_info.format = message;
    message_info.severity = sev;

    int current_arg_index = 0;
    auto add_arg = [&](auto& arg_value) {
      diagnostic_message_arg_info& arg_info =
          message_info.args[current_arg_index++];
      arg_info.type = arg_type(arg_value);
      arg_info.offset = narrow_cast<std::uint8_t>(
          reinterpret_cast<const char*>(&arg_value) -
          reinterpret_cast<const char*>(this->diagnostic_));
    };
    (add_arg(args), ...);
  }

  static diagnostic_arg_type arg_type(char8) noexcept {
    return diagnostic_arg_type::char8;
  }
  static diagnostic_arg_type arg_type(identifier) noexcept {
    return diagnostic_arg_type::identifier;
  }
  static diagnostic_arg_type arg_type(source_code_span) noexcept {
    return diagnostic_arg_type::source_code_span;
  }
  static diagnostic_arg_type arg_type(statement_kind) noexcept {
    return diagnostic_arg_type::statement_kind;
  }

  const void* diagnostic_;
  diagnostic_info info_;
  int current_message_index_ = 0;
};

template <class Error>
struct diagnostic_info_for_error_detail;

// Convert a legacy error_formatter user into a diagnostic_info.
#define QLJS_ERROR_TYPE(name, code, struct_body, format_call)               \
  template <>                                                               \
  struct diagnostic_info_for_error_detail<name> : public name {             \
    constexpr diagnostic_info build_impl() const noexcept {                 \
      return diagnostic_info_builder<name>(code, this) format_call.build(); \
    }                                                                       \
                                                                            \
    static diagnostic_info build() noexcept {                               \
      union U {                                                             \
        U() {}                                                              \
        diagnostic_info_for_error_detail instance;                          \
      } u;                                                                  \
      /* HACK(strager): This is probably undefined behavior. */             \
      return u.instance.build_impl();                                       \
    }                                                                       \
  };
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE

const diagnostic_info all_diagnostic_infos[] = {
#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  diagnostic_info_for_error_detail<name>::build(),
    QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
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
