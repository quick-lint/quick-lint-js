// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/cpp.h>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/error.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/warning.h>
#include <string_view>
#include <utility>

namespace quick_lint_js {
namespace {
constexpr void strcpy(char* out, const char* in) noexcept {
  while ((*out++ = *in++) != '\0')
    ;
}

// Convert a QLJS_ERROR_TYPE user into a diagnostic_info.
template <class Error>
class diagnostic_info_builder {
 public:
  constexpr explicit diagnostic_info_builder(const char* code) {
    strcpy(this->info_.code, code);
  }

  // Each of Args must be a diagnostic_message_arg_info.
  template <class... Args>
  constexpr diagnostic_info_builder add(diagnostic_severity sev,
                                        const translatable_message& message,
                                        const Args&... arg_infos) {
    diagnostic_message_info& message_info =
        this->info_.messages[this->current_message_index_++];
    message_info.format = message;
    message_info.severity = sev;

    int current_arg_index = 0;
    ((message_info.args[current_arg_index++] = arg_infos), ...);

    return *this;
  }

  constexpr diagnostic_info build() noexcept { return this->info_; }

 private:
  diagnostic_info info_{};
  int current_message_index_ = 0;
};

template <class ArgType>
constexpr diagnostic_arg_type get_diagnostic_message_arg_type() noexcept;
template <>
constexpr diagnostic_arg_type
get_diagnostic_message_arg_type<char8>() noexcept {
  return diagnostic_arg_type::char8;
}
template <>
constexpr diagnostic_arg_type
get_diagnostic_message_arg_type<identifier>() noexcept {
  return diagnostic_arg_type::identifier;
}
template <>
constexpr diagnostic_arg_type
get_diagnostic_message_arg_type<source_code_span>() noexcept {
  return diagnostic_arg_type::source_code_span;
}
template <>
constexpr diagnostic_arg_type
get_diagnostic_message_arg_type<statement_kind>() noexcept {
  return diagnostic_arg_type::statement_kind;
}

template <class ArgType>
constexpr diagnostic_message_arg_info make_diagnostic_message_arg_info(
    std::uint8_t offset) noexcept {
  return diagnostic_message_arg_info{
      .offset = offset,
      .type = get_diagnostic_message_arg_type<ArgType>(),
  };
}

template <class Error>
struct diagnostic_info_for_error;

#define MAKE_ARGS(...) MAKE_ARGS_N(QLJS_COUNT_ARGS(__VA_ARGS__), __VA_ARGS__)
#define MAKE_ARGS_N(...) MAKE_ARGS_N_(__VA_ARGS__)
#define MAKE_ARGS_N_(count, ...) MAKE_ARGS_##count(__VA_ARGS__)

#define MAKE_ARGS_1(arg0)                                        \
  make_diagnostic_message_arg_info<decltype(error_class::arg0)>( \
      offsetof(error_class, arg0))
#define MAKE_ARGS_2(arg0, arg1) MAKE_ARGS_1(arg0), MAKE_ARGS_1(arg1)
#define MAKE_ARGS_3(arg0, arg1, arg2) MAKE_ARGS_2(arg0, arg1), MAKE_ARGS_1(arg2)

#define ERROR(message_format, ...) \
  .add(diagnostic_severity::error, message_format, MAKE_ARGS(__VA_ARGS__))
#define WARNING(message_format, ...) \
  .add(diagnostic_severity::warning, message_format, MAKE_ARGS(__VA_ARGS__))
#define NOTE(message_format, ...) \
  .add(diagnostic_severity::note, message_format, MAKE_ARGS(__VA_ARGS__))

#define QLJS_ERROR_TYPE(name, code, struct_body, format_call)         \
  template <>                                                         \
  struct diagnostic_info_for_error<name> {                            \
    using error_class = name;                                         \
                                                                      \
    static constexpr diagnostic_info get() noexcept {                 \
      return diagnostic_info_builder<name>(code) format_call.build(); \
    }                                                                 \
  };
QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
}

constexpr const diagnostic_info all_diagnostic_infos[] = {
#define QLJS_ERROR_TYPE(name, code, struct_body, format_call) \
  diagnostic_info_for_error<name>::get(),
    QLJS_X_ERROR_TYPES
#undef QLJS_ERROR_TYPE
};

const diagnostic_info& get_diagnostic_info(error_type type) noexcept {
  return all_diagnostic_infos[static_cast<std::ptrdiff_t>(type)];
}

QLJS_WARNING_PUSH
// GCC thinks that all_diagnostic_infos[i].code is not null-terminated, but it
// is.
QLJS_WARNING_IGNORE_GCC("-Wstringop-overflow")

std::optional<error_type> error_type_from_code_slow(
    std::string_view code) noexcept {
  for (int i = 0; i < error_type_count; ++i) {
    if (all_diagnostic_infos[i].code == code) {
      return static_cast<error_type>(i);
    }
  }
  return std::nullopt;
}

QLJS_WARNING_POP
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
