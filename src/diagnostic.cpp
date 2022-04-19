// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <array>
#include <cstdint>
#include <cstring>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/cpp.h>
#include <quick-lint-js/diagnostic-types.h>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/warning.h>
#include <string_view>
#include <utility>

#if defined(_MSC_FULL_VER) && _MSC_FULL_VER <= 192829337
// Work around the following false compilation error in MSVC version 19.28.29337
// (aka 14.28-16.8 aka 14.28.29333) and older:
//
// error C2131: expression did not evaluate to a constant
// message: failure was caused by out of range index 3; allowed range is 0 <=
//          index < 2
// message: while evaluating constexpr function
//          diagnostic_info_builder<diag_adjacent_jsx_without_parent>::add
// message: while evaluating constexpr function
//          info_for_diagnostic<diag_adjacent_jsx_without_parent>::get
#define DIAGNOSTIC_CONSTEXPR_IF_POSSIBLE /* */
#else
#define DIAGNOSTIC_CONSTEXPR_IF_POSSIBLE constexpr
#endif

namespace quick_lint_js {
namespace {
constexpr std::uint16_t parse_code_string(const char* code_string) noexcept {
  QLJS_CONSTEXPR_ASSERT(code_string[0] == 'E');
  QLJS_CONSTEXPR_ASSERT('0' <= code_string[1] && code_string[1] <= '9');
  QLJS_CONSTEXPR_ASSERT('0' <= code_string[2] && code_string[2] <= '9');
  QLJS_CONSTEXPR_ASSERT('0' <= code_string[3] && code_string[3] <= '9');
  QLJS_CONSTEXPR_ASSERT('0' <= code_string[4] && code_string[4] <= '9');
  QLJS_CONSTEXPR_ASSERT(code_string[5] == '\0');
  return static_cast<std::uint16_t>((code_string[1] - '0') * 1000 +  //
                                    (code_string[2] - '0') * 100 +   //
                                    (code_string[3] - '0') * 10 +    //
                                    (code_string[4] - '0') * 1);
}

std::array<char, 5> diag_code_to_string(std::uint16_t diag_code) noexcept {
  QLJS_ASSERT(diag_code <= 9999);
  return std::array<char, 5>{
      'E',
      static_cast<char>('0' + ((diag_code / 1000) % 10)),
      static_cast<char>('0' + ((diag_code / 100) % 10)),
      static_cast<char>('0' + ((diag_code / 10) % 10)),
      static_cast<char>('0' + ((diag_code / 1) % 10)),
  };
}

// Convert a QLJS_DIAG_TYPE user into a diagnostic_info.
template <class Diag>
class diagnostic_info_builder {
 public:
  QLJS_WARNING_PUSH
  QLJS_WARNING_IGNORE_GCC("-Wconversion")
  constexpr explicit diagnostic_info_builder(const char* code_string,
                                             diagnostic_severity sev) {
    this->info_.severity = sev;
    this->info_.code = parse_code_string(code_string);
  }
  QLJS_WARNING_POP

  // Each of Args must be a diagnostic_message_arg_info.
  template <class... Args>
  constexpr diagnostic_info_builder add(const translatable_message& message,
                                        const Args&... arg_infos) {
    this->info_.message_formats[this->current_message_index_] = message;

    std::size_t current_arg_index = 0;
    diagnostic_message_args& args =
        this->info_.message_args[this->current_message_index_];
    ((args[current_arg_index++] = arg_infos), ...);

    ++this->current_message_index_;
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
template <>
constexpr diagnostic_arg_type
get_diagnostic_message_arg_type<string8_view>() noexcept {
  return diagnostic_arg_type::string8_view;
}

template <class ArgType>
constexpr diagnostic_message_arg_info make_diagnostic_message_arg_info(
    std::uint8_t offset) noexcept {
  return diagnostic_message_arg_info(
      offset, get_diagnostic_message_arg_type<ArgType>());
}

template <class Diag>
struct info_for_diagnostic;

#define MAKE_ARGS(...) MAKE_ARGS_N(QLJS_COUNT_ARGS(__VA_ARGS__), __VA_ARGS__)
#define MAKE_ARGS_N(...) MAKE_ARGS_N_(__VA_ARGS__)
#define MAKE_ARGS_N_(count, ...) MAKE_ARGS_##count(__VA_ARGS__)

#define MAKE_ARGS_1(arg0)                                       \
  make_diagnostic_message_arg_info<decltype(diag_class::arg0)>( \
      offsetof(diag_class, arg0))
#define MAKE_ARGS_2(arg0, arg1) MAKE_ARGS_1(arg0), MAKE_ARGS_1(arg1)
#define MAKE_ARGS_3(arg0, arg1, arg2) MAKE_ARGS_2(arg0, arg1), MAKE_ARGS_1(arg2)

#define MESSAGE(message_format, ...) \
  .add(message_format, MAKE_ARGS(__VA_ARGS__))

#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call)       \
  template <>                                                                \
  struct info_for_diagnostic<name> {                                         \
    using diag_class = name;                                                 \
                                                                             \
    static DIAGNOSTIC_CONSTEXPR_IF_POSSIBLE diagnostic_info get() noexcept { \
      return diagnostic_info_builder<name>(code, severity)                   \
          format_call.build();                                               \
    }                                                                        \
  };
QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
}

DIAGNOSTIC_CONSTEXPR_IF_POSSIBLE const diagnostic_info
    all_diagnostic_infos[] = {
#define QLJS_DIAG_TYPE(name, code, severity, struct_body, format_call) \
  info_for_diagnostic<name>::get(),
        QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
};

const diagnostic_info& get_diagnostic_info(diag_type type) noexcept {
  return all_diagnostic_infos[static_cast<std::ptrdiff_t>(type)];
}

std::array<char, 5> diagnostic_info::code_string() const noexcept {
  return diag_code_to_string(this->code);
}

QLJS_WARNING_PUSH
// GCC thinks that all_diagnostic_infos[i].code is not null-terminated, but it
// is.
QLJS_WARNING_IGNORE_GCC("-Wstringop-overflow")

std::optional<diag_type> diag_type_from_code_slow(
    std::string_view code) noexcept {
  for (int i = 0; i < diag_type_count; ++i) {
    // TODO(strager): Parse the incoming code instead of stringifying each code
    // in the table.
    auto diag_code_string = all_diagnostic_infos[i].code_string();
    std::string_view diag_code_string_view(diag_code_string.data(),
                                           diag_code_string.size());
    if (diag_code_string_view == code) {
      return static_cast<diag_type>(i);
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
