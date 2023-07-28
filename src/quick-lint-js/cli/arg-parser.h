// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_CLI_ARG_PARSER_H
#define QUICK_LINT_JS_CLI_ARG_PARSER_H

#include <optional>
#include <string_view>

namespace quick_lint_js {
// The following macros form a DSL for using arg_parser.
//
// Example:
//
// bool show_help = true;
// bool verbose_logging = true;
// arg_parser parser(argc, argv);
// QLJS_ARG_PARSER_LOOP(parser) {
//   // QLJS_ARGUMENT is *required* and *must be first*.
//   QLJS_ARGUMENT(const char* argument) {
//     // argument has type const char* in this scope.
//     std::printf("positional arg: %s\n", argument);
//   }
//
//   // QLJS_FLAG is optional and can be specified any number of times.
//   QLJS_FLAG('h', "--help"sv, "--help"sv) {
//     show_help = true;
//     // Loop control flow is allowed.
//     break;
//   }
//
//   // QLJS_OPTION is optional and can be specified any number of times.
//   QLJS_OPTION(const char* arg_value, "--format"sv) {
//     // arg_value has type const char* in this scope.
//     std::printf("setting --format to %s\n", arg_value);
//   }
//
//   // QLJS_FLAG and QLJS_OPTION can be interleaved arbitrarily.
//   QLJS_FLAG("--verbose"sv, "--verbose"sv) {
//     verbose_logging = true;
//   }
//
//   // QLJS_UNRECOGNIZED_OPTION is optional and *must be last*.
//   QLJS_UNRECOGNIZED_OPTION(const char* unrecognized) {
//     // unrecognized has type const char* in this scope.
//     std::printf("error: unrecognized option: %s\n", unrecognized);
//   }
// }

// QLJS_ARGUMENT must always be present and come before any QLJS_FLAG-s or
// QLJS_OPTION-s.
#define QLJS_ARGUMENT(variable) if (variable = _arg_parser.match_argument())

// QLJS_FLAG takes the same arguments as arg_parser::match_flag_option.
#define QLJS_FLAG(...)                                                         \
  /* 'if' is implied from a prior QLJS_ARGUMENT, QLJS_FLAG, or QLJS_OPTION. */ \
  else if (_arg_parser.match_flag_option(__VA_ARGS__))

// QLJS_OPTION takes the same arguments as arg_parser::match_option_with_value.
#define QLJS_OPTION(variable, ...)                                             \
  /* 'if' is implied from a prior QLJS_ARGUMENT, QLJS_FLAG, or QLJS_OPTION. */ \
  else if (variable = _arg_parser.match_option_with_value(__VA_ARGS__))

// QLJS_UNRECOGNIZED_OPTION, if present, must come after all QLJS_FLAG-s,
// QLJS_OPTION-s, and QLJS_ARGUMENT.
#define QLJS_UNRECOGNIZED_OPTION(variable)                                     \
  /* 'if' is implied from a prior QLJS_ARGUMENT, QLJS_FLAG, or QLJS_OPTION. */ \
  /* NOTE(strager): match_anything never returns nullptr. */                   \
  else if (variable = _arg_parser.match_anything())

#define QLJS_ARG_PARSER_LOOP(parser)                        \
  for (::quick_lint_js::Arg_Parser& _arg_parser = (parser); \
       !_arg_parser.done();)

class Arg_Parser {
 public:
  explicit Arg_Parser(int argc, char** argv);

  Arg_Parser(const Arg_Parser&) = delete;
  Arg_Parser& operator=(const Arg_Parser&) = delete;

  const char* match_option_with_value(std::string_view option_name);

  bool match_flag_shorthand(char option_shorthand);

  bool match_flag_option(std::string_view full_option_name,
                         std::string_view partial_option_name);

  // Equivalent to:
  //
  // this->match_flag_option(full_option_name, partial_option_name)
  // || this->match_flag_shorthand(option_shorthand)
  bool match_flag_option(char option_shorthand,
                         std::string_view full_option_name,
                         std::string_view partial_option_name);

  const char* match_argument();

  const char* match_anything();

  bool done() const;

 private:
  void parse_current_arg();

  void advance(int count);

  const char* current_arg();

  struct Option {
    std::string_view arg_key;
    const char* arg_value;
    bool arg_has_equal;
  };

  std::optional<Option> option_;
  bool is_ignoring_options_ = false;
  int current_arg_index_ = 1;

  int argc_;
  char** argv_;
};
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
