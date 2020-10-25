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

#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/integer.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/options.h>
#include <quick-lint-js/warning.h>
#include <string_view>
#include <vector>

QLJS_WARNING_IGNORE_GCC("-Wmaybe-uninitialized")
QLJS_WARNING_IGNORE_GCC("-Wshadow=local")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
bool starts_with(std::string_view haystack, std::string_view needle) noexcept {
  return haystack.substr(0, needle.size()) == needle;
}

class arg_parser {
 public:
  explicit arg_parser(int argc, char** argv) noexcept
      : argc_(argc), argv_(argv) {
    this->parse_current_arg();
  }

  arg_parser(const arg_parser&) = delete;
  arg_parser& operator=(const arg_parser&) = delete;

  const char* match_option_with_value(std::string_view option_name) noexcept {
    if (!this->option_.has_value() || !this->option_->arg_value) {
      return nullptr;
    }
    if (this->option_->arg_key != option_name) {
      return nullptr;
    }
    const char* arg_value = this->option_->arg_value;
    this->advance(this->option_->arg_has_equal ? 1 : 2);
    return arg_value;
  }

  bool match_flag_shorthand(char option_shorthand) noexcept {
    if (!this->option_.has_value()) {
      return false;
    }
    bool matches = this->option_->arg_key == std::string{'-', option_shorthand};

    if (matches) {
      this->advance(1);
    }
    return matches;
  }

  bool match_flag_option(std::string_view full_option_name,
                         std::string_view partial_option_name) noexcept {
    if (!this->option_.has_value()) {
      return false;
    }
    bool matches = starts_with(this->option_->arg_key, partial_option_name) &&
                   starts_with(full_option_name, this->option_->arg_key);
    if (matches) {
      this->advance(1);
    }
    return matches;
  }

  const char* match_argument() noexcept {
    if (this->option_.has_value()) {
      return nullptr;
    }
    return this->match_anything();
  }

  const char* match_anything() noexcept {
    const char* anything = this->current_arg();
    this->advance(1);
    return anything;
  }

  bool done() const noexcept { return this->current_arg_index_ >= this->argc_; }

 private:
  void parse_current_arg() noexcept {
    if (this->done()) {
      return;
    }
    if (this->is_ignoring_options_) {
      // Do nothing.
    } else if (this->current_arg() == "--"sv) {
      this->current_arg_index_ += 1;
      if (this->done()) {
        return;
      }
      this->is_ignoring_options_ = true;
      this->option_ = std::nullopt;
    } else if (this->current_arg()[0] == '-') {
      const char* equal = std::strchr(this->current_arg(), '=');
      option o;
      o.arg_has_equal = equal != nullptr;
      if (o.arg_has_equal) {
        o.arg_key = std::string_view(
            this->current_arg(),
            narrow_cast<std::size_t>(equal - this->current_arg()));
        o.arg_value = equal + 1;
      } else {
        o.arg_key = this->current_arg();
        o.arg_value = this->current_arg_index_ + 1 < this->argc_
                          ? this->argv_[this->current_arg_index_ + 1]
                          : nullptr;
      }
      this->option_ = o;
    } else {
      this->option_ = std::nullopt;
    }
  }

  void advance(int count) noexcept {
    this->current_arg_index_ += count;
    this->parse_current_arg();
  }

  const char* current_arg() noexcept {
    QLJS_ASSERT(this->current_arg_index_ < this->argc_);
    return this->argv_[this->current_arg_index_];
  }

  struct option {
    std::string_view arg_key;
    const char* arg_value;
    bool arg_has_equal;
  };

  std::optional<option> option_;
  bool is_ignoring_options_ = false;
  int current_arg_index_ = 1;

  int argc_;
  char** argv_;
};
}

options parse_options(int argc, char** argv) {
  options o;

  std::optional<int> next_vim_file_bufnr;

  arg_parser parser(argc, argv);
  while (!parser.done()) {
    if (const char* argument = parser.match_argument()) {
      file_to_lint file{.path = argument, .vim_bufnr = next_vim_file_bufnr};
      o.files_to_lint.emplace_back(file);
      next_vim_file_bufnr = std::nullopt;
    } else if (parser.match_flag_option("--debug-parser-visits"sv,
                                        "--debug-p"sv)) {
      o.print_parser_visits = true;
    } else if (const char* arg_value =
                   parser.match_option_with_value("--output-format"sv)) {
      if (arg_value == "gnu-like"sv) {
        o.output_format = quick_lint_js::output_format::gnu_like;
      } else if (arg_value == "vim-qflist-json"sv) {
        o.output_format = quick_lint_js::output_format::vim_qflist_json;
      } else {
        o.error_unrecognized_options.emplace_back(arg_value);
      }
    } else if (const char* arg_value =
                   parser.match_option_with_value("--vim-file-bufnr"sv)) {
      int bufnr;
      from_chars_result result =
          from_chars(&arg_value[0], &arg_value[std::strlen(arg_value)], bufnr);
      if (*result.ptr != '\0' || result.ec != std::errc{}) {
        o.error_unrecognized_options.emplace_back(arg_value);
      } else {
        next_vim_file_bufnr = bufnr;
      }
    } else if (parser.match_flag_option("--help"sv, "--h"sv) ||
               parser.match_flag_shorthand('h')) {
      o.help = true;
    } else if (parser.match_flag_option("--version"sv, "--v"sv) ||
               parser.match_flag_shorthand('v')) {
      o.version = true;
    } else if (parser.match_flag_option("--lsp-server"sv, "--lsp"sv)) {
      o.lsp_server = true;
    } else {
      const char* unrecognized = parser.match_anything();
      o.error_unrecognized_options.emplace_back(unrecognized);
      goto done_parsing_options;
    }
  }
done_parsing_options:

  return o;
}
}
