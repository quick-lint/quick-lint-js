// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <optional>
#include <quick-lint-js/arg-parser.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/string-view.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
arg_parser::arg_parser(int argc, char** argv) noexcept
    : argc_(argc), argv_(argv) {
  this->parse_current_arg();
}

const char* arg_parser::match_option_with_value(
    std::string_view option_name) noexcept {
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

bool arg_parser::match_flag_shorthand(char option_shorthand) noexcept {
  if (!this->option_.has_value()) {
    return false;
  }
  std::string_view arg = this->option_->arg_key;
  bool matches = arg.size() == 2 && arg[0] == '-' && arg[1] == option_shorthand;
  if (matches) {
    this->advance(1);
  }
  return matches;
}

bool arg_parser::match_flag_option(
    std::string_view full_option_name,
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

const char* arg_parser::match_argument() noexcept {
  if (this->option_.has_value()) {
    return nullptr;
  }
  return this->match_anything();
}

const char* arg_parser::match_anything() noexcept {
  const char* anything = this->current_arg();
  this->advance(1);
  return anything;
}

bool arg_parser::done() const noexcept {
  return this->current_arg_index_ >= this->argc_;
}

void arg_parser::parse_current_arg() noexcept {
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
  } else if (this->current_arg() == "-"sv) {
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

void arg_parser::advance(int count) noexcept {
  this->current_arg_index_ += count;
  this->parse_current_arg();
}

const char* arg_parser::current_arg() noexcept {
  QLJS_ASSERT(this->current_arg_index_ < this->argc_);
  return this->argv_[this->current_arg_index_];
}
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
