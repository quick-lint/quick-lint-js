// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <quick-lint-js/diagnostic.h>
#include <quick-lint-js/error-list.h>
#include <quick-lint-js/error.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
namespace {
template <class Container, class T>
bool contains(const Container& container, const T& item) {
  using std::begin;
  using std::end;
  return std::find(begin(container), end(container), item) != end(container);
}
}

bool parsed_error_list::error_missing_predicate() const noexcept {
  return this->included_codes.empty() && this->excluded_codes.empty() &&
         this->included_categories.empty() && this->excluded_categories.empty();
}

parsed_error_list parse_error_list(const char* const raw_error_list) {
  static auto is_initial_category_character = [](char c) -> bool {
    return 'a' <= c && c <= 'z';
  };
  static auto is_continue_category_character = [](char c) -> bool {
    return is_initial_category_character(c) || c == '-';
  };
  static auto is_continue_code_character = [](char c) -> bool {
    return '0' <= c && c <= '9';
  };

  parsed_error_list errors;
  std::size_t i = 0;
  bool need_comma = false;

  auto try_parse_category_or_code = [&](bool is_include) -> bool {
    auto parse_word = [&](auto& is_continue_character) -> std::string_view {
      std::size_t begin = i;
      i += 1;  // Skip initial character. Assume it is valid.
      while (is_continue_character(raw_error_list[i])) {
        i += 1;
      }
      std::size_t end = i;
      return std::string_view(&raw_error_list[begin], end - begin);
    };

    if (raw_error_list[i] == 'E') {
      (is_include ? errors.included_codes : errors.excluded_codes)
          .emplace_back(parse_word(is_continue_code_character));
      return true;
    } else if (is_initial_category_character(raw_error_list[i])) {
      (is_include ? errors.included_categories : errors.excluded_categories)
          .emplace_back(parse_word(is_continue_category_character));
      return true;
    } else {
      errors.unexpected.emplace_back(&raw_error_list[i], 1);
      return false;
    }
  };

  for (;;) {
    i = i + std::strspn(&raw_error_list[i], " \t");
    if (raw_error_list[i] == '\0') {
      break;
    }
    if (need_comma && raw_error_list[i] != ',') {
      errors.unexpected.emplace_back(&raw_error_list[i], 1);
      break;
    }
    i = i + std::strspn(&raw_error_list[i], " \t,");
    need_comma = true;

    if (raw_error_list[i] == '\0') {
      break;
    } else if (raw_error_list[i] == '+' || raw_error_list[i] == '-') {
      bool is_include = raw_error_list[i] == '+';
      i += 1;
      if (!try_parse_category_or_code(/*is_include=*/is_include)) {
        break;
      }
    } else {
      if (!try_parse_category_or_code(/*is_include=*/true)) {
        break;
      }
      errors.override_defaults = true;
    }
  }

  return errors;
}

void compiled_error_list::add(const parsed_error_list& error_list) {
  auto add_code = [this](std::string_view code, auto& code_set) -> void {
    std::optional<diag_type> code_error_type = error_type_from_code_slow(code);
    if (code_error_type.has_value()) {
      code_set[static_cast<std::size_t>(*code_error_type)] = true;
    } else {
      this->unknown_codes_.emplace_back(code);
    }
  };

  codes& c = this->parsed_error_lists_.emplace_back();
  for (std::string_view code : error_list.included_codes) {
    add_code(code, c.included_codes);
  }
  for (std::string_view code : error_list.excluded_codes) {
    add_code(code, c.excluded_codes);
  }
  c.included_categories = error_list.included_categories;
  c.excluded_categories = error_list.excluded_categories;
  c.override_defaults = error_list.override_defaults;

  if (error_list.error_missing_predicate()) {
    this->has_missing_predicate_error_ = true;
  }
}

std::vector<std::string> compiled_error_list::parse_errors(
    std::string_view cli_option_name) const {
  std::vector<std::string> errors;
  if (this->has_missing_predicate_error_) {
    errors.emplace_back(std::string(cli_option_name) +
                        " must be given at least one category or code");
  }
  return errors;
}

std::vector<std::string> compiled_error_list::parse_warnings() const {
  std::vector<std::string> warnings;
  auto check_category = [&warnings](std::string_view category) {
    if (category != "all") {
      warnings.emplace_back("unknown error category: ");
      warnings.back().append(category);
    }
  };

  for (const codes& c : this->parsed_error_lists_) {
    for (std::string_view category : c.included_categories) {
      check_category(category);
    }
    for (std::string_view category : c.excluded_categories) {
      check_category(category);
    }
  }

  for (std::string_view code : this->unknown_codes_) {
    warnings.emplace_back("unknown error code: ");
    warnings.back().append(code);
  }

  return warnings;
}

bool compiled_error_list::is_present(diag_type type) const noexcept {
  bool is_default = true;  // For now, all codes are enabled by default.
  bool present = true;
  for (const codes& c : this->parsed_error_lists_) {
    std::size_t error_type_index = static_cast<std::size_t>(type);
    if (c.override_defaults || c.excluded_codes[error_type_index] ||
        (is_default && contains(c.excluded_categories, "all"))) {
      present = false;
    }
    if (c.included_codes[error_type_index] ||
        (is_default && contains(c.included_categories, "all"))) {
      present = true;
    }
  }
  return present;
}

bool compiled_error_list::is_user_provided() const noexcept {
  return !parsed_error_lists_.empty();
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
