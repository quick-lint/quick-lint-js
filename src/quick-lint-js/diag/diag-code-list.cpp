// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <quick-lint-js/diag/diag-code-list.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/util/algorithm.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
bool Parsed_Diag_Code_List::error_missing_predicate() const {
  return this->included_codes.empty() && this->excluded_codes.empty() &&
         this->included_categories.empty() && this->excluded_categories.empty();
}

Parsed_Diag_Code_List parse_diag_code_list(const char* const raw_diag_code_list,
                                           Monotonic_Allocator* allocator) {
  static auto is_initial_category_character = [](char c) -> bool {
    return 'a' <= c && c <= 'z';
  };
  static auto is_continue_category_character = [](char c) -> bool {
    return is_initial_category_character(c) || c == '-';
  };
  static auto is_continue_code_character = [](char c) -> bool {
    return '0' <= c && c <= '9';
  };

  Vector<std::string_view> included_codes("included_codes", allocator);
  Vector<std::string_view> excluded_codes("excluded_codes", allocator);
  Vector<std::string_view> included_categories("included_categories",
                                               allocator);
  Vector<std::string_view> excluded_categories("excluded_categories",
                                               allocator);
  Vector<std::string_view> unexpected("unexpected", allocator);
  bool override_defaults = false;

  std::size_t i = 0;
  bool need_comma = false;

  auto try_parse_category_or_code = [&](bool is_include) -> bool {
    auto parse_word = [&](auto& is_continue_character) -> std::string_view {
      std::size_t begin = i;
      i += 1;  // Skip initial character. Assume it is valid.
      while (is_continue_character(raw_diag_code_list[i])) {
        i += 1;
      }
      std::size_t end = i;
      return std::string_view(&raw_diag_code_list[begin], end - begin);
    };

    if (raw_diag_code_list[i] == 'E') {
      (is_include ? included_codes : excluded_codes)
          .emplace_back(parse_word(is_continue_code_character));
      return true;
    } else if (is_initial_category_character(raw_diag_code_list[i])) {
      (is_include ? included_categories : excluded_categories)
          .emplace_back(parse_word(is_continue_category_character));
      return true;
    } else {
      unexpected.emplace_back(&raw_diag_code_list[i], std::size_t{1});
      return false;
    }
  };

  for (;;) {
    i = i + std::strspn(&raw_diag_code_list[i], " \t");
    if (raw_diag_code_list[i] == '\0') {
      break;
    }
    if (need_comma && raw_diag_code_list[i] != ',') {
      unexpected.emplace_back(&raw_diag_code_list[i], std::size_t{1});
      break;
    }
    i = i + std::strspn(&raw_diag_code_list[i], " \t,");
    need_comma = true;

    if (raw_diag_code_list[i] == '\0') {
      break;
    } else if (raw_diag_code_list[i] == '+' || raw_diag_code_list[i] == '-') {
      bool is_include = raw_diag_code_list[i] == '+';
      i += 1;
      if (!try_parse_category_or_code(/*is_include=*/is_include)) {
        break;
      }
    } else {
      if (!try_parse_category_or_code(/*is_include=*/true)) {
        break;
      }
      override_defaults = true;
    }
  }

  return Parsed_Diag_Code_List{
      .included_codes = included_codes.release_to_span(),
      .excluded_codes = excluded_codes.release_to_span(),
      .included_categories = included_categories.release_to_span(),
      .excluded_categories = excluded_categories.release_to_span(),
      .unexpected = unexpected.release_to_span(),
      .override_defaults = override_defaults,
  };
}

Compiled_Diag_Code_List::Compiled_Diag_Code_List()
    : allocator_(
          std::make_unique<Monotonic_Allocator>("Compiled_Diag_Code_List")) {}

void Compiled_Diag_Code_List::add(const Parsed_Diag_Code_List& diag_code_list) {
  auto add_code = [this](std::string_view code, auto& code_set) -> void {
    std::optional<Diag_Type> code_diag_type = diag_type_from_code_slow(code);
    if (code_diag_type.has_value()) {
      code_set[static_cast<std::size_t>(*code_diag_type)] = true;
    } else {
      this->unknown_codes_.emplace_back(code);
    }
  };

  Codes& c = this->parsed_diag_code_lists_.emplace_back();
  for (std::string_view code : diag_code_list.included_codes) {
    add_code(code, c.included_codes);
  }
  for (std::string_view code : diag_code_list.excluded_codes) {
    add_code(code, c.excluded_codes);
  }
  // TODO(strager): Copy the referenced std::string_view-s.
  c.included_categories =
      this->allocator_->new_objects_copy(diag_code_list.included_categories);
  c.excluded_categories =
      this->allocator_->new_objects_copy(diag_code_list.excluded_categories);
  c.override_defaults = diag_code_list.override_defaults;

  if (diag_code_list.error_missing_predicate()) {
    this->has_missing_predicate_error_ = true;
  }
}

Span<std::string_view> Compiled_Diag_Code_List::parse_errors(
    std::string_view cli_option_name, Monotonic_Allocator* allocator) const {
  Vector<std::string_view> errors("errors", allocator);
  if (this->has_missing_predicate_error_) {
    // TODO(#1102): Make this code pretty.
    Vector<char> error("error", allocator);
    error += cli_option_name;
    error += " must be given at least one category or code"sv;
    errors.emplace_back(error.release_to_string_view());
  }
  return errors.release_to_span();
}

Span<std::string_view> Compiled_Diag_Code_List::parse_warnings(
    Monotonic_Allocator* allocator) const {
  Vector<std::string_view> warnings("warnings", allocator);
  auto check_category = [&](std::string_view category) {
    if (category != "all") {
      // TODO(#1102): Make this code pretty.
      Vector<char> warning("warning", allocator);
      warning += "unknown error category: "sv;
      warning += category;
      warnings.emplace_back(warning.release_to_string_view());
    }
  };

  for (const Codes& c : this->parsed_diag_code_lists_) {
    for (std::string_view category : c.included_categories) {
      check_category(category);
    }
    for (std::string_view category : c.excluded_categories) {
      check_category(category);
    }
  }

  for (std::string_view code : this->unknown_codes_) {
    // TODO(#1102): Make this code pretty.
    Vector<char> warning("warning", allocator);
    warning += "unknown error code: "sv;
    warning += code;
    warnings.emplace_back(warning.release_to_string_view());
  }

  return warnings.release_to_span();
}

bool Compiled_Diag_Code_List::is_present(Diag_Type type) const {
  bool is_default = true;  // For now, all codes are enabled by default.
  bool present = true;
  for (const Codes& c : this->parsed_diag_code_lists_) {
    std::size_t diag_type_index = static_cast<std::size_t>(type);
    if (c.override_defaults || c.excluded_codes[diag_type_index] ||
        (is_default && contains(c.excluded_categories, "all"))) {
      present = false;
    }
    if (c.included_codes[diag_type_index] ||
        (is_default && contains(c.included_categories, "all"))) {
      present = true;
    }
  }
  return present;
}

bool Compiled_Diag_Code_List::is_user_provided() const {
  return !parsed_diag_code_lists_.empty();
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
