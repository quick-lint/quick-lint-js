// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diagnostic-types.h>
#include <string>
#include <unordered_map>

namespace quick_lint_js {
namespace {
struct diag_name_and_code {
  const char* name;
  const char* code;
};
static constexpr diag_name_and_code all_diags[] = {
#define QLJS_DIAG_TYPE(diag_name, diag_code, severity, struct_body, format) \
  {.name = #diag_name, .code = diag_code},
    QLJS_X_DIAG_TYPES
#undef QLJS_DIAG_TYPE
};

std::string next_unused_diag_code() {
  for (int i = 1; i <= 9999; ++i) {
    char code[7];
    std::snprintf(code, sizeof(code), "E%04d", i);
    auto existing_it =
        std::find_if(std::begin(all_diags), std::end(all_diags),
                     [&](const diag_name_and_code& diag) {
                       return std::string_view(diag.code) == code;
                     });
    if (existing_it == std::end(all_diags)) {
      return std::string(code);
    }
  }
  QLJS_UNIMPLEMENTED();
}

TEST(test_diag_code, diag_codes_are_unique) {
  std::unordered_map<std::string, const char*> code_to_diag_name;
  for (const diag_name_and_code& diag : all_diags) {
    auto existing_it = code_to_diag_name.find(diag.code);
    if (existing_it == code_to_diag_name.end()) {
      code_to_diag_name.emplace(diag.code, diag.name);
    } else {
      ADD_FAILURE() << "diag code " << diag.code
                    << " used for multiple diags: " << diag.name << ", "
                    << existing_it->second << "\ntry this unused diag code: "
                    << next_unused_diag_code();
    }
  }
}

TEST(test_diag_code, diag_codes_are_well_formed) {
  for (const diag_name_and_code& diag : all_diags) {
#if defined(_WIN32)
    constexpr const char* code_pattern = R"(^E\d\d\d\d$)";
#else
    constexpr const char* code_pattern = R"(^E[0-9][0-9][0-9][0-9]$)";
#endif
    // Wrapping the code in std::string improves gtest diagnostics.
    EXPECT_THAT(std::string(diag.code), ::testing::MatchesRegex(code_pattern))
        << "diag " << diag.name << " should have a code like E1234"
        << "\ntry this unused diag code: " << next_unused_diag_code();
  }
}
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
