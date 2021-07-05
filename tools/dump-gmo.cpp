// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/leaf/handle_errors.hpp>
#include <boost/leaf/result.hpp>
#include <cstdlib>
#include <iostream>
#include <quick-lint-js/file.h>
#include <quick-lint-js/gmo.h>

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  if (argc != 2) {
    std::cerr << "error: expected file name\n";
    std::exit(EXIT_FAILURE);
  }

  const char* gmo_path = argv[1];
  boost::leaf::try_handle_all(
      [&]() -> boost::leaf::result<void> {
        boost::leaf::result<padded_string> gmo_data = read_file_2(gmo_path);
        if (!gmo_data) return gmo_data.error();

        quick_lint_js::gmo_file gmo(gmo_data->data());

        for (quick_lint_js::gmo_file::word_type i = 0; i < gmo.string_count();
             ++i) {
          std::string_view original = gmo.original_string_at(i);
          std::string_view translated = gmo.translated_string_at(i);
          std::cerr << original << "\n  -> " << translated << '\n';

          std::string_view translated_by_lookup = gmo.find_translation(
              quick_lint_js::gmo_message(original.data(), original.size()));
          if (translated_by_lookup != translated) {
            std::cerr << "    !!! error: lookup returned instead: "
                      << translated_by_lookup << '\n';
          }
        }
        return {};
      },
      exit_on_read_file_error_handlers<void>(gmo_path));

  return EXIT_SUCCESS;
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
