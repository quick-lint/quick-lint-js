// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

//==============================================================================
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                             sublime text utils                             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#ifndef QUICK_LINT_JS_SUBLIME_TEXT_UTILS_H
#define QUICK_LINT_JS_SUBLIME_TEXT_UTILS_H

#include <algorithm>

namespace quick_lint_js {
namespace sublime_text {

// Like std::transform with an std::back_insert_iterator, but more efficient for
// std::vector<int>.
template <class InputIt, class Output, class Transformer>
void insert_back_transform(InputIt input_begin, InputIt input_end,
                           Output &output, Transformer &&transformer) {
  using difference_type = typename Output::difference_type;
  std::size_t original_size = output.size();
  std::size_t input_size = narrow_cast<std::size_t>(input_end - input_begin);
  std::size_t final_size = original_size + input_size;
  output.resize(final_size);
  auto output_it = output.begin() + narrow_cast<difference_type>(original_size);
  output_it = std::transform(input_begin, input_end, output_it, transformer);
  QLJS_ASSERT(output_it == output.end());
}

}  // namespace sublime_text
}  // namespace quick_lint_js
#endif  // QUICK_LINT_JS_SUBLIME_TEXT_UTILS_H

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
