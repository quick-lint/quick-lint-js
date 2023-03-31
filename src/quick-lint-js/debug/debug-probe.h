// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DEBUG_DEBUG_PROBE_H
#define QUICK_LINT_JS_DEBUG_DEBUG_PROBE_H

namespace quick_lint_js {
template <class Data>
class synchronized;
struct lsp_documents;

// Call this occasionally after modifying lsp_documents.
//
// If the debug server is enabled, this function will push document data to
// opened browsers.
void debug_probe_publish_lsp_documents();

// Call this occasionally after using quick_lint_js::vector.
//
// If the debug server is enabled, this function will push vector profiling
// statistics to opened browsers.
void debug_probe_publish_vector_profile();
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
