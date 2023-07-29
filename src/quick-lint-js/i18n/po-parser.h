// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_I18N_PO_PARSER_H
#define QUICK_LINT_JS_I18N_PO_PARSER_H

#include <iosfwd>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>

namespace quick_lint_js {
class CLI_Locator;

// An entry in a .po file storing a translation.
struct PO_Entry {
  // The untranslated string; a key.
  String8_View msgid;
  // The translated string.
  String8_View msgstr;
  bool is_fuzzy = false;

  bool is_metadata() const { return this->msgid.empty(); }
  bool has_translation() const {
    return !this->is_fuzzy && !this->msgstr.empty();
  }

  friend bool operator==(const PO_Entry& lhs, const PO_Entry& rhs);
  friend bool operator!=(const PO_Entry& lhs, const PO_Entry& rhs);

  friend std::ostream& operator<<(std::ostream&, const PO_Entry&);
};

// A parsed .po translation strings file.
//
// .po files are used by the GNU gettext system to store translatable and
// translated strings. Format details:
// https://www.gnu.org/software/gettext/manual/html_node/PO-Files.html
struct PO_File {
  String8_View locale;
  Span<PO_Entry> entries;
};

// Parses a .po translation strings file.
Span<PO_Entry> parse_po_file(Padded_String_View code, const char* file_path,
                             CLI_Locator* locator,
                             Monotonic_Allocator* allocator);
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
