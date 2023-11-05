// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <cstdlib>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/i18n/po-parser.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/unreachable.h>

namespace quick_lint_js {
bool operator==(const PO_Entry& lhs, const PO_Entry& rhs) {
  return lhs.msgid == rhs.msgid && lhs.msgstr == rhs.msgstr &&
         lhs.is_fuzzy == rhs.is_fuzzy;
}

bool operator!=(const PO_Entry& lhs, const PO_Entry& rhs) {
  return !(lhs == rhs);
}

class PO_Parser {
 public:
  explicit PO_Parser(Padded_String_View code, const char* file_path,
                     CLI_Locator* locator, Monotonic_Allocator* allocator)
      : input_(code.data()),
        file_path_(file_path),
        locator_(locator),
        allocator_(allocator) {}

  void parse_file() {
    this->skip_whitespace();
    while (this->parse_directive()) {
      this->skip_whitespace();
    }
  }

  Span<PO_Entry> entries() { return Span<PO_Entry>(this->entries_); }

  bool parse_directive() {
  again:
    switch (*this->input_) {
    case u8'\0':
      // End of file.
      // TODO(strager): Check for null bytes in the middle of the file.
      return false;

    case u8'm':
      if (this->try_skip_word(u8"msgid"_sv)) {
        PO_Entry& entry = this->entries_.emplace_back();
        entry.is_fuzzy = std::exchange(this->is_next_entry_fuzzy_, false);
        this->skip_whitespace();
        entry.msgid = this->parse_string();
      } else if (this->try_skip_word(u8"msgstr"_sv)) {
        if (this->entries_.empty()) {
          this->fatal();
        }
        PO_Entry& entry = this->entries_.back();
        this->skip_whitespace();
        entry.msgstr = this->parse_string();
      }
      return true;

    case u8'#':
      this->parse_comment();
      goto again;

    default:
      this->fatal();
      QLJS_UNREACHABLE();
    }
  }

  bool try_skip_word(String8_View word) {
    bool match = std::equal(word.begin(), word.end(), this->input_);
    if (match) {
      this->input_ += word.size();
    }
    return match;
  }

  void parse_comment() {
    QLJS_ASSERT(*this->input_ == u8'#');
    this->input_ += 1;
    switch (*this->input_) {
    case u8',':
      this->input_ += 1;
      this->skip_horizontal_whitespace();
      if (this->try_skip_word(u8"fuzzy"_sv)) {
        this->is_next_entry_fuzzy_ = true;
      } else {
        this->fatal();
      }
      break;

    default:
      break;
    }
    this->skip_to_end_of_line();
    this->skip_whitespace();
  }

  void skip_whitespace() {
    // See <gnu-gettext>/gettext-tools/src/po-lex.c.
    while (u8" \f\n\r\t\v"_sv.find(*this->input_) != String8_View::npos) {
      this->input_ += 1;
    }
  }

  void skip_horizontal_whitespace() {
    // FIXME(strager): I haven't tested this, but these are the characters
    // which GNU gettext seems to use as whitespace in comments. See
    // <gnu-gettext>/gettext-tools/src/read-catalog-abstract.c.
    while (u8" \f\r\t\v"_sv.find(*this->input_) != String8_View::npos) {
      this->input_ += 1;
    }
  }

  void skip_to_end_of_line() {
    while (u8"\0\n"_sv.find(*this->input_) == String8_View::npos) {
      this->input_ += 1;
    }
  }

  String8_View parse_string() {
    if (*this->input_ != u8'"') {
      this->fatal();
    }
    this->input_ += 1;
    Bump_Vector<Char8> decoded("parse_string", this->allocator_);
    for (;;) {
      switch (*this->input_) {
      case u8'"': {
        this->input_ += 1;
        this->skip_whitespace();

        if (*this->input_ == u8'"') {
          // Concatenated strings: "abc" "def"
          this->input_ += 1;
          continue;
        }

        return decoded.release_to_string_view();
      }

      case u8'\\':
        // See <gnu-gettext>/gettext-tools/src/po-lex.c.
        switch (this->input_[1]) {
#define QLJS_CASE_ESCAPE_CODE(code, output) \
  case (code):                              \
    decoded += (output);                    \
    this->input_ += 2;                      \
    break;

          QLJS_CASE_ESCAPE_CODE(u8'"', u8'\"')
          QLJS_CASE_ESCAPE_CODE(u8'\\', u8'\\')
          QLJS_CASE_ESCAPE_CODE(u8'a', u8'\a')
          QLJS_CASE_ESCAPE_CODE(u8'b', u8'\b')
          QLJS_CASE_ESCAPE_CODE(u8'f', u8'\f')
          QLJS_CASE_ESCAPE_CODE(u8'n', u8'\n')
          QLJS_CASE_ESCAPE_CODE(u8'r', u8'\r')
          QLJS_CASE_ESCAPE_CODE(u8't', u8'\t')
          QLJS_CASE_ESCAPE_CODE(u8'v', u8'\v')

#undef QLJS_CASE_ESCAPE_CODE

        case u8'0':
        case u8'1':
        case u8'2':
        case u8'3':
        case u8'4':
        case u8'5':
        case u8'6':
        case u8'7':
          // Octal escape (e.g. \007).
          // TODO(strager)
          this->fatal();
          break;

        case u8'x':
          // Hex escape (e.g. \x07 or \xffef).
          // TODO(strager)
          this->fatal();
          break;

        default:
          // Invalid escape sequence.
          this->fatal();
          break;
        }
        break;

      default:
        decoded += *this->input_;
        this->input_ += 1;
        break;

      case u8'\0':
        this->fatal();
        break;
      }
    }
  }

  [[noreturn]] void fatal() {
    CLI_Source_Position p = this->locator_->position(this->input_);
    std::fprintf(stderr, "%s:%d:%d: error: failed to parse\n", this->file_path_,
                 p.line_number, p.column_number);
    std::exit(1);
  }

  const Char8* input_;
  const char* file_path_;
  const CLI_Locator* locator_;
  Monotonic_Allocator* allocator_;

  Bump_Vector<PO_Entry> entries_{"PO_Parser::entries", this->allocator_};

  bool is_next_entry_fuzzy_ = false;
};

Span<PO_Entry> parse_po_file(Padded_String_View code, const char* file_path,
                             CLI_Locator* locator,
                             Monotonic_Allocator* allocator) {
  PO_Parser parser(code, file_path, locator, allocator);
  parser.parse_file();
  return parser.entries();
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
