// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

class Position {
  constructor(line, character) {
    this.line = line;
    this.character = character;
  }
}
exports.Position = Position;

class Range {
  constructor(start, end) {
    this.start = start;
    this.end = end;
  }
}
exports.Range = Range;

class Diagnostic {
  constructor(range, message, severity) {
    this.message = message;
    this.range = range;
    this.severity = severity;
  }
}
exports.Diagnostic = Diagnostic;

let DiagnosticSeverity = {
  Error: "Error",
  Warning: "Warning",
  Information: "Information",
  Hint: "Hint",
};
exports.DiagnosticSeverity = DiagnosticSeverity;

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
