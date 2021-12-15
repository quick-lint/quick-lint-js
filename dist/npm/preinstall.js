#!/usr/bin/env node

// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

let fs = require("fs");
let path = require("path");

let platformToRelativeExecutable = {
  "darwin": path.join("bin", "quick-lint-js"),
  "linux": path.join("bin", "quick-lint-js"),
  "win32": path.join("bin", "quick-lint-js.exe"),
};
let platformAndArch = `${process.platform}-${process.arch}`;
let relativeExecutable = platformToRelativeExecutable[process.platform];
if (typeof relativeExecutable === "undefined") {
  console.error(
    `fatal: cannot install quick-lint-js on unsupported platform ${process.platform}`
  );
  process.exit(1);
}

fs.copyFileSync(
  path.join(__dirname, platformAndArch, relativeExecutable),
  path.join(__dirname, "quick-lint-js.exe"),
  fs.constants.COPYFILE_FICLONE
);
fs.chmodSync(
  path.join(__dirname, "quick-lint-js.exe"),
  0o755,
);

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
