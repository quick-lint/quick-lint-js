#!/usr/bin/env node

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

let fs = require("fs");
let path = require("path");

let platformToExecutable = {
  darwin: path.join("macos", "bin", "quick-lint-js"),
  linux: path.join("linux", "bin", "quick-lint-js"),
  win32: path.join("windows", "bin", "quick-lint-js.exe"),
};
let platformExecutable = platformToExecutable[process.platform];
if (typeof platformExecutable === "undefined") {
  console.error(
    `fatal: cannot install quick-lint-js on unsupported platform ${process.platform}`
  );
  process.exit(1);
}

fs.copyFileSync(
  path.join(__dirname, platformExecutable),
  path.join(__dirname, "quick-lint-js.exe"),
  fs.constants.COPYFILE_FICLONE
);
fs.chmodSync(
  path.join(__dirname, "quick-lint-js.exe"),
  0o755,
);
