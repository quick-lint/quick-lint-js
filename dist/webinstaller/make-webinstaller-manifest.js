// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

const fs = require("fs");

const args = process.argv.slice(2);
const releaseVersion = process.argv[3];
const releaseDate = process.argv[5];
const outputFile = process.argv[7];

if (!args.includes("-releaseVersion")) {
  console.log("error: missing -releaseVersion");
  return 0;
}
if (!args.includes("-releaseDate")) {
  console.log("error: missing -releaseDate");
  return 0;
}
if (!args.includes("-Out")) {
  console.log("error: missing -Out");
  return 0;
}

const releaseVersionRegex = /^\d+\.\d+\.\d+$/;
if (!releaseVersionRegex.test(releaseVersion)) {
  console.log(
    "error: invalid -releaseVersion; must match regular expression: " +
      releaseVersionRegex
  );
  return 0;
}

const releasesDateRegex = /^\d{4}-\d{2}-\d{2}$/;
if (!releasesDateRegex.test(releaseDate)) {
  console.log(
    "error: invalid -releaseDate; must match regular expression: " +
      releasesDateRegex
  );
  return 0;
}

let data = fs.readFileSync("quick-lint-js-template.json", "utf8");
data = data.replace(/{version}/g, releaseVersion);
data = data.replace(/\${releaseDate}/g, releaseDate);
fs.writeFileSync(outputFile, data, "utf8");

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
