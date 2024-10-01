// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

const fs = require("fs");
const path = require("node:path");

const args = process.argv.slice(2);
let releaseVersion;
let releaseDate;
let outputFile;

args.forEach((arg, index) => {
  switch (arg) {
    case "-releaseVersion":
      releaseVersion = args[index + 1];
      break;
    case "-releaseDate":
      releaseDate = args[index + 1];
      break;
    case "-Out":
      outputFile = args[index + 1];
      break;
    default:
      break;
  }
});

if (!releaseVersion) {
  console.log("error: missing -releaseVersion");
  process.exit(1);
}
if (!releaseDate) {
  console.log("error: missing -releaseDate");
  process.exit(1);
}
if (!outputFile) {
  console.log("error: missing -outputFile");
  process.exit(1);
}

const releaseVersionRegex = /^\d+\.\d+\.\d+$/;
if (!releaseVersionRegex.test(releaseVersion)) {
  console.log(
    "error: invalid -releaseVersion; must match regular expression: " +
      releaseVersionRegex
  );
  process.exit(1);
}

const releasesDateRegex = /^\d{4}-\d{2}-\d{2}$/;
if (!releasesDateRegex.test(releaseDate)) {
  console.log(
    "error: invalid -releaseDate; must match regular expression: " +
      releasesDateRegex
  );
  process.exit(1);
}

let data = fs.readFileSync(
  path.join(__dirname, "quick-lint-js-template.json"),
  "utf8"
);
data = data.replace(/\${version}/g, releaseVersion);
data = data.replace(/\${releaseDate}/g, releaseDate);
fs.writeFileSync(outputFile, data, "utf8");

//Global manifest
const versionSpecific = fs.readFileSync(
  path.join(__dirname, "quick-lint-js.json"),
  "utf8"
);

const allVersions = fs.readFileSync(
  path.join(__dirname, "all-versions-manifest/all-versions-manifest.json"),
  "utf8"
);

const versionSpecificData = JSON.parse(versionSpecific);
const allVersionsData = JSON.parse(allVersions);

const concatenatedArray = allVersionsData.releases.concat(
  versionSpecificData.releases
);

allVersionsData.releases = concatenatedArray;

const updatedAllVersionsContent = JSON.stringify(allVersionsData, null, 2);

fs.writeFileSync(
  "all-versions-manifest/all-versions-manifest.json",
  updatedAllVersionsContent,
  "utf8"
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
