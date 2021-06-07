// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import Jasmine from "jasmine";
import colors from "colors";
import fs from "fs";
import path from "path";
import url from "url";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

function main() {
  let jasmine = new Jasmine();
  jasmine.loadConfig({
    spec_dir: path.relative("", __dirname),
    spec_files: ["!node_modules/**", "**/test-*.mjs"],
    stopSpecOnExpectationFailure: true,
    random: false,
  });
  jasmine.clearReporters();
  jasmine.addReporter(new BasicReporter());
  let { fileNames, testNames } = parseCommandLineOptions();
  jasmine.execute(fileNames, testNames);
}

function parseCommandLineOptions() {
  let options = process.argv.slice(2);

  let fileNames = [];
  let testNames = [];
  for (let option of options) {
    if (fs.existsSync(option)) {
      fileNames.push(option);
    } else {
      testNames.push(option);
    }
  }
  if (testNames.length > 1) {
    console.error(
      `${colors.red("ERROR")} only one test name filter is allowed`
    );
    process.exit(1);
  }
  return { fileNames, testNames };
}

class BasicReporter {
  constructor() {
    this._executedTests = 0;
  }

  jasmineStarted(_summary) {}

  jasmineDone(summary) {
    switch (summary.overallStatus) {
      case "passed":
        if (this._executedTests === 0) {
          console.error(`${colors.red("ERROR")} no tests executed`);
          process.exit(1);
        }
        break;

      case "incomplete":
        console.error(`${colors.red("ERROR")} ${summary.incompleteReason}`);
        process.exit(1);
        break;

      case "failed":
        // specDone should have killed the process already. But just in case...
        console.error(`${colors.red("ERROR")} tests failed`);
        process.exit(1);
        break;

      default:
        console.error(
          `${colors.red("ERROR")} unknown status: ${summary.overallStatus}`
        );
        process.exit(1);
        break;
    }
  }

  suiteStarted(_suite) {}

  suiteDone(_suite) {}

  specStarted(spec) {
    console.error(`Running ${spec.fullName} ...`);
  }

  specDone(spec) {
    switch (spec.status) {
      case "passed":
        console.error(`${colors.green("OK")}      ${spec.fullName}`);
        this._executedTests += 1;
        break;

      case "excluded":
        console.error(`${colors.yellow("SKIP")}    ${spec.fullName}`);
        break;

      default:
      case "failed":
        for (let failure of spec.failedExpectations) {
          console.error(failure.stack);
        }
        console.error(`${colors.red("FAIL")}    ${spec.fullName}`);
        process.exit(1);
        break;
    }
  }
}

main();

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
