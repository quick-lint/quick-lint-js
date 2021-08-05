// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

"use strict";

let colors = require("colors");

exports.testFilterEnvironmentVariable = "QLJS_TEST_FILTER";

async function testMainAsync(
  tests,
  onError = (message) => {
    console.error(message);
    process.exit(1);
  }
) {
  let filteredTests = filterTestsFromUserInput(tests);
  if (Object.keys(filteredTests).length === 0) {
    onError("no tests found");
  }
  try {
    await runTestsAsync(filteredTests);
  } catch (error) {
    if (!(error instanceof TestFailed)) {
      console.error(error.stack);
    }
    onError("test failed");
  }
}
exports.testMainAsync = testMainAsync;

async function runTestsAsync(tests) {
  for (let testName in tests) {
    if (Object.prototype.hasOwnProperty.call(tests, testName)) {
      await runTestAsync(testName, tests[testName]);
    }
  }
}

async function runTestAsync(testName, test) {
  console.log(`Running ${testName} ...`);

  let cleanupFunctions = [];
  function addCleanup(callback) {
    cleanupFunctions.push(callback);
  }

  let testError = null;
  let testPassed = false;
  try {
    await test({ addCleanup: addCleanup });
    testPassed = true;
  } catch (error) {
    testPassed = false;
    testError = error;
    if (error instanceof Error) {
      console.error(error.stack);
    } else {
      console.error(error);
    }
  } finally {
    for (let cleanupFunction of cleanupFunctions) {
      await cleanupFunction();
    }

    if (testPassed) {
      console.log(`${colors.green("OK")}      ${testName}`);
    } else {
      console.log(`${colors.red("FAIL")}    ${testName}`);
      // eslint-disable-next-line no-unsafe-finally
      throw new TestFailed(testError);
    }
  }
}

function filterTestsFromUserInput(tests) {
  let userFilter = process.env[exports.testFilterEnvironmentVariable];
  if (typeof userFilter === "undefined" || userFilter === "") {
    return tests;
  }
  let filterRegexp = new RegExp(userFilter);
  let result = {};
  for (let testName in tests) {
    if (Object.prototype.hasOwnProperty.call(tests, testName)) {
      if (filterRegexp.test(testName)) {
        result[testName] = tests[testName];
      }
    }
  }
  return result;
}

class TestFailed extends Error {
  constructor(error) {
    super(error instanceof Error ? error.message : String(error));
    this.error = error;
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
