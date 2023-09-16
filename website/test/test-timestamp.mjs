// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "node:assert/strict";
import { datestampToRFC822, parseTimestamp } from "../src/timestamp.mjs";
import { describe, it } from "node:test";

describe("parse timestamp", () => {
  it("gives date in local timezone", () => {
    // This date is 2022-05-26 in UTC.
    assert.equal(
      parseTimestamp("2022-05-25T21:04:02-07:00").date,
      "2022-05-25"
    );
  });

  it("gives RFC 822 timestamp in local timezone without day of week", () => {
    // This date is 2022-05-26 in UTC.
    assert.equal(
      parseTimestamp("2022-05-25T21:04:02-07:00").rfc822,
      "25 May 2022 21:04:02 -0700"
    );
  });
});

describe("datestampToRFC822", () => {
  it("converts date to RFC 822 format in UTC timezone without day of week", () => {
    assert.equal(datestampToRFC822("2022-05-25"), "25 May 2022 00:00:00 Z");
  });
});

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
