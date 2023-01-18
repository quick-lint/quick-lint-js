// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import { parseTimestamp } from "../../src/timestamp.mjs";

export let routes = {
  "/blog/feed.xml": {
    type: "build-ejs",
    path: "blog/feed.ejs.xml",
    contentType: "application/rss+xml",
  },
};

export let customComponents = {
  "qljs-date": qljsDate,
};

// <qljs-date datetime="2022-05-25T21:04:02-07:00" />
//
// Create a <time> containing a date string (without time).
function qljsDate(attributes, { currentURI }) {
  let timestamp = parseTimestamp(attributes.datetime);
  return `<time>${timestamp.date}</time>`;
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
