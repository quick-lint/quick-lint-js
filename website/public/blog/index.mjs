// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "node:fs";
import path from "node:path";
import url from "node:url";
import { parseTimestamp } from "../../src/timestamp.mjs";
import { readFrontMatterFromFileAsync } from "../../src/front-matter.mjs";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

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

export async function loadBlogPostsAsync() {
  let directories = await fs.promises.readdir(__dirname, {
    withFileTypes: true,
  });
  directories = directories.filter((dir) => dir.isDirectory());
  let posts = await Promise.all(
    directories.map(async (dir) => {
      let indexPath = path.join(__dirname, dir.name, "index.ejs.html");
      let meta = await readFrontMatterFromFileAsync(indexPath);
      if (typeof meta.blogDate === "undefined") {
        throw new Error(`Missing blogDate in front matter of ${indexPath}`);
      }
      if (typeof meta.navTitle === "undefined") {
        throw new Error(`Missing navTitle in front matter of ${indexPath}`);
      }
      return {
        dir: dir.name,
        meta: meta,
        timestamp: parseTimestamp(meta.blogDate),
      };
    })
  );
  sortPostsReverseChronologically(posts);
  return posts;
}

function sortPostsReverseChronologically(posts) {
  posts.sort((a, b) => {
    if (a.meta.blogDate < b.meta.blogDate) return +1;
    if (a.meta.blogDate > b.meta.blogDate) return -1;
    return 0;
  });
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
