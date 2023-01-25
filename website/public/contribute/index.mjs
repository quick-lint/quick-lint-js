// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "node:fs";
import path from "node:path";
import url from "node:url";
import { readFrontMatterFromFileAsync } from "../../src/front-matter.mjs";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

export async function loadNavSubpagesAsync() {
  let root = path.join(__dirname, "..");
  return [
    ...(await loadNavSubpageAndSubsubpagesAsync({
      root: root,
      uri: "/contribute/build-from-source/",
    })),
    ...(await loadNavSubpageAndSubsubpagesAsync({
      root: root,
      uri: "/contribute/create-diagnostic/",
    })),
  ];
}

async function loadNavSubpageAndSubsubpagesAsync({ root, uri }) {
  let directories = await fs.promises.readdir(path.join(root, uri), {
    withFileTypes: true,
  });
  directories = directories.filter((dir) => dir.isDirectory());
  let subsubpages = await Promise.all(
    directories.map(async (dir) => ({
      ...(await loadNavSinglePageAsync({
        root: root,
        uri: `${uri}${dir.name}/`,
      })),
      hidden: true,
    }))
  );
  return [
    await loadNavSinglePageAsync({ root: root, uri: uri }),
    ...subsubpages,
  ];
}

async function loadNavSinglePageAsync({ root, uri }) {
  let frontMatter = await readFrontMatterFromFileAsync(
    path.join(root, uri, "index.ejs.html")
  );
  return { uri: uri, title: frontMatter.navTitle, meta: frontMatter };
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
