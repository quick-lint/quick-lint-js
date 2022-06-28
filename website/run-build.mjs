// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "fs";
import path from "path";
import { websiteConfig } from "./src/config.mjs";
import {
  IndexConflictVFSError,
  MalformedDirectoryURIError,
  ServerConfigVFSFile,
  VFS,
  VFSDirectory,
} from "./src/vfs.mjs";

async function mainAsync() {
  let { targetDirectory } = parseArguments(process.argv.slice(2));

  let wwwRootPath = websiteConfig.wwwRootPath;
  let vfs = new VFS(wwwRootPath);
  await buildDirAsync(vfs, "/", targetDirectory);

  process.exit(0);
}

async function buildDirAsync(vfs, uri, outputDirectory) {
  let listing = await vfs.listDirectoryAsync(uri);
  await fs.promises.mkdir(outputDirectory, { recursive: true });
  for (let childName of listing.names()) {
    let child = listing.get(childName);
    let childPath = path.join(
      outputDirectory,
      childName === "" ? "index.html" : childName
    );
    if (child instanceof VFSDirectory) {
      await buildDirAsync(vfs, `${uri}${childName}/`, childPath);
    } else if (child instanceof IndexConflictVFSError) {
      throw child;
    } else {
      console.log(`generating ${childPath} ...`);
      let data = await child.getContentsAsync();
      await fs.promises.writeFile(childPath, data);
    }
  }
}

function parseArguments(args) {
  let targetDirectory;
  switch (args.length) {
    case 1:
      targetDirectory = args[0];
      break;

    default:
      throw new Error("Expected exactly 1 argument");
  }
  return { targetDirectory };
}

mainAsync().catch((error) => {
  console.error(error.stack);
  process.exit(1);
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
