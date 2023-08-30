// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import child_process from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import tar from "tar";
import url from "node:url";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

let websiteAnalyticsRoot = __dirname;

async function mainAsync() {
  if (process.argv.length !== 3) {
    throw new Error(`usage: ${process.argv[1]} output-file.tar.gz`);
  }
  let outputPath = process.argv[2];

  let tempDir = await fs.promises.mkdtemp(os.tmpdir() + path.sep);
  try {
    await fs.promises.mkdir(path.join(tempDir, "src"));
    for (let fileName of await fs.promises.readdir(
      path.join(websiteAnalyticsRoot, "src")
    )) {
      await fs.promises.copyFile(
        path.join(websiteAnalyticsRoot, "src", fileName),
        path.join(tempDir, "src", fileName)
      );
    }
    await fs.promises.copyFile(
      path.join(websiteAnalyticsRoot, "yarn.lock"),
      path.join(tempDir, "yarn.lock")
    );

    let nodePackage = JSON.parse(
      await fs.promises.readFile(
        path.join(websiteAnalyticsRoot, "package.json"),
        "utf-8"
      )
    );
    // HACK(strager): Prevent Yarn from freaking out at relative paths in
    // devDependencies.
    delete nodePackage.devDependencies;
    await fs.promises.writeFile(
      path.join(tempDir, "package.json"),
      JSON.stringify(nodePackage, null, 2)
    );

    child_process.execSync(
      "yarn install --ignore-scripts --frozen-lockfile --production",
      {
        cwd: tempDir,
      }
    );

    await tar.c(
      {
        gzip: true,
        file: outputPath,
        cwd: tempDir,
      },
      ["."]
    );
  } finally {
    await fs.promises.rm(tempDir, { recursive: true });
  }
}

mainAsync().catch((e) => {
  console.error(e.stack || e);
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
