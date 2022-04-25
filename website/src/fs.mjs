// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import child_process from "child_process";
import fs from "fs";
import path from "path";
import util from "util";

let execFileAsync = util.promisify(child_process.execFile);

// Like fs.promises.readFile, but fixes symlinks on Windows.
//
// Some of our files are symlinks. On Windows, if core.symlinks=false (default),
// Git creates text files instead of links. readFileAsync detects such files and
// manually follows the symlink.
export async function readFileAsync(filePath, encoding) {
  if (
    !(await isSymlinkInFileSystemAsync(filePath)) &&
    (await isSymlinkInGitAsync(filePath))
  ) {
    let symlinkPath = await fs.promises.readFile(filePath, "utf-8");
    let targetPath = path.join(path.dirname(filePath), symlinkPath);
    // TODO(strager): Avoid infinite recursion in case a symlink points to
    // another symlink.
    return await readFileAsync(targetPath, encoding);
  }
  return await fs.promises.readFile(filePath, encoding);
}

// Asks whether Git thinks the given file is a symbolic link.
async function isSymlinkInGitAsync(filePath) {
  // Git constants:
  let S_IFLNK = 0x120000;
  let S_IFMT = 0x170000;

  try {
    let { stdout, stderr } = await execFileAsync("git", [
      "ls-files",
      "--stage",
      "--",
      filePath,
    ]);
    if (stdout === "") {
      // File does not exist.
      return false;
    }
    let modeString = stdout.split(" ")[0];
    let mode = parseInt(modeString, 16);
    return (mode & S_IFMT) == S_IFLNK;
  } catch (e) {
    console.warn(`failed to ask Git if file ${filePath} is symlink:`, e);
    return false;
  }
}

async function isSymlinkInFileSystemAsync(filePath) {
  try {
    return (await fs.promises.lstat(filePath)).isSymbolicLink();
  } catch (e) {
    console.warn(`failed to ask filesystem if file ${filePath} symlink:`, e);
    return false;
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
