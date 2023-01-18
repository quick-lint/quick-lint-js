// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import ejs from "ejs";
import fs from "fs";
import path from "path";
import url from "url";
import { getQuickLintJSVersionInfoAsync } from "./qljs-version.mjs";
import { makeRelativeURI } from "./uri.mjs";
import { stripHTMLFrontMatter } from "./front-matter.mjs";

export async function renderEJSFileAsync(ejsFilePath, { currentURI }) {
  ejsFilePath = path.resolve(ejsFilePath);
  let ejsHTML = await fs.promises.readFile(ejsFilePath, "utf-8");

  let stripResult;
  try {
    stripResult = stripHTMLFrontMatter(ejsHTML);
  } catch (e) {
    if (e instanceof SyntaxError) {
      throw new SyntaxError(
        `failed to parse front matter of ${ejsFilePath}: ${e}`
      );
    }
    throw e;
  }
  ejsHTML = stripResult.strippedHTML;
  let frontMatterData = stripResult.data;

  let state = {
    cwd: path.dirname(ejsFilePath),
  };

  function includer(_path, resolvedPath) {
    // include() (defined by our prelude) will restore state.cwd for us.
    state.cwd = path.dirname(resolvedPath);
  }

  let prelude = `
    let __realInclude = include;
    include = async function (...args) {
      let oldCWD = _state.cwd;
      try {
        /* __realInclude will call includer which will call process.chdir. */
        return await __realInclude(...args);
      } finally {
        _state.cwd = oldCWD;
      }
    }
  `;
  prelude = prelude.replace(/\n/g, " "); // Preserve line numbers in user code.

  function absoluteFilePath(p) {
    return path.resolve(state.cwd, p);
  }

  return await ejs.render(
    `<% ${prelude} %>${ejsHTML}`,
    {
      _state: state,
      currentURI: currentURI,
      absoluteFilePath: absoluteFilePath,
      meta: frontMatterData,
      importFileAsync: async (pathToImport) => {
        return await import(url.pathToFileURL(absoluteFilePath(pathToImport)));
      },
      makeRelativeURI: (uri) => {
        return makeRelativeURI(currentURI, uri);
      },
      qljsVersionInfo: await getQuickLintJSVersionInfoAsync(),
      collapseInteriorWhitespace: (s) => {
        return s.replace(/\s+/g, " ");
      },
    },
    {
      async: true,
      compileDebug: true,
      filename: ejsFilePath,
      includer: includer,
    }
  );
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
