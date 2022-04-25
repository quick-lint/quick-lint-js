// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "fs";
import path from "path";
import url from "url";
import { Router, makeHTMLRedirect } from "./src/router.mjs";
import { makeBuildInstructionsAsync } from "./src/build.mjs";
import { readFileAsync } from "./src/fs.mjs";
import { websiteConfig } from "./src/config.mjs";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

async function mainAsync() {
  let { targetDirectory } = parseArguments(process.argv.slice(2));

  let wwwRootPath = websiteConfig.wwwRootPath;
  let instructions = await makeBuildInstructionsAsync(websiteConfig);
  let router = new Router(websiteConfig);

  async function copyFileAsync(fromPath, toPath) {
    let from = path.relative("", path.resolve(wwwRootPath, fromPath));
    let to = path.relative("", path.resolve(targetDirectory, toPath));
    console.log(`copy: ${from} -> ${to}`);
    await fs.promises.mkdir(path.dirname(to), { recursive: true });
    await fs.promises.writeFile(to, await readFileAsync(from));
  }

  for (let instruction of instructions) {
    switch (instruction.type) {
      case "copy":
        await copyFileAsync(instruction.path, instruction.path);
        break;

      case "copy-to":
        await copyFileAsync(
          instruction.sourcePath,
          instruction.destinationPath
        );
        break;

      case "build-ejs":
        let ejsPath = path.join(wwwRootPath, instruction.sourcePath);
        let outPath = path.join(targetDirectory, instruction.destinationPath);
        console.log(`build EJS: ${ejsPath} -> ${outPath}`);
        await fs.promises.mkdir(path.dirname(outPath), { recursive: true });
        let out = await router.renderEJSFile(
          ejsPath,
          instruction.ejsVariables,
          {
            // Reduce peak memory usage.
            allowCacheBusting: false,
          }
        );
        fs.promises.writeFile(outPath, out);
        break;

      case "esbuild":
        let bundlePath = path.join(targetDirectory, instruction.bundlePath);
        console.log(
          `esbuild: ${instruction.esbuildConfig.entryPoints
            .map((uri) => path.relative("", path.join(router.wwwRootPath, uri)))
            .join(", ")} -> ${path.relative("", bundlePath)}`
        );
        await fs.promises.mkdir(path.dirname(bundlePath), { recursive: true });
        await router.runESBuildAsync(instruction.esbuildConfig, bundlePath);
        break;

      case "html-redirect":
        let htmlPath = path.join(targetDirectory, instruction.htmlPath);
        await fs.promises.mkdir(path.dirname(htmlPath), { recursive: true });
        console.log(
          `redirect: ${htmlPath} -> ${instruction.redirectTargetURL}`
        );
        await fs.promises.writeFile(
          htmlPath,
          makeHTMLRedirect(instruction.htmlPath, instruction.redirectTargetURL)
        );
        break;

      case "warning":
        console.error(`error: ${instruction.message}`);
        process.exit(1);
        break;

      default:
        throw new Error(
          `Unexpected type from makeBuildInstructionsAsync: ${instruction.type}`
        );
    }
  }

  process.exit(0);
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
