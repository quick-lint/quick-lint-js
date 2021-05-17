// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

import fs from "fs";
import os from "os";
import path from "path";
import { makeBuildInstructionsAsync } from "../src/build.mjs";

describe("build", () => {
  let wwwRootPath;
  let temporaryDirectory;

  beforeEach(async () => {
    temporaryDirectory = fs.mkdtempSync(os.tmpdir() + path.sep);
    temporaryDirectory = fs.realpathSync(temporaryDirectory);
    wwwRootPath = temporaryDirectory;
  });

  afterEach(async () => {
    fs.rmSync(temporaryDirectory, { recursive: true });
  });

  it("/index.html causes copy to /index.html", async () => {
    fs.writeFileSync(path.join(wwwRootPath, "index.html"), "hello world");

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([{ type: "copy", path: "index.html" }]);
  });

  it("/index.ejs.html causes EJS-build to /index.html", async () => {
    fs.writeFileSync(
      path.join(wwwRootPath, "index.ejs.html"),
      "hello <%= 2+2 %>"
    );

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([
      {
        type: "build-ejs",
        sourcePath: "index.ejs.html",
        destinationPath: "index.html",
        ejsVariables: {
          currentURI: "/",
        },
      },
    ]);
  });

  it("both /index.html and /index.ejs.html causes warning", async () => {
    fs.writeFileSync(
      path.join(wwwRootPath, "index.ejs.html"),
      "hello <%= 2+2 %>"
    );
    fs.writeFileSync(path.join(wwwRootPath, "index.html"), "hello world");

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([
      {
        type: "warning",
        message: "/ has both index.ejs.html and index.html; using neither",
      },
    ]);
  });

  it("/subdir/index.html causes copy to /subdir/index.html", async () => {
    fs.mkdirSync(path.join(wwwRootPath, "subdir"));
    fs.writeFileSync(
      path.join(wwwRootPath, "subdir", "index.html"),
      "hello world"
    );

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([
      { type: "copy", path: "subdir/index.html" },
    ]);
  });

  it("/subdir/index.html causes EJS-build to /subdir/index.html", async () => {
    fs.mkdirSync(path.join(wwwRootPath, "subdir"));
    fs.writeFileSync(
      path.join(wwwRootPath, "subdir", "index.ejs.html"),
      "hello <%= 2+2 =>"
    );

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([
      {
        type: "build-ejs",
        sourcePath: "subdir/index.ejs.html",
        destinationPath: "subdir/index.html",
        ejsVariables: {
          currentURI: "/subdir/",
        },
      },
    ]);
  });

  it("both /subdir/index.html and /subdir/index.ejs.html causes warning", async () => {
    fs.mkdirSync(path.join(wwwRootPath, "subdir"));
    fs.writeFileSync(
      path.join(wwwRootPath, "subdir", "index.ejs.html"),
      "hello <%= 2+2 %>"
    );
    fs.writeFileSync(
      path.join(wwwRootPath, "subdir", "index.html"),
      "hello world"
    );

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([
      {
        type: "warning",
        message:
          "/subdir has both index.ejs.html and index.html; using neither",
      },
    ]);
  });

  describe("static asset causes copy", () => {
    it("/test.js", async () => {
      fs.writeFileSync(path.join(wwwRootPath, "test.js"), "hello();");

      let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
      expect(buildInstructions).toEqual([{ type: "copy", path: "test.js" }]);
    });

    it("/subdir/test.tar.bz2", async () => {
      fs.mkdirSync(path.join(wwwRootPath, "subdir"));
      fs.writeFileSync(
        path.join(wwwRootPath, "subdir", "test.tar.bz2"),
        "cmprssd dt"
      );

      let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
      expect(buildInstructions).toEqual([
        { type: "copy", path: "subdir/test.tar.bz2" },
      ]);
    });
  });

  describe("static asset symlink causes copy-to", () => {
    it("/image.webp -> ../symlink-target.webp", async () => {
      wwwRootPath = path.join(temporaryDirectory, "root");
      fs.mkdirSync(wwwRootPath);
      fs.symlinkSync(
        "../symlink-target.webp",
        path.join(wwwRootPath, "image.webp")
      );
      fs.writeFileSync(
        path.join(temporaryDirectory, "symlink-target.webp"),
        "WEBP"
      );

      let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
      expect(buildInstructions).toEqual([
        {
          type: "copy-to",
          sourcePath: path.join(temporaryDirectory, "symlink-target.webp"),
          destinationPath: "image.webp",
        },
      ]);
    });
  });

  describe("broken static asset symlink causes warning", () => {
    it("/image.webp -> does-not-exist.webp", async () => {
      fs.symlinkSync(
        "does-not-exist.webp",
        path.join(wwwRootPath, "image.webp")
      );

      let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
      expect(buildInstructions).toEqual([
        {
          type: "warning",
          message: "/image.webp is a broken symlink; ignoring",
        },
      ]);
    });
  });

  it("dotfiles (hidden files) are ignored", async () => {
    fs.writeFileSync(path.join(wwwRootPath, ".test.js"), "hello();");
    fs.writeFileSync(path.join(wwwRootPath, ".index.html"), "hello world");

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([]);
  });

  it("files in hidden directories are ignored", async () => {
    fs.mkdirSync(path.join(wwwRootPath, ".subdir"));
    fs.writeFileSync(path.join(wwwRootPath, ".subdir", "test.js"), "hello();");
    fs.writeFileSync(
      path.join(wwwRootPath, ".subdir", "index.html"),
      "hello world"
    );

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([]);
  });

  it("files in node_modules are ignored", async () => {
    fs.mkdirSync(path.join(wwwRootPath, "node_modules"));
    fs.mkdirSync(path.join(wwwRootPath, "node_modules", "subdir"));
    fs.writeFileSync(
      path.join(wwwRootPath, "node_modules", "test.js"),
      "hello();"
    );
    fs.writeFileSync(
      path.join(wwwRootPath, "node_modules", "subdir", "test.js"),
      "hello();"
    );

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([]);
  });

  it(".md files are ignored", async () => {
    fs.writeFileSync(path.join(wwwRootPath, "README.md"), "how stuff works");
    fs.writeFileSync(
      path.join(wwwRootPath, "copyright.txt"),
      "please no steal"
    );

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([
      { type: "copy", path: "copyright.txt" },
    ]);
  });

  it("files without extension are ignored", async () => {
    fs.writeFileSync(path.join(wwwRootPath, "linux-executable"), "beep boop");
    fs.writeFileSync(path.join(wwwRootPath, "COPYING"), "copyright goes here");

    let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
    expect(buildInstructions).toEqual([]);
  });

  it("htmlRedirects creates files", async () => {
    let buildInstructions = await makeBuildInstructionsAsync({
      wwwRootPath,
      htmlRedirects: {
        "/redirect-from.html": "redirect-to/",
      },
    });
    expect(buildInstructions).toEqual([
      {
        type: "html-redirect",
        htmlPath: "redirect-from.html",
        redirectTargetURL: "redirect-to/",
      },
    ]);
  });

  it("htmlRedirects creates files in subdirectories", async () => {
    fs.mkdirSync(path.join(wwwRootPath, "subdir"));

    let buildInstructions = await makeBuildInstructionsAsync({
      wwwRootPath,
      htmlRedirects: {
        "/subdir/from.html": "to/",
      },
    });
    expect(buildInstructions).toEqual([
      {
        type: "html-redirect",
        htmlPath: "subdir/from.html",
        redirectTargetURL: "to/",
      },
    ]);
  });

  describe("esbuildBundles", () => {
    it("creates a file", async () => {
      let buildInstructions = await makeBuildInstructionsAsync({
        wwwRootPath,
        esbuildBundles: {
          "/app.bundled.js": {
            entryPoints: ["/app.js"],
          },
        },
      });
      expect(buildInstructions).toEqual([
        {
          type: "esbuild",
          bundlePath: "app.bundled.js",
          esbuildConfig: {
            entryPoints: ["/app.js"],
          },
        },
      ]);
    });
  });
});

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
