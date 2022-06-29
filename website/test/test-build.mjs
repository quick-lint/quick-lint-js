// Copyright (C) 2020  Matthew "strager" Glazar
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
      { type: "copy", path: path.join("subdir", "index.html") },
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
        sourcePath: path.join("subdir", "index.ejs.html"),
        destinationPath: path.join("subdir", "index.html"),
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

  describe("/generated/<subdir>/", () => {
    it("/generated/ builds index.ejs.html if index.mjs is present", async () => {
      fs.mkdirSync(path.join(wwwRootPath, "generated"));
      fs.writeFileSync(
        path.join(wwwRootPath, "generated", "index.mjs"),
        "export let routes = {};"
      );
      fs.writeFileSync(
        path.join(wwwRootPath, "generated", "index.ejs.html"),
        "hello <%= 2+2 %>"
      );

      let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
      expect(buildInstructions).toContain({
        type: "build-ejs",
        sourcePath: path.join("generated", "index.ejs.html"),
        destinationPath: path.join("generated", "index.html"),
        ejsVariables: {
          currentURI: "/generated/",
        },
      });
    });

    it("builds .ejs.html page mentioned in /generated/index.mjs", async () => {
      fs.mkdirSync(path.join(wwwRootPath, "generated"));
      fs.writeFileSync(
        path.join(wwwRootPath, "generated", "index.mjs"),
        "export let routes = { '/generated/subdir/': { type: 'build-ejs', path: 'generated/page.ejs.html' } };"
      );
      fs.writeFileSync(
        path.join(wwwRootPath, "generated", "page.ejs.html"),
        "current URI is <%- currentURI %>"
      );

      let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
      expect(buildInstructions).toContain({
        type: "build-ejs",
        sourcePath: path.join("generated", "page.ejs.html"),
        destinationPath: path.join("generated", "subdir", "index.html"),
        ejsVariables: {
          currentURI: "/generated/subdir/",
        },
      });
    });

    it("does not copy index.mjs", async () => {
      fs.mkdirSync(path.join(wwwRootPath, "generated"));
      fs.writeFileSync(
        path.join(wwwRootPath, "generated", "index.mjs"),
        "export let routes = {};"
      );

      let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
      expect(buildInstructions).toEqual([]);
    });
  });

  describe("/generated/app.bundle.js ESBuild bundle", () => {
    it("creates a file", async () => {
      fs.mkdirSync(path.join(wwwRootPath, "generated"));
      fs.writeFileSync(
        path.join(wwwRootPath, "generated", "index.mjs"),
        `export let routes = {
          '/generated/app.bundle.js': {
            type: 'esbuild',
            esbuildConfig: {
              entryPoints: ["/my-app.js"],
            },
          }
        };`
      );
      fs.writeFileSync(
        path.join(wwwRootPath, "generated", "page.ejs.html"),
        "current URI is <%- currentURI %>"
      );

      let buildInstructions = await makeBuildInstructionsAsync({ wwwRootPath });
      expect(buildInstructions).toContain({
        type: "esbuild",
        bundlePath: "generated/app.bundle.js",
        esbuildConfig: {
          entryPoints: ["/my-app.js"],
        },
      });
    });
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
        { type: "copy", path: path.join("subdir", "test.tar.bz2") },
      ]);
    });
  });

  describe("static asset symlink causes copy", () => {
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
      // It's the responsibility of the follower of the instructions to copy the
      // target of the symlink.
      expect(buildInstructions).toEqual([
        {
          type: "copy",
          path: "image.webp",
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
