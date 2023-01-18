// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "fs";
import os from "os";
import path from "path";
import url from "url";
import {
  EJSVFSFile,
  ESBuildVFSFile,
  IndexConflictVFSError,
  MalformedDirectoryURIError,
  ServerConfigVFSFile,
  StaticVFSFile,
  VFS,
  VFSDirectory,
  uriAncestry,
} from "../src/vfs.mjs";

it("uriAncestry", () => {
  expect(uriAncestry("/")).toEqual(["/"]);
  expect(uriAncestry("/dir/")).toEqual(["/dir/", "/"]);
  expect(uriAncestry("/dir/subdir/")).toEqual(["/dir/subdir/", "/dir/", "/"]);
});

describe("VFS", () => {
  let vfs;
  let temporaryDirectory;
  let rootPath;
  beforeEach(async () => {
    temporaryDirectory = fs.mkdtempSync(os.tmpdir() + path.sep);
    temporaryDirectory = fs.realpathSync(temporaryDirectory);
    rootPath = temporaryDirectory;
    vfs = new VFS(rootPath);
  });

  afterEach(async () => {
    fs.rmSync(temporaryDirectory, { recursive: true });
  });

  describe("directory index", () => {
    it("directory with index.html shows index.html content", async () => {
      fs.writeFileSync(path.join(rootPath, "index.html"), "HTML goes here");

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual([""]);
      let index = children.get("");
      expect(index).toBeInstanceOf(StaticVFSFile);
      await assertSameFileAsync(index.path, path.join(rootPath, "index.html"));
    });

    it("directory with index.ejs.html shows index.ejs.html content", async () => {
      fs.writeFileSync(path.join(rootPath, "index.ejs.html"), "HTML goes here");

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual([""]);
      let index = children.get("");
      expect(index).toBeInstanceOf(EJSVFSFile);
      await assertSameFileAsync(
        index.path,
        path.join(rootPath, "index.ejs.html")
      );
    });

    it("directory with both index.html and index.ejs.html reports error", async () => {
      fs.writeFileSync(path.join(rootPath, "index.ejs.html"), "");
      fs.writeFileSync(path.join(rootPath, "index.html"), "");

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual([""]);
      let index = children.get("");
      expect(index).toBeInstanceOf(IndexConflictVFSError);
      await expect(
        index.conflictingPaths.map((p) => path.basename(p)).sort()
      ).toEqual(["index.ejs.html", "index.html"]);
    });

    it("subdirectory with index.ejs.html", async () => {
      fs.mkdirSync(path.join(rootPath, "subdir"));
      fs.writeFileSync(path.join(rootPath, "subdir", "index.ejs.html"), "");

      let children = await vfs.listDirectoryAsync("/subdir/");

      expect(children.names()).toEqual([""]);
      let index = children.get("");
      expect(index).toBeInstanceOf(EJSVFSFile);
      await assertSameFileAsync(
        index.path,
        path.join(rootPath, "subdir", "index.ejs.html")
      );
    });
  });

  it("lists nothing if the directory does not exist", async () => {
    let children = await vfs.listDirectoryAsync("/doesnotexist/");
    expect(children.names()).toEqual([]);
  });

  it("lists nothing if the directory is actually a file", async () => {
    fs.writeFileSync(path.join(rootPath, "somefile"), "");
    let children = await vfs.listDirectoryAsync("/somefile/");
    expect(children.names()).toEqual([]);
  });

  describe("subdirectory", () => {
    it("lists all subdirectories", async () => {
      fs.mkdirSync(path.join(rootPath, "dir1"));
      fs.mkdirSync(path.join(rootPath, "dir2"));
      fs.mkdirSync(path.join(rootPath, "dir3"));
      fs.writeFileSync(path.join(rootPath, "dir1", "index.html"), "");
      fs.writeFileSync(path.join(rootPath, "dir2", "index.ejs.html"), "");
      // dir3 remains empty.

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual(["dir1", "dir2", "dir3"]);
      for (let name of children.names()) {
        expect(children.get(name)).toBeInstanceOf(VFSDirectory);
      }
      expect(children.names().map((name) => children.get(name).uri)).toEqual([
        "/dir1/",
        "/dir2/",
        "/dir3/",
      ]);
    });

    it("listed shows index and static files and subsubdirectories", async () => {
      fs.mkdirSync(path.join(rootPath, "dir"));
      fs.mkdirSync(path.join(rootPath, "dir", "subdir"));
      fs.writeFileSync(path.join(rootPath, "dir", "index.html"), "");
      fs.writeFileSync(path.join(rootPath, "dir", "test.js"), "");

      let children = await vfs.listDirectoryAsync("/dir/");

      expect(children.names()).toEqual(["", "subdir", "test.js"]);

      let index = children.get("");
      expect(index).toBeInstanceOf(StaticVFSFile);
      await assertSameFileAsync(
        index.path,
        path.join(rootPath, "dir", "index.html")
      );

      let subdir = children.get("subdir");
      expect(subdir).toBeInstanceOf(VFSDirectory);
      expect(subdir.uri).toEqual("/dir/subdir/");

      let testJS = children.get("test.js");
      expect(testJS).toBeInstanceOf(StaticVFSFile);
      await assertSameFileAsync(
        testJS.path,
        path.join(rootPath, "dir", "test.js")
      );
    });
  });

  describe("static", () => {
    it("test.js shows file content", async () => {
      fs.writeFileSync(path.join(rootPath, "test.js"), "console.log('hello')");

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual(["test.js"]);
      let testJS = children.get("test.js");
      expect(testJS).toBeInstanceOf(StaticVFSFile);
      await assertSameFileAsync(testJS.path, path.join(rootPath, "test.js"));
    });
  });

  describe("hidden", () => {
    it("extensionless file", async () => {
      fs.writeFileSync(path.join(rootPath, "testfile"), "");
      let children = await vfs.listDirectoryAsync("/");
      expect(children.names()).toEqual([]);
    });

    it("something.ejs.html file", async () => {
      fs.writeFileSync(path.join(rootPath, "something.ejs.html"), "");
      let children = await vfs.listDirectoryAsync("/");
      expect(children.names()).toEqual([]);
    });

    it("Markdown file", async () => {
      fs.writeFileSync(path.join(rootPath, "readme.md"), "");
      let children = await vfs.listDirectoryAsync("/");
      expect(children.names()).toEqual([]);
    });

    it("dotfile", async () => {
      fs.writeFileSync(path.join(rootPath, ".test.js"), "");
      let children = await vfs.listDirectoryAsync("/");
      expect(children.names()).toEqual([]);
    });

    it("dotdirectory", async () => {
      fs.mkdirSync(path.join(rootPath, ".dir"));
      fs.writeFileSync(path.join(rootPath, ".dir", "test.js"), "");

      let children = await vfs.listDirectoryAsync("/");
      expect(children.names()).toEqual([]);

      children = await vfs.listDirectoryAsync("/.dir/");
      expect(children.names()).toEqual([]);
    });

    it("node_modules", async () => {
      fs.mkdirSync(path.join(rootPath, "node_modules"));
      fs.writeFileSync(path.join(rootPath, "node_modules", "test.js"), "");

      let children = await vfs.listDirectoryAsync("/");
      expect(children.names()).toEqual([]);

      children = await vfs.listDirectoryAsync("/node_modules/");
      expect(children.names()).toEqual([]);
    });
  });

  describe("server config", () => {
    it(".htaccess (Apache configuration)", async () => {
      fs.writeFileSync(path.join(rootPath, ".htaccess"), "");

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual([".htaccess"]);
      let htaccess = children.get(".htaccess");
      expect(htaccess).toBeInstanceOf(ServerConfigVFSFile);
      await assertSameFileAsync(
        htaccess.path,
        path.join(rootPath, ".htaccess")
      );
    });
  });

  describe("listed URI", () => {
    it("requires leading slash", async () => {
      fs.mkdirSync(path.join(rootPath, "dir"));
      await expectAsync(vfs.listDirectoryAsync("dir/")).toBeRejectedWithError(
        MalformedDirectoryURIError
      );
      await expectAsync(vfs.listDirectoryAsync("")).toBeRejectedWithError(
        MalformedDirectoryURIError
      );
    });

    it("requires trailing slash", async () => {
      fs.mkdirSync(path.join(rootPath, "dir"));
      await expectAsync(vfs.listDirectoryAsync("/dir")).toBeRejectedWithError(
        MalformedDirectoryURIError
      );
      await expectAsync(vfs.listDirectoryAsync("")).toBeRejectedWithError(
        MalformedDirectoryURIError
      );
    });

    it("disallows '.' components", async () => {
      fs.mkdirSync(path.join(rootPath, "dir"));
      await expectAsync(vfs.listDirectoryAsync("/./")).toBeRejectedWithError(
        MalformedDirectoryURIError
      );
      await expectAsync(
        vfs.listDirectoryAsync("/dir/./")
      ).toBeRejectedWithError(MalformedDirectoryURIError);
      await expectAsync(
        vfs.listDirectoryAsync("/./dir/")
      ).toBeRejectedWithError(MalformedDirectoryURIError);
    });

    it("disallows '..' components", async () => {
      fs.mkdirSync(path.join(rootPath, "dir"));
      fs.mkdirSync(path.join(rootPath, "otherdir"));
      await expectAsync(vfs.listDirectoryAsync("/../")).toBeRejectedWithError(
        MalformedDirectoryURIError
      );
      await expectAsync(
        vfs.listDirectoryAsync("/dir/../")
      ).toBeRejectedWithError(MalformedDirectoryURIError);
      await expectAsync(
        vfs.listDirectoryAsync("/dir/../otherdir/")
      ).toBeRejectedWithError(MalformedDirectoryURIError);
    });
  });

  describe("index.mjs routes", () => {
    it("creates directory and file routes", async () => {
      fs.writeFileSync(
        path.join(rootPath, "index.mjs"),
        `export let routes = {
          "/dir/": "test dir route",
          "/file": "test file route",
        };`
      );

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual(["dir", "file"]);
      let dir = children.get("dir");
      expect(dir).toBeInstanceOf(VFSDirectory);
      expect(dir.uri).toEqual("/dir/");
      expect(children.get("file")).toEqual("test file route");
    });

    it("has no conflict if directory exists and is routed by index.mjs", async () => {
      fs.writeFileSync(
        path.join(rootPath, "index.mjs"),
        `export let routes = {
          "/dir/": "test dir route",
        };`
      );
      fs.mkdirSync(path.join(rootPath, "dir"));

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual(["dir"]);
      let dir = children.get("dir");
      expect(dir).toBeInstanceOf(VFSDirectory);
      expect(dir.uri).toEqual("/dir/");
    });

    it("creates single directory route for multiple ancestor routes", async () => {
      fs.writeFileSync(
        path.join(rootPath, "index.mjs"),
        `export let routes = {
          "/dir/subdir/": "test dir/subdir route",
          "/dir/otherdir/": "test dir/otherdir route",
          "/dir/file.js": "test dir/file.js route",
          "/dir/other.js": "test dir/other.js route",
        };`
      );

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual(["dir"]);
      let dir = children.get("dir");
      expect(dir).toBeInstanceOf(VFSDirectory);
      expect(dir.uri).toEqual("/dir/");
    });

    it("synthetic directory route sets index", async () => {
      fs.writeFileSync(
        path.join(rootPath, "index.mjs"),
        `export let routes = {
          "/dir/": "test dir route",
        };`
      );

      let children = await vfs.listDirectoryAsync("/dir/");

      expect(children.names()).toEqual([""]);
      expect(children.get("")).toEqual("test dir route");
    });

    it("index.mjs without routes export is ignored", async () => {
      fs.mkdirSync(path.join(rootPath, "dir"));
      fs.writeFileSync(
        path.join(rootPath, "index.mjs"),
        `export let routes = {
          "/dir/subdir/": "route from /index.mjs",
        };`
      );
      fs.writeFileSync(path.join(rootPath, "dir", "index.mjs"), `/* empty */`);

      let children = await vfs.listDirectoryAsync("/dir/subdir/");

      expect(children.names()).toEqual([""]);
      expect(children.get("")).toEqual("route from /index.mjs");
    });

    it("type=build-ejs route is translated into EJSVFSFile", async () => {
      fs.writeFileSync(
        path.join(rootPath, "index.mjs"),
        `export let routes = {
          "/testfile": { type: "build-ejs", path: ".hello.ejs" },
        };`
      );
      fs.writeFileSync(path.join(rootPath, ".hello.ejs"), "");

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual(["testfile"]);
      let testfile = children.get("testfile");
      expect(testfile).toBeInstanceOf(EJSVFSFile);
      await assertSameFileAsync(
        testfile.path,
        path.join(rootPath, ".hello.ejs")
      );
    });

    it("type=esbuild route is translated into ESBuildVFSFile", async () => {
      fs.writeFileSync(
        path.join(rootPath, "index.mjs"),
        `export let routes = {
          "/testfile": {
            type: "esbuild",
            esbuildConfig: {
              entryPoints: ["/.hello.js"],
            },
          }
        };`
      );
      fs.writeFileSync(path.join(rootPath, ".hello.js"), "");

      let children = await vfs.listDirectoryAsync("/");

      expect(children.names()).toEqual(["testfile"]);
      let testfile = children.get("testfile");
      expect(testfile).toBeInstanceOf(ESBuildVFSFile);
      await assertSameFileAsync(
        testfile.config.entryPoints[0],
        path.join(rootPath, ".hello.js")
      );
    });
  });

  describe("index.mjs customComponents", () => {
    it("is used for index.ejs.html", async () => {
      fs.writeFileSync(
        path.join(rootPath, "index.mjs"),
        `export let customComponents = {
          "x-example": () => {
            throw new Error("should not be called");
          },
        };`
      );
      fs.writeFileSync(path.join(rootPath, "index.ejs.html"), "hello world");

      let children = await vfs.listDirectoryAsync("/");

      let index = children.get("");
      expect(index).toBeInstanceOf(EJSVFSFile);
      expect(Object.keys(index.customComponents)).toEqual(["x-example"]);
    });

    it("are combined from multiple index.mjs scripts", async () => {
      fs.mkdirSync(path.join(rootPath, "dir"));
      fs.writeFileSync(
        path.join(rootPath, "index.mjs"),
        `export let customComponents = {
          "x-from-root": () => {},
        };`
      );
      fs.writeFileSync(
        path.join(rootPath, "dir", "index.mjs"),
        `export let customComponents = {
          "x-from-dir": () => {},
        };`
      );
      fs.writeFileSync(
        path.join(rootPath, "dir", "index.ejs.html"),
        "hello world"
      );

      let children = await vfs.listDirectoryAsync("/dir/");

      let index = children.get("");
      expect(index).toBeInstanceOf(EJSVFSFile);
      expect(Object.keys(index.customComponents).sort()).toEqual([
        "x-from-dir",
        "x-from-root",
      ]);
    });

    it("disallows same custom component in multiple index.mjs scripts", async () => {
      fs.mkdirSync(path.join(rootPath, "dir"));
      fs.writeFileSync(
        path.join(rootPath, "index.mjs"),
        `export let customComponents = {
          "x-example": () => {},
        };`
      );
      fs.writeFileSync(
        path.join(rootPath, "dir", "index.mjs"),
        `export let customComponents = {
          "x-example": () => {},
        };`
      );
      fs.writeFileSync(
        path.join(rootPath, "dir", "index.ejs.html"),
        "hello world"
      );

      await expectAsync(vfs.listDirectoryAsync("/dir/")).toBeRejectedWithError(
        /duplicate|multiple/
      );
    });
  });
});

describe("StaticVFSFile", () => {
  let temporaryDirectory;
  beforeEach(() => {
    temporaryDirectory = fs.mkdtempSync(os.tmpdir() + path.sep);
    temporaryDirectory = fs.realpathSync(temporaryDirectory);
  });

  it("HTML file", async () => {
    let p = path.join(temporaryDirectory, "hello.html");
    fs.writeFileSync(p, "<!DOCTYPE html><h1>hello</h1>");

    let f = new StaticVFSFile(p);
    expect(await f.getContentsAsync()).toEqual(
      Buffer.from("<!DOCTYPE html><h1>hello</h1>")
    );
    expect(f.getContentType()).toEqual("text/html");
  });

  it("JavaScript file", async () => {
    let p = path.join(temporaryDirectory, "script.mjs");
    fs.writeFileSync(p, "console.log('hi')");

    let f = new StaticVFSFile(p);
    expect(await f.getContentsAsync()).toEqual(
      Buffer.from("console.log('hi')")
    );
    expect(f.getContentType()).toEqual("application/javascript");
  });
});

describe("ServerConfigVFSFile", () => {
  let temporaryDirectory;
  beforeEach(() => {
    temporaryDirectory = fs.mkdtempSync(os.tmpdir() + path.sep);
    temporaryDirectory = fs.realpathSync(temporaryDirectory);
  });

  it("has content based on file", async () => {
    let p = path.join(temporaryDirectory, ".htaccess");
    fs.writeFileSync(p, "# hello");

    let f = new ServerConfigVFSFile(p);
    expect(await f.getContentsAsync()).toEqual(Buffer.from("# hello"));
  });
});

describe("EJSVFSFile", () => {
  let temporaryDirectory;
  beforeEach(() => {
    temporaryDirectory = fs.mkdtempSync(os.tmpdir() + path.sep);
    temporaryDirectory = fs.realpathSync(temporaryDirectory);
  });

  it("content type", async () => {
    let p = path.join(temporaryDirectory, "hello.ejs.html");
    fs.writeFileSync(p, "");

    let f = new EJSVFSFile(p);
    expect(f.getContentType()).toEqual("text/html");
  });

  it("resolves basic EJS", async () => {
    let p = path.join(temporaryDirectory, "hello.ejs.html");
    fs.writeFileSync(p, "hello <%= 2+2 %>");

    let f = new EJSVFSFile(p, "/");
    expect(await f.getContentsAsync()).toEqual(Buffer.from("hello 4"));
  });

  it("included template can import relative paths using importFileAsync", async () => {
    fs.writeFileSync(
      path.join(temporaryDirectory, "index.ejs.html"),
      "<%- await include('./dir/included.ejs.html') %>"
    );
    fs.mkdirSync(path.join(temporaryDirectory, "dir"));
    fs.writeFileSync(
      path.join(temporaryDirectory, "dir/included.ejs.html"),
      `<%
        let url = await import("url");
        let { hello } = await importFileAsync("./hello.mjs");
        __append(hello());
      %>`
    );
    fs.writeFileSync(
      path.join(temporaryDirectory, "dir/hello.mjs"),
      "export function hello() { return 'hi'; }"
    );

    let f = new EJSVFSFile(
      path.join(temporaryDirectory, "index.ejs.html"),
      "/"
    );
    expect(await f.getContentsAsync()).toEqual(Buffer.from("hi"));
  });

  it("including does not affect later imports", async () => {
    fs.writeFileSync(
      path.join(temporaryDirectory, "index.ejs.html"),
      `<%
          __append(await include("./dir-a/included-a.ejs.html"));
          __append(" ");
          let url = await import("url");
          let { hello } = await importFileAsync("./dir-b/hello-b.mjs");
          __append(hello());
        %>`
    );
    fs.mkdirSync(path.join(temporaryDirectory, "dir-a"));
    fs.writeFileSync(
      path.join(temporaryDirectory, "dir-a/included-a.ejs.html"),
      `<%
          let url = await import("url");
          let { hello } = await importFileAsync("./hello-a.mjs");
          __append(hello());
        %>`
    );
    fs.writeFileSync(
      path.join(temporaryDirectory, "dir-a/hello-a.mjs"),
      "export function hello() { return 'hi-a'; }"
    );
    fs.mkdirSync(path.join(temporaryDirectory, "dir-b"));
    fs.writeFileSync(
      path.join(temporaryDirectory, "dir-b/hello-b.mjs"),
      "export function hello() { return 'hi-b'; }"
    );

    let f = new EJSVFSFile(
      path.join(temporaryDirectory, "index.ejs.html"),
      "/"
    );
    expect(await f.getContentsAsync()).toEqual(Buffer.from("hi-a hi-b"));
  });

  it("expands custom components with attributes and current URI", async () => {
    let components = {
      "x-test": (attributes, { currentURI }) => {
        return `myattr:${attributes.myattr}, currentURI:${currentURI}`;
      },
    };
    let p = path.join(temporaryDirectory, "hello.ejs.html");
    fs.writeFileSync(p, "<x-test myattr=myvalue />");

    let f = new EJSVFSFile(p, "/", components);
    expect(await f.getContentsAsync()).toEqual(
      Buffer.from("myattr:myvalue, currentURI:/")
    );
  });

  it("strips front matter", async () => {
    let p = path.join(temporaryDirectory, "hello.ejs.html");
    fs.writeFileSync(p, '<!---{\n"key": "value"\n}--->\n\nhello world');

    let f = new EJSVFSFile(p, "/");
    expect((await f.getContentsAsync()).toString("utf-8")).toEqual(
      "hello world"
    );
  });

  it("front matter declares meta variables", async () => {
    let p = path.join(temporaryDirectory, "hello.ejs.html");
    fs.writeFileSync(
      p,
      '<!---\n{"key": "value"}\n--->\n' + "<%= meta.key.toUpperCase() %>"
    );

    let f = new EJSVFSFile(p, "/");
    expect((await f.getContentsAsync()).toString("utf-8")).toContain("VALUE");
  });

  it("front matter meta data is accessible by included EJS", async () => {
    fs.writeFileSync(
      path.join(temporaryDirectory, "index.ejs.html"),
      `<!---{ "myMetaData": "myMetaValue" }--->
      <%- await include("./included.ejs.html") %>`
    );
    fs.writeFileSync(
      path.join(temporaryDirectory, "included.ejs.html"),
      `<%= meta.myMetaData.toUpperCase() %>`
    );

    let f = new EJSVFSFile(
      path.join(temporaryDirectory, "index.ejs.html"),
      "/"
    );
    expect((await f.getContentsAsync()).toString("utf-8")).toContain(
      "MYMETAVALUE"
    );
  });
});

describe("ESBuildVFSFile", () => {
  let temporaryDirectory;
  beforeEach(() => {
    temporaryDirectory = fs.mkdtempSync(os.tmpdir() + path.sep);
    temporaryDirectory = fs.realpathSync(temporaryDirectory);
  });

  it("content type", async () => {
    let f = new ESBuildVFSFile({});
    expect(f.getContentType()).toEqual("application/javascript");
  });

  it("should preserve simple script", async () => {
    fs.writeFileSync(
      path.join(temporaryDirectory, "app.js"),
      'console.log("hello world")'
    );

    let f = new ESBuildVFSFile({
      entryPoints: [path.join(temporaryDirectory, "app.js")],
    });
    let data = await f.getContentsAsync();
    expect(data.toString()).toContain('console.log("hello world")');
  });

  it("should bundle imported files", async () => {
    fs.writeFileSync(
      path.join(temporaryDirectory, "app.js"),
      'import { greet } from "./lib.js"; greet();'
    );
    fs.writeFileSync(
      path.join(temporaryDirectory, "lib.js"),
      'export function greet() { console.log("hello world"); }'
    );

    let f = new ESBuildVFSFile({
      entryPoints: [path.join(temporaryDirectory, "app.js")],
    });
    let data = (await f.getContentsAsync()).toString("utf-8");
    expect(data).toContain('console.log("hello world")');
    expect(data).toContain("greet");
    expect(data).not.toContain("import");
  });

  it("syntax error causes exception", async () => {
    fs.writeFileSync(
      path.join(temporaryDirectory, "bad-app.js"),
      "syntax error goes here !@#%$^"
    );

    let f = new ESBuildVFSFile({
      entryPoints: [path.join(temporaryDirectory, "bad-app.js")],
    });
    await expectAsync(f.getContentsAsync()).toBeRejectedWithError(
      Error,
      /Build failed with 1 error/
    );
  });
});

async function assertSameFileAsync(actual, expected) {
  if (!(await isSameFileAsync(actual, expected))) {
    throw new Error(`expected ${actual} to be the same as ${expected}`);
  }
}

// Follows symlinks.
async function isSameFileAsync(path1, path2) {
  let [stat1, stat2] = await Promise.all([
    fs.promises.stat(path1),
    fs.promises.stat(path2),
  ]);
  return stat1.dev === stat2.dev && stat1.ino === stat2.ino;
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
