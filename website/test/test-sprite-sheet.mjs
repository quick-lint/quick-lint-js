// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "node:assert/strict";
import fs from "fs";
import os from "os";
import path from "path";
import {
  ExternalSpriteSheet,
  InlineSpriteSheet,
} from "../src/sprite-sheet.mjs";
import { describe, it, beforeEach, afterEach } from "node:test";

describe("InlineSpriteSheet", () => {
  it("empty has empty inline data", async () => {
    let sheet = new InlineSpriteSheet({ symbolIDPrefix: "sym" });
    assert.equal(await sheet.makeInlineHTMLAsync(), "");
  });

  let tempDir;
  beforeEach(() => {
    tempDir = fs.mkdtempSync(os.tmpdir() + path.sep);
  });
  afterEach(() => {
    fs.rmSync(tempDir, { recursive: true });
  });

  describe("one SVG", () => {
    let testSVGPath;
    let sheet;
    let testSVG;
    beforeEach(() => {
      testSVGPath = path.join(tempDir, "test.svg");
      fs.writeFileSync(
        testSVGPath,
        `
          <svg viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg">
            <path d="M0 0h8v8H0z" />
          </svg>
        `
      );
      sheet = new InlineSpriteSheet({ symbolIDPrefix: "sym" });
      testSVG = sheet.addSVG(testSVGPath);
    });

    it("appears in inline HTML", async () => {
      let inlineHTML = await sheet.makeInlineHTMLAsync();
      assert.ok(inlineHTML.includes(`id="${testSVG.symbolID}"`));
      assert.match(inlineHTML, /<path d="M0 0h8v8H0z"/);
    });

    it("reference", async () => {
      let referenceHTML = testSVG.makeReferenceHTML({
        alt: "alt text goes here",
        title: "title goes here",
        width: 10,
        height: 12,
      });
      assert.ok(referenceHTML.includes(`xlink:href="#${testSVG.symbolID}"`));
      assert.match(referenceHTML, /<svg/);
      assert.match(referenceHTML, /role="img"/);
      assert.match(referenceHTML, /aria-label="alt text goes here"/);
      assert.match(referenceHTML, /width="10"/);
      assert.match(referenceHTML, /height="12"/);
      assert.match(referenceHTML, /<title>title goes here<\/title>/);
    });
  });
});

describe("ExternalSpriteSheet", () => {
  it("empty has blank SVG external data", async () => {
    let sheet = new ExternalSpriteSheet();
    let external = await sheet.makeExternalFileAsync();
    assert.match(external, /<svg/);
    assert.match(external, /><\/svg>/);
  });

  let tempDir;
  beforeEach(() => {
    tempDir = fs.mkdtempSync(os.tmpdir() + path.sep);
  });
  afterEach(() => {
    fs.rmSync(tempDir, { recursive: true });
  });

  describe("one SVG", () => {
    let testSVGPath;
    let sheet;
    let testSVG;
    beforeEach(() => {
      testSVGPath = path.join(tempDir, "test.svg");
      fs.writeFileSync(
        testSVGPath,
        `
          <svg viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg">
            <path d="M0 0h8v8H0z" />
          </svg>
        `
      );
      sheet = new ExternalSpriteSheet();
      testSVG = sheet.addSVG(testSVGPath);
    });

    it("appears in external file", async () => {
      let external = await sheet.makeExternalFileAsync();
      assert.ok(external.includes(`id="${testSVG.symbolID}"`));
      assert.match(external, /<path d="M0 0h8v8H0z"/);
    });

    it("reference", async () => {
      let referenceHTML = testSVG.makeReferenceHTML({
        externalFileURI: "myspritesheet.svg",
        attributes: {
          alt: "alt text goes here",
          title: "title goes here",
          width: 10,
          height: 12,
          class: "banana",
        },
      });
      assert.ok(
        referenceHTML.includes(
          `xlink:href="myspritesheet.svg#${testSVG.symbolID}"`
        )
      );
      assert.match(referenceHTML, /<svg/);
      assert.match(referenceHTML, /role="img"/);
      assert.match(referenceHTML, /class="banana"/);
      assert.match(referenceHTML, /aria-label="alt text goes here"/);
      assert.match(referenceHTML, /width="10"/);
      assert.match(referenceHTML, /height="12"/);
      assert.match(referenceHTML, /<title>title goes here<\/title>/);
    });
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
