// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "assert";
import fs from "fs";
import os from "os";
import path from "path";
import { InlineSpriteSheet } from "../src/sprite-sheet.mjs";

describe("InlineSpriteSheet", () => {
  it("empty has empty inline data", async () => {
    let sheet = new InlineSpriteSheet({ symbolIDPrefix: "sym" });
    expect(await sheet.makeInlineHTMLAsync()).toEqual("");
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
      expect(inlineHTML).toContain(`id="${testSVG.symbolID}"`);
      expect(inlineHTML).toContain('<path d="M0 0h8v8H0z"');
    });

    it("reference", async () => {
      let referenceHTML = testSVG.makeReferenceHTML({
        alt: "alt text goes here",
        width: 10,
        height: 12,
      });
      expect(referenceHTML).toContain(`xlink:href="#${testSVG.symbolID}"`);
      expect(referenceHTML).toContain("<svg");
      expect(referenceHTML).toContain('role="img"');
      expect(referenceHTML).toContain('aria-label="alt text goes here"');
      expect(referenceHTML).toContain('width="10"');
      expect(referenceHTML).toContain('height="12"');
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
