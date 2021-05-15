// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

import fs from "fs";
import os from "os";
import path from "path";
import { ErrorDocumentation } from "../src/error-documentation.mjs";

describe("error documentation", () => {
  it("error code from file path", () => {
    expect(
      ErrorDocumentation.parseString("E123.md", "").filePathErrorCode
    ).toBe("E123");
    expect(
      ErrorDocumentation.parseString("path/to/E666.md", "").filePathErrorCode
    ).toBe("E666");
    if (path === path.win32) {
      expect(
        ErrorDocumentation.parseString("path\\to\\E666.md", "")
          .filePathErrorCode
      ).toBe("E666");
    }
  });

  it("title", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "# E123: title goes here\n"
    );
    expect(doc.titleErrorCode).toBe("E123");
    expect(doc.titleErrorDescription).toBe("title goes here");
  });

  it("title with HTML entity", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "# E123: title &#x67;oes here\n"
    );
    expect(doc.titleErrorCode).toBe("E123");
    // TODO(strager): Translate HTML entities.
    expect(doc.titleErrorDescription).toBe("title &#x67;oes here");
  });

  it("level 2 heading is not title", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "## E123: title goes here\n"
    );
    expect(doc.titleErrorCode).toBe("");
    expect(doc.titleErrorDescription).toBe("");
  });
  it("no code blocks", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "paragraph goes here\n"
    );
    expect(doc.codeBlocks).toEqual([]);
  });

  it("one indented code block", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      `see this code:

    here is some code
    with multiple lines

    and a blank line
        and extra indentation

wasn't that neat?
`
    );
    expect(doc.codeBlocks).toEqual([
      "here is some code\nwith multiple lines\n\nand a blank line\n    and extra indentation\n",
    ]);
  });

  it("one bracketed code block", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      `see this code:

\`\`\`
here is some code
with multiple lines

and a blank line
    and extra indentation
\`\`\`

wasn't that neat?
`
    );
    expect(doc.codeBlocks).toEqual([
      "here is some code\nwith multiple lines\n\nand a blank line\n    and extra indentation\n",
    ]);
  });

  it("one bracketed code block with language", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      `see this code:

\`\`\`testscript
here is some code
with multiple lines

and a blank line
    and extra indentation
\`\`\`

wasn't that neat?
`
    );
    expect(doc.codeBlocks).toEqual([
      "here is some code\nwith multiple lines\n\nand a blank line\n    and extra indentation\n",
    ]);
  });

  it("multiple code blocks", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      `see this code:

    first

\`\`\`
second
\`\`\`

    third

wasn't that neat?
`
    );
    expect(doc.codeBlocks).toEqual(["first\n", "second\n", "third\n"]);
  });

  it("html wraps byte order mark", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n    \ufeff--BOM\n"
    );
    expect(doc.toHTML()).toContain(
      "<code><span class='unicode-bom'>\u{feff}</span>--BOM"
    );
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
