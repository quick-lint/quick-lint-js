// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "fs";
import os from "os";
import path from "path";
import {
  ErrorDocumentation,
  errorDocumentationExampleToHTML,
} from "../src/error-documentation.mjs";

describe("error documentation", () => {
  it("error code from file path", () => {
    expect(
      ErrorDocumentation.parseString("E0123.md", "").filePathErrorCode
    ).toBe("E0123");
    expect(
      ErrorDocumentation.parseString("path/to/E0666.md", "").filePathErrorCode
    ).toBe("E0666");
    if (path === path.win32) {
      expect(
        ErrorDocumentation.parseString("path\\to\\E0666.md", "")
          .filePathErrorCode
      ).toBe("E0666");
    }
  });

  it("title", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "# E0123: title goes here\n"
    );
    expect(doc.titleErrorCode).toBe("E0123");
    expect(doc.titleErrorDescription).toBe("title goes here");
  });

  it("title with HTML entity", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "# E0123: title &#x67;oes here\n"
    );
    expect(doc.titleErrorCode).toBe("E0123");
    // TODO(strager): Translate HTML entities.
    expect(doc.titleErrorDescription).toBe("title &#x67;oes here");
  });

  it("title with extra colon", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "# E0123: banana: strawberry: apple\n"
    );
    expect(doc.titleErrorCode).toBe("E0123");
    expect(doc.titleErrorDescription).toBe("banana: strawberry: apple");
  });

  it("level 2 heading is not title", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "## E0123: title goes here\n"
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
    expect(doc.shouldCheckCodeBlocks).toBeTrue();
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
      {
        language: "javascript",
        text: "here is some code\nwith multiple lines\n\nand a blank line\n    and extra indentation\n",
      },
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
      {
        language: "javascript",
        text: "here is some code\nwith multiple lines\n\nand a blank line\n    and extra indentation\n",
      },
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
      {
        language: "testscript",
        text: "here is some code\nwith multiple lines\n\nand a blank line\n    and extra indentation\n",
      },
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
    expect(doc.codeBlocks).toEqual([
      { language: "javascript", text: "first\n" },
      { language: "javascript", text: "second\n" },
      { language: "javascript", text: "third\n" },
    ]);
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

  it("html does not wrap fake byte order mark", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n    &#xfeff;--BOM\n"
    );
    expect(doc.toHTML()).toContain("<code>&amp;#xfeff;--BOM");
  });

  it("html wraps <mark>-d byte order mark", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n    \ufeff--BOM\n"
    );
    doc.diagnostics = [[{ begin: 0, end: 1 }]];
    expect(doc.toHTML()).toContain(
      "<code><mark><span class='unicode-bom'>\u{feff}</span></mark>--BOM"
    );
  });

  it("html does not wrap zero-width no break space", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n    hello\ufeffworld\n"
    );
    expect(doc.toHTML()).toContain("hello\ufeffworld");
  });

  it("html wraps weird control characters", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n```\n" + "BEL:\u0007\n" + "BS:\u0008\n" + "DEL:\u007f\n" + "```"
    );
    let html = doc.toHTML();
    expect(html).toContain("BEL:<span class='unicode-bel'>\u0007</span>");
    expect(html).toContain("BS:<span class='unicode-bs'>\u0008</span>");
    expect(html).toContain("DEL:<span class='unicode-del'>\u007f</span>");
  });

  it("lint JavaScript", async () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "    let x;\n    let x;\n"
    );
    await doc.findDiagnosticsAsync();
    expect(doc.diagnostics).toEqual([
      [
        {
          code: "E0034",
          message: "redeclaration of variable: x",
          severity: 1,
          begin: 11,
          end: 12,
        },
      ],
    ]);
  });

  it("lint config file", async () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      '```quick-lint-js.config\n{"globals": false}\n```\n'
    );
    await doc.findDiagnosticsAsync();
    expect(doc.diagnostics).toEqual([
      [
        {
          code: "E0168",
          message: '"globals" must be an object',
          severity: 1,
          begin: 12,
          end: 17,
        },
      ],
    ]);
  });

  it("config file for examples is null by default", async () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "```\nconsole.log();\n```"
    );
    expect(doc.configForExamples).toBeNull();
    await doc.findDiagnosticsAsync();
    expect(doc.diagnostics).toEqual([[]]);
  });

  it("config file for examples", async () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      '```config-for-examples\n{"global-groups": false}\n```\n\n    console.log();'
    );
    expect(doc.configForExamples).toEqual('{"global-groups": false}\n');
    expect(doc.codeBlocks).toEqual([
      {
        language: "javascript",
        text: "console.log();\n",
      },
    ]);
    await doc.findDiagnosticsAsync();
    expect(doc.diagnostics).toEqual([
      [
        {
          code: "E0057",
          message: "use of undeclared variable: console",
          severity: 2,
          begin: 0,
          end: 7,
        },
      ],
    ]);
    expect(doc.toHTML()).not.toContain("global-groups");
  });

  it("QLJS_NO_CHECK_CODE disables all code block checks", async () => {
    let doc = ErrorDocumentation.parseString(
      "E9999.md",
      "# E9999: test\n\n<!-- QLJS_NO_CHECK_CODE -->\n\ndocs go here"
    );
    expect(doc.shouldCheckCodeBlocks).toBeFalse();
  });
});

describe("errorDocumentationExampleToHTML", () => {
  it("escapes special HTML characters", () => {
    let html = errorDocumentationExampleToHTML({
      code: "a < b > c & d \u00a0 e",
      diagnostics: [],
    });
    expect(html).toBe("a &lt; b &gt; c &amp; d &nbsp; e");
  });

  it("wraps byte order mark", () => {
    let html = errorDocumentationExampleToHTML({
      code: "\ufeff--BOM",
      diagnostics: [],
    });
    expect(html).toBe("<span class='unicode-bom'>\u{feff}</span>--BOM");
  });

  it("does not wrap fake byte order mark", () => {
    let html = errorDocumentationExampleToHTML({
      code: "&#xfeff;--BOM",
      diagnostics: [],
    });
    expect(html).toBe("&amp;#xfeff;--BOM");
  });

  it("wraps <mark>-d byte order mark", () => {
    let html = errorDocumentationExampleToHTML({
      code: "\ufeff--BOM",
      diagnostics: [{ begin: 0, end: 1 }],
    });
    expect(html).toBe(
      "<mark><span class='unicode-bom'>\u{feff}</span></mark>--BOM"
    );
  });

  it("does not wrap zero-width no break space", () => {
    let html = errorDocumentationExampleToHTML({
      code: "hello\ufeffworld",
      diagnostics: [],
    });
    expect(html).toBe("hello\ufeffworld");
  });

  it("wraps weird control characters", () => {
    let html = errorDocumentationExampleToHTML({
      code: "BEL:\u0007\n" + "BS:\u0008\n" + "DEL:\u007f\n",
      diagnostics: [],
    });
    expect(html).toContain("BEL:<span class='unicode-bel'>\u0007</span>");
    expect(html).toContain("BS:<span class='unicode-bs'>\u0008</span>");
    expect(html).toContain("DEL:<span class='unicode-del'>\u007f</span>");
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
