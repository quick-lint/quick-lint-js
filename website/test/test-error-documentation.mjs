// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "node:assert/strict";
import fs from "fs";
import os from "os";
import path from "path";
import {
  ErrorDocumentation,
  errorDocumentationExampleToHTML,
  flattenDiagnostics,
} from "../src/error-documentation.mjs";
import { DiagnosticSeverity } from "../wasm/quick-lint-js.js";
import { describe, it } from "node:test";

describe("error documentation", () => {
  it("error code from file path", () => {
    assert.equal(
      ErrorDocumentation.parseString("E0123.md", "").filePathErrorCode,
      "E0123"
    );
    assert.equal(
      ErrorDocumentation.parseString("path/to/E0666.md", "").filePathErrorCode,
      "E0666"
    );
    if (path === path.win32) {
      assert.equal(
        ErrorDocumentation.parseString("path\\to\\E0666.md", "")
          .filePathErrorCode,
        "E0666"
      );
    }
  });

  it("title", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "# E0123: title goes here\n"
    );
    assert.equal(doc.titleErrorCode, "E0123");
    assert.equal(doc.titleErrorDescriptionHTML, "title goes here");
  });

  it("title with HTML entity", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "# E0123: title &#x67;oes here\n"
    );
    assert.equal(doc.titleErrorCode, "E0123");
    assert.equal(doc.titleErrorDescriptionHTML, "title goes here");
  });

  it("title with < HTML entity", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "# E0123: test &lt; test\n"
    );
    assert.equal(doc.titleErrorDescriptionHTML, "test &lt; test");
  });

  it("title with inline code", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "# E0123: title goes `here`\n"
    );
    assert.equal(doc.titleErrorDescriptionHTML, "title goes <code>here</code>");
  });

  it("title with extra colon", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "# E0123: banana: strawberry: apple\n"
    );
    assert.equal(doc.titleErrorCode, "E0123");
    assert.equal(doc.titleErrorDescriptionHTML, "banana: strawberry: apple");
  });

  it("level 2 heading is not title", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "## E0123: title goes here\n"
    );
    assert.equal(doc.titleErrorCode, "");
    assert.equal(doc.titleErrorDescriptionHTML, "");
  });

  it("no code blocks", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "paragraph goes here\n"
    );
    assert.deepEqual(doc.codeBlocks, []);
    assert.ok(doc.shouldCheckCodeBlocks);
  });

  it("indented code blocks are ignored", () => {
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
    assert.deepEqual(doc.codeBlocks, []);
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
    assert.deepEqual(doc.codeBlocks, [
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
    assert.deepEqual(doc.codeBlocks, [
      {
        language: "testscript",
        text: "here is some code\nwith multiple lines\n\nand a blank line\n    and extra indentation\n",
      },
    ]);
  });

  it("multiple bracketed code blocks", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      `see this code:

\`\`\`
first
\`\`\`

\`\`\`typescript
second
\`\`\`

\`\`\`javascript
third
\`\`\`

wasn't that neat?
`
    );
    assert.deepEqual(doc.codeBlocks, [
      { language: "javascript", text: "first\n" },
      { language: "typescript", text: "second\n" },
      { language: "javascript", text: "third\n" },
    ]);
  });

  it("html wraps byte order mark", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n```\n\ufeff--BOM\n```\n"
    );
    assert.match(
      doc.toHTML(),
      /<span class='unicode-bom'>\u{feff}<\/span>--BOM/u
    );
  });

  it("html does not wrap fake byte order mark", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n```\n&#xfeff;--BOM\n```\n"
    );
    assert.match(doc.toHTML(), /&amp;#xfeff;--BOM/);
  });

  it("html wraps <mark>-d byte order mark", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n```\n\ufeff--BOM\n```\n"
    );
    doc.diagnostics = [[{ begin: 0, end: 1 }]];
    assert.match(
      doc.toHTML(),
      /<mark><span class='unicode-bom'>\u{feff}<\/span><\/mark>--BOM/u
    );
  });

  it("html does not wrap zero-width no break space", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n```\nhello\ufeffworld\n```\n"
    );
    assert.match(doc.toHTML(), /hello\ufeffworld/u);
  });

  it("html wraps weird control characters", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n```\n" + "BEL:\u0007\n" + "BS:\u0008\n" + "DEL:\u007f\n" + "```"
    );
    let html = doc.toHTML();
    assert.match(html, /BEL:<span class='unicode-bel'>\u0007<\/span>/u);
    assert.match(html, /BS:<span class='unicode-bs'>\u0008<\/span>/u);
    assert.match(html, /DEL:<span class='unicode-del'>\u007f<\/span>/u);
  });

  it("html has javascript class", () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "code:\n\n```\nhello\n```\n"
    );
    assert.match(doc.toHTML(), /<code class="javascript">/);
  });

  it("lint JavaScript", async () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "```javascript\nlet x;\nlet x;\n```\n"
    );
    await doc.findDiagnosticsAsync();
    assert.deepEqual(doc.diagnostics, [
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

  it("lint TypeScript", async () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      "```typescript\nabstract class C { }\nclass C { }\n```\n"
    );
    await doc.findDiagnosticsAsync();
    assert.deepEqual(doc.diagnostics, [
      [
        {
          code: "E0034",
          message: "redeclaration of variable: C",
          severity: 1,
          begin: 27,
          end: 28,
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
    assert.deepEqual(doc.diagnostics, [
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
    assert.equal(doc.configForExamples, null);
    await doc.findDiagnosticsAsync();
    assert.deepEqual(doc.diagnostics, [[]]);
  });

  it("config file for examples", async () => {
    let doc = ErrorDocumentation.parseString(
      "file.md",
      '```config-for-examples\n{"global-groups": false}\n```\n\n```\nconsole.log();\n```\n'
    );
    assert.equal(doc.configForExamples, '{"global-groups": false}\n');
    assert.deepEqual(doc.codeBlocks, [
      {
        language: "javascript",
        text: "console.log();\n",
      },
    ]);
    await doc.findDiagnosticsAsync();
    assert.deepEqual(doc.diagnostics, [
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
    assert.doesNotMatch(doc.toHTML(), /global-groups/);
  });

  it("QLJS_NO_CHECK_CODE disables all code block checks", async () => {
    let doc = ErrorDocumentation.parseString(
      "E9999.md",
      "# E9999: test\n\n<!-- QLJS_NO_CHECK_CODE -->\n\ndocs go here"
    );
    assert.ok(!doc.shouldCheckCodeBlocks);
  });
});

describe("flattenDiagnostics", () => {
  it("flattens nothing to nothing", () => {
    assert.deepEqual(flattenDiagnostics([]), []);
  });

  it("one diag flattens to two points", () => {
    assert.deepEqual(flattenDiagnostics([{ begin: 0, end: 5 }]), [
      { offset: 0, type: "begin", diagnostic: { begin: 0, end: 5 } },
      { offset: 5, type: "end", diagnostic: { begin: 0, end: 5 } },
    ]);
  });

  it("one diag point diag flattens to one point", () => {
    assert.deepEqual(flattenDiagnostics([{ begin: 1, end: 1 }]), [
      { offset: 1, type: "point", diagnostic: { begin: 1, end: 1 } },
    ]);
  });

  it("two unrelated diags flatten to two points each", () => {
    assert.deepEqual(
      flattenDiagnostics([
        { begin: 0, end: 5 },
        { begin: 10, end: 15 },
      ]),
      [
        { offset: 0, type: "begin", diagnostic: { begin: 0, end: 5 } },
        { offset: 5, type: "end", diagnostic: { begin: 0, end: 5 } },
        { offset: 10, type: "begin", diagnostic: { begin: 10, end: 15 } },
        { offset: 15, type: "end", diagnostic: { begin: 10, end: 15 } },
      ]
    );

    assert.deepEqual(
      flattenDiagnostics([
        { begin: 10, end: 15 },
        { begin: 0, end: 5 },
      ]),
      [
        { offset: 0, type: "begin", diagnostic: { begin: 0, end: 5 } },
        { offset: 5, type: "end", diagnostic: { begin: 0, end: 5 } },
        { offset: 10, type: "begin", diagnostic: { begin: 10, end: 15 } },
        { offset: 15, type: "end", diagnostic: { begin: 10, end: 15 } },
      ]
    );
  });

  it("two fully overlapping diags", () => {
    let a = { begin: 0, end: 5, message: "a" };
    let b = { begin: 0, end: 5, message: "b" };
    assert.deepEqual(flattenDiagnostics([a, b]), [
      { offset: 0, type: "begin", diagnostic: a },
      { offset: 0, type: "begin", diagnostic: b },
      { offset: 5, type: "end", diagnostic: b },
      { offset: 5, type: "end", diagnostic: a },
    ]);
    assert.deepEqual(flattenDiagnostics([b, a]), [
      { offset: 0, type: "begin", diagnostic: b },
      { offset: 0, type: "begin", diagnostic: a },
      { offset: 5, type: "end", diagnostic: a },
      { offset: 5, type: "end", diagnostic: b },
    ]);
  });

  it("one diag fully nested within another", () => {
    // OOOOO
    //   i
    let outer = { begin: 0, end: 5 };
    let inner = { begin: 2, end: 3 };
    assert.deepEqual(flattenDiagnostics([outer, inner]), [
      { offset: 0, type: "begin", diagnostic: outer },
      { offset: 2, type: "begin", diagnostic: inner },
      { offset: 3, type: "end", diagnostic: inner },
      { offset: 5, type: "end", diagnostic: outer },
    ]);
  });

  it("one point diag fully nested within a diag", () => {
    // OOOOO
    //   <
    let outer = { begin: 0, end: 5 };
    let inner = { begin: 2, end: 2 };
    let expectedPoints = [
      { offset: 0, type: "begin", diagnostic: outer },
      { offset: 2, type: "point", diagnostic: inner },
      { offset: 5, type: "end", diagnostic: outer },
    ];
    assert.deepEqual(flattenDiagnostics([outer, inner]), expectedPoints);
    assert.deepEqual(flattenDiagnostics([inner, outer]), expectedPoints);
  });

  it("one diag inside another touching end", () => {
    // OOOOO
    //   iii
    let outer = { begin: 0, end: 5 };
    let inner = { begin: 2, end: 5 };
    let expectedPoints = [
      { offset: 0, type: "begin", diagnostic: outer },
      { offset: 2, type: "begin", diagnostic: inner },
      { offset: 5, type: "end", diagnostic: inner },
      { offset: 5, type: "end", diagnostic: outer },
    ];
    assert.deepEqual(flattenDiagnostics([outer, inner]), expectedPoints);
    assert.deepEqual(flattenDiagnostics([inner, outer]), expectedPoints);
  });

  it("one diag inside another touching begin", () => {
    // OOOOO
    // iii
    let outer = { begin: 0, end: 5 };
    let inner = { begin: 0, end: 3 };
    let expectedPoints = [
      { offset: 0, type: "begin", diagnostic: outer },
      { offset: 0, type: "begin", diagnostic: inner },
      { offset: 3, type: "end", diagnostic: inner },
      { offset: 5, type: "end", diagnostic: outer },
    ];
    assert.deepEqual(flattenDiagnostics([outer, inner]), expectedPoints);
    assert.deepEqual(flattenDiagnostics([inner, outer]), expectedPoints);
  });

  it("two overlapping non-nested diags", () => {
    // LLLLL
    //    RRRR
    let left = { begin: 0, end: 5, message: "left" };
    let right = { begin: 3, end: 8, message: "right" };
    assert.deepEqual(flattenDiagnostics([left, right]), [
      { offset: 0, type: "begin", diagnostic: left },
      { offset: 3, type: "begin", diagnostic: right },
      { offset: 5, type: "end", diagnostic: right },
      { offset: 5, type: "end", diagnostic: left },
      { offset: 5, type: "begin", diagnostic: right },
      { offset: 8, type: "end", diagnostic: right },
    ]);
  });

  it("diag overlapping two adjacent diags", () => {
    // LLLLLLLLLLRRRRRRRRRR
    //      MMMMMMMMMM
    let left = { begin: 0, end: 10, message: "left" };
    let middle = { begin: 5, end: 15, message: "middle" };
    let right = { begin: 10, end: 20, message: "right" };
    assert.deepEqual(flattenDiagnostics([left, middle, right]), [
      { offset: 0, type: "begin", diagnostic: left },
      { offset: 5, type: "begin", diagnostic: middle },
      { offset: 10, type: "end", diagnostic: middle },
      { offset: 10, type: "end", diagnostic: left },
      { offset: 10, type: "begin", diagnostic: right },
      { offset: 10, type: "begin", diagnostic: middle },
      { offset: 15, type: "end", diagnostic: middle },
      { offset: 20, type: "end", diagnostic: right },
    ]);
  });
});

describe("errorDocumentationExampleToHTML", () => {
  it("escapes special HTML characters", () => {
    let html = errorDocumentationExampleToHTML({
      code: "a < b > c & d \u00a0 e",
      diagnostics: [],
    });
    assert.equal(html, "a &lt; b &gt; c &amp; d &nbsp; e");
  });

  it("mark first word on line", () => {
    let html = errorDocumentationExampleToHTML({
      code: "hello world",
      diagnostics: [{ begin: 0, end: 5 }],
    });
    assert.equal(html, "<mark>hello</mark> world");
  });

  it("mark last word on line", () => {
    let html = errorDocumentationExampleToHTML({
      code: "hello world",
      diagnostics: [{ begin: 6, end: 11 }],
    });
    assert.equal(html, "hello <mark>world</mark>");
  });

  it("multiple marks", () => {
    let html = errorDocumentationExampleToHTML({
      code: "hello world",
      diagnostics: [
        { begin: 0, end: 5 },
        { begin: 6, end: 11 },
      ],
    });
    assert.equal(html, "<mark>hello</mark> <mark>world</mark>");
  });

  it("multiple marks backwards", () => {
    let html = errorDocumentationExampleToHTML({
      code: "hello world",
      diagnostics: [
        { begin: 6, end: 11 },
        { begin: 0, end: 5 },
      ],
    });
    assert.equal(html, "<mark>hello</mark> <mark>world</mark>");
  });

  it("empty mark at beginning", () => {
    let html = errorDocumentationExampleToHTML({
      code: "helloworld",
      diagnostics: [{ begin: 0, end: 0 }],
    });
    assert.equal(html, "<mark></mark>helloworld");
  });

  it("empty mark in middle", () => {
    let html = errorDocumentationExampleToHTML({
      code: "helloworld",
      diagnostics: [{ begin: 5, end: 5 }],
    });
    assert.equal(html, "hello<mark></mark>world");
  });

  it("empty mark at end", () => {
    let html = errorDocumentationExampleToHTML({
      code: "helloworld",
      diagnostics: [{ begin: 10, end: 10 }],
    });
    assert.equal(html, "helloworld<mark></mark>");
  });

  it("empty mark immediately after non-empty mark", () => {
    let html = errorDocumentationExampleToHTML({
      code: "helloworld",
      diagnostics: [
        { begin: 0, end: 5 },
        { begin: 5, end: 5 },
      ],
    });
    assert.equal(html, "<mark>hello</mark><mark></mark>world");
  });

  it("identical marks are nested", () => {
    let html = errorDocumentationExampleToHTML({
      code: "helloworld",
      diagnostics: [
        { begin: 0, end: 5 },
        { begin: 0, end: 5 },
      ],
    });
    assert.equal(html, "<mark><mark>hello</mark></mark>world");
  });

  it("overlapping marks with same begin are nested", () => {
    let html = errorDocumentationExampleToHTML({
      code: "two errors please thanks",
      diagnostics: [
        { begin: 4, end: 10 }, // "errors"
        { begin: 4, end: 17 }, // "errors please"
      ],
    });
    assert.equal(html, "two <mark><mark>errors</mark> please</mark> thanks");
  });

  it("wraps byte order mark", () => {
    let html = errorDocumentationExampleToHTML({
      code: "\ufeff--BOM",
      diagnostics: [],
    });
    assert.equal(html, "<span class='unicode-bom'>\u{feff}</span>--BOM");
  });

  it("does not wrap fake byte order mark", () => {
    let html = errorDocumentationExampleToHTML({
      code: "&#xfeff;--BOM",
      diagnostics: [],
    });
    assert.equal(html, "&amp;#xfeff;--BOM");
  });

  it("wraps <mark>-d byte order mark", () => {
    let html = errorDocumentationExampleToHTML({
      code: "\ufeff--BOM",
      diagnostics: [{ begin: 0, end: 1 }],
    });
    assert.equal(
      html,
      "<mark><span class='unicode-bom'>\u{feff}</span></mark>--BOM"
    );
  });

  it("does not wrap zero-width no break space", () => {
    let html = errorDocumentationExampleToHTML({
      code: "hello\ufeffworld",
      diagnostics: [],
    });
    assert.equal(html, "hello\ufeffworld");
  });

  it("wraps weird control characters", () => {
    let html = errorDocumentationExampleToHTML({
      code: "BEL:\u0007\n" + "BS:\u0008\n" + "DEL:\u007f\n",
      diagnostics: [],
    });
    assert.match(html, /BEL:<span class='unicode-bel'>\u0007<\/span>/u);
    assert.match(html, /BS:<span class='unicode-bs'>\u0008<\/span>/u);
    assert.match(html, /DEL:<span class='unicode-del'>\u007f<\/span>/u);
  });

  it("mark includes diagnostic code, message, and severity", () => {
    let html = errorDocumentationExampleToHTML({
      code: "NaN = 0",
      diagnostics: [
        {
          begin: 0,
          end: 3,
          code: "E0002",
          message: "assignment to const global variable",
          severity: DiagnosticSeverity.ERROR,
        },
      ],
    });
    assert.equal(
      html,
      '<mark data-code="E0002" data-message="assignment to const global variable" data-severity="error">NaN</mark> = 0'
    );
  });

  it("warning mark", () => {
    let html = errorDocumentationExampleToHTML({
      code: "hi",
      diagnostics: [
        {
          begin: 0,
          end: 2,
          severity: DiagnosticSeverity.WARNING,
        },
      ],
    });
    assert.equal(html, '<mark data-severity="warning">hi</mark>');
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
