// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import MarkdownIt from "markdown-it";
import assert from "node:assert";
import fs from "node:fs";
import path from "node:path";
import url from "node:url";
import {
  DiagnosticSeverity,
  LanguageOptions,
  createProcessFactoryAsync,
} from "../wasm/quick-lint-js.js";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

export let documentationDirectoryPath = path.join(
  __dirname,
  "..",
  "..",
  "docs",
  "errors"
);

let markdownParser = new MarkdownIt("commonmark");

async function makeQLJSProcessAsync() {
  let factory = await createProcessFactoryAsync();
  let process = await factory.createProcessAsync();
  return process;
}
export let qljsProcessPromise = makeQLJSProcessAsync();

markdownParser.renderer.rules = {
  ...markdownParser.renderer.rules,

  fence(tokens, tokenIndex, options, env, self) {
    let token = tokens[tokenIndex];
    if (token.info === "config-for-examples") {
      // Don't show config snippets which configure other code blocks.
      return "";
    }

    if (typeof env.codeBlockIndex === "undefined") {
      env.codeBlockIndex = 0;
    }
    let codeHTML = errorDocumentationExampleToHTML({
      code: token.content,
      diagnostics:
        env.doc.diagnostics === null
          ? null
          : env.doc.diagnostics[env.codeBlockIndex],
    });
    env.codeBlockIndex += 1;
    return `<figure><pre><code class="javascript">${codeHTML}</code></pre></figure>`;
  },
};

export function errorDocumentationExampleToHTML({ code, diagnostics }) {
  let diagPoints = diagnostics === null ? [] : flattenDiagnostics(diagnostics);
  let codeHTML = "";
  let lastPointOffset = 0;
  for (let point of diagPoints) {
    let diagnostic = point.diagnostic;
    codeHTML += textToHTML(code.slice(lastPointOffset, point.offset));
    if (point.type === "begin" || point.type === "point") {
      codeHTML += "<mark";
      if (typeof diagnostic.code !== "undefined") {
        codeHTML += ` data-code="${textToHTML(diagnostic.code)}"`;
      }
      if (typeof diagnostic.message !== "undefined") {
        codeHTML += ` data-message="${textToHTML(diagnostic.message)}"`;
      }
      if (typeof diagnostic.severity !== "undefined") {
        codeHTML += ` data-severity="${
          {
            [DiagnosticSeverity.ERROR]: "error",
            [DiagnosticSeverity.WARNING]: "warning",
          }[diagnostic.severity]
        }"`;
      }
      codeHTML += ">";
    }
    if (point.type === "end" || point.type === "point") {
      codeHTML += "</mark>";
    }
    lastPointOffset = point.offset;
  }
  codeHTML += textToHTML(code.slice(lastPointOffset));

  // Wrap BOM in a <span>.
  let hasBOM = code.startsWith("\ufeff");
  if (hasBOM) {
    codeHTML = codeHTML.replace(
      /\ufeff/,
      "<span class='unicode-bom'>\u{feff}</span>"
    );
  }

  codeHTML = wrapASCIIControlCharacters(codeHTML);

  return codeHTML;
}

function textToHTML(text) {
  return text.replace(
    /[<>&\u00a0]/g,
    (match) =>
      ({
        "<": "&lt;",
        ">": "&gt;",
        "&": "&amp;",
        "\u00a0": "&nbsp;",
      }[match])
  );
}

export class ErrorDocumentation {
  constructor({
    codeBlocks,
    configForExamples,
    filePath,
    markdownEnv,
    markdownTokens,
    shouldCheckCodeBlocks,
    titleErrorCode,
    titleErrorDescriptionHTML,
  }) {
    this._markdownEnv = markdownEnv;
    this._markdownTokens = markdownTokens;
    this.codeBlocks = codeBlocks;
    this.configForExamples = configForExamples;
    this.filePath = filePath;
    this.shouldCheckCodeBlocks = shouldCheckCodeBlocks;
    this.titleErrorCode = titleErrorCode;
    this.titleErrorDescriptionHTML = titleErrorDescriptionHTML;
    this.diagnostics = null;
    this._diagnosticsLocale = null;
  }

  get filePathErrorCode() {
    return path.basename(this.filePath, ".md");
  }

  async findDiagnosticsAsync(locale = "") {
    if (this.diagnostics !== null && this._diagnosticsLocale === locale) {
      // Already found.
      return;
    }

    this.diagnostics = [];
    let process = await qljsProcessPromise;
    for (let i = 0; i < this.codeBlocks.length; ++i) {
      let doc = await process.createDocumentForWebDemoAsync();
      let configDoc = null;
      try {
        doc.setLocale(locale);
        let { text, language } = this.codeBlocks[i];
        if (this.configForExamples !== null) {
          configDoc = await process.createDocumentForWebDemoAsync();
          configDoc.setText(this.configForExamples);
          doc.setConfig(configDoc);
        }
        doc.setText(text);
        switch (language) {
          case "javascript":
            doc.setLanguageOptions(LanguageOptions.NONE);
            break;
          case "javascript-jsx":
            doc.setLanguageOptions(LanguageOptions.JSX);
            break;
          case "typescript":
            doc.setLanguageOptions(LanguageOptions.TYPESCRIPT);
            break;
          case "typescript-jsx":
            doc.setLanguageOptions(
              LanguageOptions.TYPESCRIPT | LanguageOptions.JSX
            );
            break;
          case "quick-lint-js.config":
            doc.setLanguageOptions(LanguageOptions.CONFIG_JSON);
            break;
          default:
            // TODO(strager): Warn.
            break;
        }
        let diagnostics = doc.lint();
        this.diagnostics.push(diagnostics);
      } finally {
        if (configDoc !== null) {
          configDoc.dispose();
        }
        doc.dispose();
      }
    }
    assert.strictEqual(this.diagnostics.length, this.codeBlocks.length);
  }

  static async parseFileAsync(filePath) {
    let markdown = await fs.promises.readFile(filePath, "utf-8");
    return ErrorDocumentation.parseString(filePath, markdown);
  }

  static parseString(filePath, markdown) {
    let markdownEnv = {};
    let tokens = markdownParser.parse(markdown, markdownEnv);

    let codeBlocks = [];
    let configForExamples = null;
    let shouldCheckCodeBlocks = true;
    let titleErrorCode = "";
    let titleErrorDescriptionHTML = "";

    let inTitle = false;
    let currentBlock;
    for (let token of tokens) {
      switch (token.type) {
        case "heading_open":
          if (token.tag === "h1") {
            inTitle = true;
            currentBlock = "";
          }
          break;

        case "heading_close":
          if (inTitle) {
            let match = currentBlock.match(
              /^(?<code>.*?):\s*(?<description>.*)$/
            );
            if (match !== null) {
              titleErrorCode = match.groups.code;
              titleErrorDescriptionHTML = match.groups.description;
            }
            inTitle = false;
          }
          break;

        case "code_block":
          // Ignore code blocks. Only respect code fences.
          break;

        case "fence":
          if (token.info === "config-for-examples") {
            configForExamples = token.content;
          } else {
            codeBlocks.push({
              text: token.content,
              language: token.info || "javascript",
            });
          }
          break;

        case "inline":
          if (inTitle) {
            currentBlock += markdownParser.renderer.render(
              [token],
              markdownParser.options,
              markdownEnv
            );
          }
          break;

        case "html_block":
          if (/\bQLJS_NO_CHECK_CODE\b/.test(token.content)) {
            shouldCheckCodeBlocks = false;
          }
          break;
      }
    }

    return new ErrorDocumentation({
      codeBlocks: codeBlocks,
      configForExamples: configForExamples,
      filePath: filePath,
      markdownEnv: markdownEnv,
      markdownTokens: tokens,
      shouldCheckCodeBlocks: shouldCheckCodeBlocks,
      titleErrorCode: titleErrorCode,
      titleErrorDescriptionHTML: titleErrorDescriptionHTML,
    });
  }

  // If findDiagnosticsAsync was previously called, then the HTML will include
  // <mark> elements for diagnostics.
  toHTML() {
    this._markdownEnv.doc = this;
    return markdownParser.renderer.render(
      this._markdownTokens,
      markdownParser.options,
      this._markdownEnv
    );
  }

  // Precondition: findDiagnosticsAsync was previously called.
  findProblems() {
    let foundProblems = [];
    if (this.titleErrorCode !== this.filePathErrorCode) {
      foundProblems.push(
        `${this.filePath}: error: file name doesn't match error code in title (${this.titleErrorCode})`
      );
    }
    if (this.shouldCheckCodeBlocks) {
      if (this.codeBlocks.length === 0) {
        foundProblems.push(`${this.filePath}: error: missing code blocks`);
      }
      if (this.diagnostics === null) {
        throw new Error(
          "findDiagnosticsAsync should have been called before calling findProblems"
        );
      }
      for (let i = 0; i < this.codeBlocks.length; ++i) {
        let diagnostics = this.diagnostics[i];

        // TODO(strager): Fix quick-lint-js (or our documentation) and remove
        // this workaround.
        if (
          this.codeBlocks[i].language === "javascript-ignoring-extra-errors"
        ) {
          diagnostics = diagnostics.filter(
            (diag) => diag.code === this.titleErrorCode
          );
        }

        let expectDiagnostic =
          i === 0 || this.codeBlocks[i].language === "javascript-with-errors";
        if (expectDiagnostic) {
          if (diagnostics.length === 0) {
            foundProblems.push(
              `${this.filePath}: error: expected error in first code block but found no errors`
            );
          } else {
            for (let diag of diagnostics) {
              if (diag.code !== this.titleErrorCode) {
                foundProblems.push(
                  `${this.filePath}: error: expected only ${this.titleErrorCode} errors in first code block but found ${diag.code}`
                );
              }
            }
          }
        } else {
          if (diagnostics.length !== 0) {
            foundProblems.push(
              `${this.filePath}: error: expected no error in code block #${
                i + 1
              } but found errors`
            );
          }
        }
      }
    }
    return foundProblems;
  }
}

export async function findErrorDocumentationFilesAsync(rootPath) {
  let files = await fs.promises.readdir(rootPath);
  return files.filter((fileName) => fileName.endsWith(".md"));
}

export async function loadErrorDocumentationFilesAsync(rootPath) {
  let files = await findErrorDocumentationFilesAsync(rootPath);

  let documents = await Promise.all(
    files.map(
      async (fileName) =>
        await ErrorDocumentation.parseFileAsync(path.join(rootPath, fileName))
    )
  );
  if (documents.length === 0) {
    throw new Error(`found no .md files in ${documentationDirectoryPath}`);
  }
  documents.sort((a, b) => {
    if (a.filePathErrorCode < b.filePathErrorCode) return -1;
    if (a.filePathErrorCode > b.filePathErrorCode) return +1;
    return 0;
  });
  return documents;
}

export class ProblemsError extends Error {}

function wrapASCIIControlCharacters(html) {
  let CONTROL_CHARACTER_TO_CSS_CLASS = {
    "\u0007": "unicode-bel",
    "\u0008": "unicode-bs",
    "\u007f": "unicode-del",
  };
  return html.replace(/[\u0007\u0008\u007f]/g, (controlCharacter) => {
    let cssClass = CONTROL_CHARACTER_TO_CSS_CLASS[controlCharacter];
    return `<span class='${cssClass}'>${controlCharacter}</span>`;
  });
}

export function flattenDiagnostics(diagnostics) {
  // TODO(strager): Use an interval map instead for efficiency.
  let diagnosticsPerCharacter = [];
  let maxOffset = Math.max(0, ...diagnostics.map((diag) => diag.end));
  for (let offset = 0; offset < maxOffset + 1; ++offset) {
    diagnosticsPerCharacter.push([]);
  }

  for (let diagnostic of diagnostics) {
    if (diagnostic.begin === diagnostic.end) {
      diagnosticsPerCharacter[diagnostic.begin].push(diagnostic);
    } else {
      for (let offset = diagnostic.begin; offset < diagnostic.end; ++offset) {
        diagnosticsPerCharacter[offset].push(diagnostic);
      }
    }
  }

  // Sort diags to encourage nesting and reduce splitting.
  //
  // If two diags begin at the same character, and one is nested within the
  // other (as determined by their 'end' properties), order them such that the
  // inner diagnostic (i.e. the one with the smaller 'end') appears after the
  // outer diagnostic (i.e. the one with the bigger 'end').
  for (let diagnosticsAtCharacter of diagnosticsPerCharacter) {
    diagnosticsAtCharacter.sort((a, b) => {
      if (a.end < b.end) return +1;
      if (a.end > b.end) return -1;
      let aIndex = diagnosticsAtCharacter.indexOf(a);
      let bIndex = diagnosticsAtCharacter.indexOf(b);
      if (aIndex < bIndex) return -1;
      if (aIndex > bIndex) return +1;
      return 0; // This shouldn't happen.
    });
  }

  let stack = [];
  let points = [];
  for (let offset = 0; offset <= maxOffset; ++offset) {
    let diagnosticsAtOffset = diagnosticsPerCharacter[offset];
    popInvalidStackEntries(offset, diagnosticsAtOffset);
    pushMissingDiagnostics(offset, diagnosticsAtOffset);
  }
  return points;

  function pushMissingDiagnostics(offset, diagnosticsAtOffset) {
    for (let d of diagnosticsAtOffset) {
      if (d.begin === d.end) {
        points.push({ type: "point", offset: offset, diagnostic: d });
      } else if (!stack.includes(d)) {
        points.push({ type: "begin", offset: offset, diagnostic: d });
        stack.push(d);
      }
    }
  }

  function popInvalidStackEntries(offset, diagnosticsAtOffset) {
    let newStackHeight = countValidStackEntries(diagnosticsAtOffset);
    for (let i = stack.length; i-- > newStackHeight; ) {
      let d = stack[i];
      points.push({ type: "end", offset: offset, diagnostic: d });
    }
    stack.length = newStackHeight;
  }

  function countValidStackEntries(diagnosticsAtOffset) {
    for (let i = 0; i < stack.length; ++i) {
      let d = stack[i];
      if (!diagnosticsAtOffset.includes(d)) {
        return i;
      }
    }
    return stack.length;
  }
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
