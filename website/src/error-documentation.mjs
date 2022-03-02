// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import MarkdownIt from "markdown-it";
import assert from "assert";
import fs from "fs";
import jsdom from "jsdom";
import path from "path";
import url from "url";
import { createProcessFactoryAsync } from "../wasm/quick-lint-js.js";
import { markEditorText } from "../public/demo/editor.mjs";

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
let qljsProcessPromise = makeQLJSProcessAsync();

markdownParser.renderer.rules = {
  ...markdownParser.renderer.rules,

  heading_open(tokens, tokenIndex, options, env, self) {
    let token = tokens[tokenIndex];
    if (token.tag === "h1") {
      return `<h2>`;
    }
  },

  heading_close(tokens, tokenIndex, options, env, self) {
    let token = tokens[tokenIndex];
    if (token.tag === "h1") {
      return "</h2>";
    }
  },

  code_block(tokens, tokenIndex, options, env, self) {
    let token = tokens[tokenIndex];
    if (token.info === "config-for-examples") {
      // Don't show config snippets which configure other code blocks.
      return "";
    }
    let content = token.content;
    let hasBOM = content.startsWith("\ufeff");

    if (typeof env.dom === "undefined") {
      env.dom = new jsdom.JSDOM("");
    }
    if (typeof env.codeBlockIndex === "undefined") {
      env.codeBlockIndex = 0;
    }

    let codeElement = env.dom.window.document.createElement("code");
    codeElement.appendChild(env.dom.window.document.createTextNode(content));

    if (env.doc.diagnostics !== null) {
      markEditorText(
        codeElement,
        env.dom.window,
        env.doc.diagnostics[env.codeBlockIndex]
      );
    }

    let codeHTML = codeElement.innerHTML;

    // Wrap BOM in a <span>.
    if (hasBOM) {
      codeHTML = codeHTML.replace(
        /\ufeff/,
        "<span class='unicode-bom'>\u{feff}</span>"
      );
    }

    codeHTML = wrapASCIIControlCharacters(codeHTML);

    env.codeBlockIndex += 1;
    return `<figure><pre><code>${codeHTML}</code></pre></figure>`;
  },

  fence(tokens, tokenIndex, options, env, self) {
    return this.code_block(tokens, tokenIndex, options, env, self);
  },
};

export class ErrorDocumentation {
  constructor({
    codeBlocks,
    configForExamples,
    filePath,
    markdownEnv,
    markdownTokens,
    shouldCheckCodeBlocks,
    titleErrorCode,
    titleErrorDescription,
  }) {
    this._markdownEnv = markdownEnv;
    this._markdownTokens = markdownTokens;
    this.codeBlocks = codeBlocks;
    this.configForExamples = configForExamples;
    this.filePath = filePath;
    this.shouldCheckCodeBlocks = shouldCheckCodeBlocks;
    this.titleErrorCode = titleErrorCode;
    this.titleErrorDescription = titleErrorDescription;
    this.diagnostics = null;
  }

  get filePathErrorCode() {
    return path.basename(this.filePath, ".md");
  }

  async findDiagnosticsAsync() {
    if (this.diagnostics !== null) {
      // Already found.
      return;
    }

    this.diagnostics = [];
    let process = await qljsProcessPromise;
    for (let i = 0; i < this.codeBlocks.length; ++i) {
      let doc = await process.createDocumentForWebDemoAsync();
      try {
        let { text, language } = this.codeBlocks[i];
        if (this.configForExamples !== null) {
          doc.setConfigText(this.configForExamples);
        }
        doc.setText(text);
        let diagnostics =
          language === "quick-lint-js.config"
            ? doc.lintAsConfigFile()
            : doc.lint();
        this.diagnostics.push(diagnostics);
      } finally {
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
    let titleErrorDescription = "";

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
              titleErrorDescription = match.groups.description;
            }
          }
          break;

        case "code_block":
          codeBlocks.push({ text: token.content, language: "javascript" });
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
            currentBlock += token.content;
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
      titleErrorDescription: titleErrorDescription,
    });
  }

  toHTML() {
    this._markdownEnv.doc = this;
    return markdownParser.renderer.render(
      this._markdownTokens,
      markdownParser.options,
      this._markdownEnv
    );
  }

  async findProblemsAsync() {
    let foundProblems = [];
    if (this.titleErrorCode !== this.filePathErrorCode) {
      foundProblems.push(
        `${this.filePath}: error: file name doesn't match error code in title (${this.titleErrorCode})`
      );
    }
    if (
      this.codeBlocks.length === 1 &&
      this.codeBlocks[0].text === "/* TODO */\n"
    ) {
      // Don't check in-progress documentation.
      // TODO(strager): Remove this check.
      return [];
    }
    if (this.shouldCheckCodeBlocks) {
      if (this.codeBlocks.length === 0) {
        foundProblems.push(`${this.filePath}: error: missing code blocks`);
      }
      await this.findDiagnosticsAsync();
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

export async function reportProblemsInDocumentsAsync(documents) {
  let foundProblems = [];
  for (let doc of documents) {
    foundProblems.push(...(await doc.findProblemsAsync()));
  }
  if (foundProblems.length !== 0) {
    throw new ProblemsError(
      `found problems in error documents:\n${foundProblems.join("\n")}`
    );
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
