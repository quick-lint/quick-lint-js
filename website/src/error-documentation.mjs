// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

import MarkdownIt from "markdown-it";
import fs from "fs";
import path from "path";
import url from "url";
import { createProcessFactoryAsync } from "quick-lint-js-wasm/quick-lint-js.js";

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

markdownParser.renderer.rules = {
  ...markdownParser.renderer.rules,

  heading_open(tokens, tokenIndex, options, env, self) {
    let token = tokens[tokenIndex];
    if (token.tag === "h1") {
      return `<h2><a class='self-reference' href='#${env.doc.titleErrorCode}'>`;
    }
  },

  heading_close(tokens, tokenIndex, options, env, self) {
    let token = tokens[tokenIndex];
    if (token.tag === "h1") {
      return "</a></h2>";
    }
  },

  code_block(tokens, tokenIndex, options, env, self) {
    let token = tokens[tokenIndex];
    let content = token.content;

    let out = `<figure><pre><code>`;

    // Wrap BOM in a <span>.
    if (content[0] === "\u{feff}") {
      out += "<span class='unicode-bom'>\u{feff}</span>";
      content = content.substr(1);
    }

    return `${out}${markdownParser.utils.escapeHtml(
      content
    )}</code></pre></figure>`;
  },

  fence(tokens, tokenIndex, options, env, self) {
    return this.code_block(tokens, tokenIndex, options, env, self);
  },
};

export class ErrorDocumentation {
  constructor({
    codeBlocks,
    filePath,
    markdownTokens,
    titleErrorCode,
    titleErrorDescription,
  }) {
    this._markdownTokens = markdownTokens;
    this.codeBlocks = codeBlocks;
    this.filePath = filePath;
    this.titleErrorCode = titleErrorCode;
    this.titleErrorDescription = titleErrorDescription;
  }

  get filePathErrorCode() {
    return path.basename(this.filePath, ".md");
  }

  static async parseFileAsync(filePath) {
    let markdown = await fs.promises.readFile(filePath, "utf-8");
    return ErrorDocumentation.parseString(filePath, markdown);
  }

  static parseString(filePath, markdown) {
    let tokens = markdownParser.parse(markdown);

    let codeBlocks = [];
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
              /^(?<code>.*):\s*(?<description>.*)$/
            );
            if (match !== null) {
              titleErrorCode = match.groups.code;
              titleErrorDescription = match.groups.description;
            }
          }
          break;

        case "code_block":
        case "fence":
          codeBlocks.push(token.content);
          break;

        case "inline":
          if (inTitle) {
            currentBlock += token.content;
          }
          break;
      }
    }

    return new ErrorDocumentation({
      codeBlocks: codeBlocks,
      filePath: filePath,
      markdownTokens: tokens,
      titleErrorCode: titleErrorCode,
      titleErrorDescription: titleErrorDescription,
    });
  }

  toHTML() {
    let innerHTML = markdownParser.renderer.render(
      this._markdownTokens,
      markdownParser.options,
      { doc: this }
    );
    return `<article id='${this.titleErrorCode}'>${innerHTML}</article>`;
  }

  async findProblemsAsync() {
    let foundProblems = [];
    if (this.titleErrorCode !== this.filePathErrorCode) {
      foundProblems.push(
        `${this.filePath}: error: file name doesn't match error code in title (${this.titleErrorCode})`
      );
    }
    if (this.codeBlocks.length === 0) {
      foundProblems.push(`${this.filePath}: error: missing code blocks`);
    }
    let factory = await createProcessFactoryAsync();
    let process = await factory.createProcessAsync();
    for (let i = 0; i < this.codeBlocks.length; ++i) {
      let parser = await process.createParserAsync();
      parser.replaceText(
        { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } },
        this.codeBlocks[i]
      );
      let diagnostics = parser.lint();

      let expectDiagnostic = i === 0;
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
    return foundProblems;
  }
}

export async function reportProblemsInDocumentsAsync(documents) {
  let foundProblems = [];
  for (let doc of documents) {
    foundProblems.push(...(await doc.findProblemsAsync()));
  }
  if (foundProblems.length !== 0) {
    throw new Error(
      `found problems in error documents:\n${foundProblems.join("\n")}`
    );
  }
}

export async function loadErrorDocumentationFilesAsync(rootPath) {
  let files = await fs.promises.readdir(rootPath);
  files = files.filter((fileName) => fileName.endsWith(".md"));

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
