// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import * as parse5 from "parse5-sax-parser";

export async function substituteCustomHTMLComponentsAsync(html, components) {
  let parser = new parse5.SAXParser({
    sourceCodeLocationInfo: true,
  });
  let outputPieces = [];
  let beginOffset = 0;
  let endOffset = 0;
  function flushPiece() {
    outputPieces.push(html.substring(beginOffset, endOffset));
    beginOffset = endOffset;
  }

  await new Promise((resolve, reject) => {
    parser.on("doctype", (doctype) => {
      endOffset = doctype.sourceCodeLocation.endOffset;
    });
    parser.on("startTag", (startTag) => {
      try {
        if (
          Object.prototype.hasOwnProperty.call(components, startTag.tagName)
        ) {
          let component = components[startTag.tagName];
          if (!startTag.selfClosing) {
            throw new Error(
              "children for custom components are not yet implemented"
            );
          }

          let attributes = {};
          for (let { name, value } of startTag.attrs) {
            attributes[name] = value;
          }
          let customHTML = component(attributes);

          flushPiece();
          outputPieces.push(customHTML);
          beginOffset = startTag.sourceCodeLocation.endOffset;
          endOffset = beginOffset;
        } else {
          endOffset = startTag.sourceCodeLocation.endOffset;
        }
      } catch (e) {
        reject(e);
      }
    });
    parser.on("endTag", (endTag) => {
      try {
        endOffset = endTag.sourceCodeLocation.endOffset;
      } catch (e) {
        reject(e);
      }
    });
    parser.on("text", (text) => {
      try {
        endOffset = text.sourceCodeLocation.endOffset;
      } catch (e) {
        reject(e);
      }
    });
    parser.on("comment", (comment) => {
      try {
        endOffset = comment.sourceCodeLocation.endOffset;
      } catch (e) {
        reject(e);
      }
    });

    parser.on("end", () => {
      resolve();
    });
    parser.write(html);
    parser.end();
  });

  flushPiece();
  return outputPieces.join("");
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
