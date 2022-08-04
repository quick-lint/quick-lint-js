// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import SVGSpriter from "svg-sprite";
import Vinyl from "vinyl";
import assert from "assert";
import fs from "fs";

export class ExternalSpriteSheet {
  constructor() {
    this._paths = [];
    // Vinyl normalizes paths. Use the normalized path as the key so we can look
    // it up later in the symbol ID generator function.
    this._vinylPathToSymbolID = new Map();
  }

  addSVG(path) {
    let symbolID = `s${this._paths.length}`;
    let vinylPath = new Vinyl({
      path: path,
      contents: emptyBuffer,
    }).path;
    this._vinylPathToSymbolID.set(vinylPath, symbolID);
    this._paths.push(path);
    return new ExternalSpriteSheetImage({ symbolID });
  }

  async makeExternalFileAsync() {
    let config = {
      shape: {
        id: {
          generator: (name, file) => {
            let symbolID = this._vinylPathToSymbolID.get(file.path);
            assert.ok(symbolID);
            return symbolID;
          },
        },
      },
      mode: {
        defs: true,
      },
    };
    let spriter = new SVGSpriter(config);
    for (let path of this._paths) {
      spriter.add(
        new Vinyl({
          path: path,
          contents: await fs.promises.readFile(path),
        })
      );
    }
    let { result } = await spriter.compileAsync();
    return result.defs.sprite.contents.toString();
  }
}

function makeSVGSymbolReference({
  externalFileURI,
  symbolID,
  attributes: { alt, title, ...passthruAttributes },
}) {
  let svgAttributes = ' role="img"';
  if (alt) {
    // TODO(strager): HTML-escape.
    svgAttributes += ` aria-label="${alt}"`;
  }
  for (let [name, value] of Object.entries(passthruAttributes)) {
    // TODO(strager): HTML-escape.
    svgAttributes += ` ${name}="${value}"`;
  }
  // TODO(strager): HTML-escape.
  let titleHTML = title ? `<title>${title}</title>` : "";
  return `<svg${svgAttributes}><use xlink:href="${externalFileURI}#${symbolID}">${titleHTML}</use></svg>`;
}

class ExternalSpriteSheetImage {
  constructor({ symbolID }) {
    this._symbolID = symbolID;
  }

  get symbolID() {
    return this._symbolID;
  }

  makeReferenceHTML({ attributes, externalFileURI }) {
    return makeSVGSymbolReference({
      attributes,
      externalFileURI,
      symbolID: this._symbolID,
    });
  }
}

export class InlineSpriteSheet {
  constructor({ symbolIDPrefix }) {
    this._symbolIDPrefix = symbolIDPrefix;
    this._paths = [];
    // Vinyl normalizes paths. Use the normalized path as the key so we can look
    // it up later in the symbol ID generator function.
    this._vinylPathToSymbolID = new Map();
  }

  addSVG(path) {
    let symbolID = `${this._symbolIDPrefix}${this._paths.length}`;
    let vinylPath = new Vinyl({
      path: path,
      contents: emptyBuffer,
    }).path;
    this._vinylPathToSymbolID.set(vinylPath, symbolID);
    this._paths.push(path);
    return new InlineSpriteSheetImage({ symbolID });
  }

  async makeInlineHTMLAsync() {
    if (this._paths.length === 0) {
      return "";
    }

    let config = {
      shape: {
        id: {
          generator: (name, file) => {
            let symbolID = this._vinylPathToSymbolID.get(file.path);
            assert.ok(symbolID);
            return symbolID;
          },
        },
      },
      mode: {
        symbol: {
          inline: true,
        },
      },
    };
    let spriter = new SVGSpriter(config);
    for (let path of this._paths) {
      spriter.add(
        new Vinyl({
          path: path,
          contents: await fs.promises.readFile(path),
        })
      );
    }
    let { result } = await spriter.compileAsync();
    return result.symbol.sprite.contents.toString();
  }
}

class InlineSpriteSheetImage {
  constructor({ symbolID }) {
    this._symbolID = symbolID;
  }

  get symbolID() {
    return this._symbolID;
  }

  makeReferenceHTML(attributes = {}) {
    return makeSVGSymbolReference({
      attributes,
      externalFileURI: "",
      symbolID: this._symbolID,
    });
  }
}

let emptyBuffer = Buffer.from([]);

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
