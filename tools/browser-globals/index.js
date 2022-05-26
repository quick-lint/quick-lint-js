// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

let WebIDL2 = require("webidl2");
let stream = require("stream");
let assert = require("assert");
let fs = require("fs");
let parse5 = require("parse5");
let path = require("path");

let extraGlobals = [
  "HTMLDocument", // Written in prose in the HTML specification.
  "Intl", // https://tc39.es/ecma402/
];

async function mainAsync() {
  let specsJSONPath = process.argv[2];
  if (typeof specsJSONPath === "undefined") {
    console.error(
      `usage: ${process.argv[0]} ${process.argv[1]} /path/to/web-specs/specs.json`
    );
    process.exit(1);
  }

  let specs = JSON.parse(fs.readFileSync(specsJSONPath, "utf-8"));
  let specsDirectory = path.dirname(specsJSONPath);
  let specPaths = specs["html-specs"].map((p) => path.join(specsDirectory, p));
  let idlPaths = specs["idl-specs"].map((p) => path.join(specsDirectory, p));

  let idlSourcePath = process.env.DUMP_IDL;
  let idlSourceOutputStream =
    typeof idlSourcePath === "undefined"
      ? new NullWriter()
      : fs.createWriteStream(idlSourcePath);

  let idlObjects = [];
  for (let specPath of specPaths) {
    idlObjects.push(
      ...getIDLObjectsFromSpecificationFile(specPath, idlSourceOutputStream)
    );
  }
  for (let idlPath of idlPaths) {
    idlObjects.push(
      ...getIDLObjectsFromIDLFile(idlPath, idlSourceOutputStream)
    );
  }

  let globals = [];
  for (let idlObject of idlObjects) {
    for (let global of getExposedGlobals(idlObject, idlObjects)) {
      globals.push(global);
    }
  }
  globals.push(...extraGlobals);
  globals.push(
    ...listRemovedInterfaces(path.join(specsDirectory, "dom/dom.bs"))
  );
  globals.sort();

  writeCPPFile(globals, process.stdout);
}

function getIDLObjectsFromSpecificationFile(specPath, idlSourceOutputStream) {
  let idlExtractor = new IDLExtractor();
  let root = parse5.parse(fs.readFileSync(specPath, "utf-8"));
  idlExtractor.visitRoot(root);
  let result = [];
  for (let idl of idlExtractor.getIDLs()) {
    idlSourceOutputStream.write(idl);
    idlSourceOutputStream.write("\n");
    result.push(...WebIDL2.parse(idl));
  }
  return result;
}

class DOMExtractorBase {
  visitRoot(root) {
    this.visitNode(root);
  }

  visitNode(node) {
    if (/^#/.test(node.nodeName)) {
      switch (node.nodeName) {
        case "#document":
          this.visitNodeChildren(node);
          break;

        case "#text":
          this.visitTextNode(node);
          break;

        case "#comment":
        case "#documentType":
          break;

        default:
          throw new TypeError(`Unexpected HTML node type: ${node.nodeName}`);
      }
    } else {
      this.visitElement(node);
    }
  }

  visitElement(node) {
    // Implement in base classes.
    this.visitNodeChildren(node);
  }

  visitTextNode(_node) {
    // Implement in base classes.
  }

  visitNodeChildren(node) {
    for (let child of node.childNodes) {
      this.visitNode(child);
    }
  }
}

class IDLExtractor extends DOMExtractorBase {
  constructor() {
    super();
    this._currentIDLChunks = [];
    this._inIDL = false;
    this._inPre = false;
    this._idls = [];
  }

  visitElement(node) {
    if (
      (node.tagName === "pre" &&
        node.attrs.some((attr) => attr.name === "w-nodev")) ||
      (node.tagName === "div" &&
        node.attrs.some(
          (attr) => attr.name === "class" && attr.value === "example"
        ))
    ) {
      // Ignore IDL examples.
      return;
    }

    let oldInPre = this._inPre;
    if (node.tagName === "pre") {
      this._inPre = true;
    }

    let oldInIDL = this._inIDL;
    if (
      (node.tagName === "code" && this._inPre) ||
      node.tagName === "pre" ||
      node.tagName === "xmp"
    ) {
      if (
        node.attrs.some(
          (attr) =>
            attr.name === "class" && attr.value.split(" ").includes("idl")
        )
      ) {
        this._inIDL = true;
      }
    } else if (node.tagName === "script") {
      if (
        node.attrs.some((attr) => attr.name === "type" && attr.value === "idl")
      ) {
        this._inIDL = true;
      }
    }
    if (this._inIDL && !oldInIDL) {
      this._currentIDLChunks.length = 0;
    }

    this.visitNodeChildren(node);

    if (this._inIDL && !oldInIDL) {
      this._idls.push(this._currentIDLChunks.join(""));
    }
    this._inIDL = oldInIDL;
    this._inPre = oldInPre;
  }

  visitTextNode(node) {
    let match;
    if (this._inIDL) {
      this._currentIDLChunks.push(node.value);
    } else if (
      (match = node.value.match(/^\s*```\s*webidl\s*$(.*?)^\s*```\s*$/ms)) !==
      null
    ) {
      // Parse Markdown embedded within HTML (yuck).
      this._idls.push(match[1]);
    }
  }

  getIDLs() {
    function fixInvalidPartialInterface(idl) {
      // HACK(strager): Fix broken IDL in webappsec-trusted-types.
      return idl.replace(/\b(partial interface \w+) : \w+ \{/g, "$1 {");
    }
    return this._idls.map((idl) => fixInvalidPartialInterface(idl));
  }
}

function getIDLObjectsFromIDLFile(idlPath, idlSourceOutputStream) {
  let idl = fs.readFileSync(idlPath, "utf-8");
  idlSourceOutputStream.write(idl);
  return WebIDL2.parse(idl);
}

function getExposedGlobals(idlObject, allIDLObjects) {
  switch (idlObject.type) {
    case "callback interface":
    case "interface":
    case "namespace":
      return getExposedGlobalsForInterface(idlObject);

    case "includes":
      return getExposedGlobalsForIncludes(idlObject, allIDLObjects);

    case "callback":
    case "dictionary":
    case "enum":
    case "interface mixin":
    case "typedef":
      return [];

    default:
      throw new TypeError(`Unexpected IDL object type: ${idlObject.type}`);
  }
}

function getExposedGlobalsForInterface(idlObject) {
  let globals = [];

  let globalNames = [
    "EventTarget", // implemented by Window
    "Window",
  ];
  if (globalNames.includes(idlObject.name)) {
    globals.push(...getInterfaceMemberNames(idlObject));
  }

  let exposed = false;
  for (let attr of idlObject.extAttrs) {
    if (attr.name === "Exposed") {
      if (
        attr.params.rhsType === "identifier" &&
        attr.params.tokens.secondaryName.value === "Window"
      ) {
        exposed = true;
        break;
      }
      if (
        attr.params.rhsType === "identifier-list" &&
        attr.params.list.some((token) => token.value === "Window")
      ) {
        exposed = true;
        break;
      }
    }
  }
  if (
    exposed &&
    !idlObject.extAttrs.some((attr) => attr.name === "LegacyNamespace")
  ) {
    globals.push(idlObject.name);
  }

  for (let attr of idlObject.extAttrs) {
    if (attr.name === "LegacyFactoryFunction") {
      if (attr.params.rhsType === "identifier") {
        globals.push(attr.params.tokens.secondaryName.value);
      }
    }
    if (attr.name === "LegacyWindowAlias") {
      if (attr.params.rhsType === "identifier") {
        globals.push(attr.params.tokens.secondaryName.value);
      }
      if (attr.params.rhsType === "identifier-list") {
        globals.push(...attr.params.list.map((token) => token.value));
      }
    }
  }

  return globals;
}

function getExposedGlobalsForIncludes(idlObject, allIDLObjects) {
  let globals = [];
  if (idlObject.target === "Window") {
    let mixinName = idlObject.includes;
    let mixins = allIDLObjects.filter(
      (o) => o.type === "interface mixin" && o.name === mixinName
    );
    if (mixins.length === 0) {
      throw new TypeError(`Could not find mixin named ${mixinName}`);
    }
    for (let mixin of mixins) {
      globals.push(...getInterfaceMemberNames(mixin));
    }
  }
  return globals;
}

function getInterfaceMemberNames(idlObject) {
  let result = [];
  for (let member of idlObject.members) {
    switch (member.type) {
      case "attribute":
      case "operation":
        if (member.name !== "") {
          result.push(member.name);
        }
        break;

      case "constructor":
        break;

      default:
        throw new TypeError(
          `Unexpected member type for ${idlObject.name}.${member.name}: ${member.type}`
        );
    }
  }
  return result;
}

function listRemovedInterfaces(specPath) {
  let root = parse5.parse(fs.readFileSync(specPath, "utf-8"));
  let extractor = new HistoricalInterfaceExtractor();
  extractor.visitRoot(root);
  return extractor._interfaceNames;
}

class HistoricalInterfaceExtractor extends DOMExtractorBase {
  constructor() {
    super();
    this._inHistorical = false;
    this._inHistoricalDfn = false;
    this._interfaceNames = [];
  }

  visitElement(node) {
    let oldInHistorical = this._inHistorical;
    if (
      node.tagName === "ul" &&
      node.attrs.some(
        (attr) => attr.name === "dfn-type" && attr.value === "interface"
      )
    ) {
      this._inHistorical = true;
    }

    let oldInHistoricalDfn = this._inHistoricalDfn;
    if (this._inHistorical && node.tagName === "dfn") {
      this._inHistoricalDfn = true;
    }

    this.visitNodeChildren(node);

    this._inHistoricalDfn = oldInHistoricalDfn;
    this._inHistorical = oldInHistorical;
  }

  visitTextNode(node) {
    if (this._inHistoricalDfn) {
      this._interfaceNames.push(node.value);
    }
  }
}

class NullWriter extends stream.Writable {
  _write(_chunk, _encoding, _callback) {
    _callback(null);
  }
}

function writeCPPFile(globals, outputStream) {
  outputStream.write(`\
// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// This file was generated by tools/browser-globals.

#include <quick-lint-js/char8.h>
#include <quick-lint-js/global-variables.h>

namespace quick_lint_js {
const char8 global_variables_browser[] =`);

  for (let global of globals) {
    if (!/^[A-Za-z0-9_$]+$/g.test(global)) {
      throw new TypeError(
        `Global variable name doesn't look like an identifier: ${global}`
      );
    }
    outputStream.write(`\n    u8"${global}\\0"`);
  }

  outputStream.write(`;
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
`);
}

mainAsync().catch((error) => {
  console.error(error.stack);
  process.exit(1);
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
