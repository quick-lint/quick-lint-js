// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import path from "path";
import url from "url";
import { ExternalSpriteSheet } from "../src/sprite-sheet.mjs";
import { html } from "../src/html-tag.mjs";
import { makeAttributesHTML } from "../src/html.mjs";
import { makeRelativeURI } from "../src/uri.mjs";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

let iconsSpriteSheetURI = "/logos.svg";
export let routes = {
  "/error-box.bundled.js": {
    type: "esbuild",
    esbuildConfig: {
      entryPoints: ["/error-box.mjs"],
    },
  },
  [iconsSpriteSheetURI]: {
    getContentType() {
      return "image/svg+xml";
    },
    async getContentsAsync() {
      return await iconsSpriteSheet.makeExternalFileAsync();
    },
  },
};

export let customComponents = {
  "qljs-content-icon": qljsContentIcon,
  "qljs-icon": qljsIcon,
};

// <qljs-content-icon name="ubuntu" />
//
// Create an <img> with a descriptive alt attribute. Intended for icons
// replacing content.
function qljsContentIcon(attributes, { currentURI }) {
  return qljsAnyIcon(attributes, { currentURI, isContent: true });
}

// <qljs-icon name="ubuntu" />
//
// Create an <img> with a descriptive alt attribute. Intended for decorative
// icons, with an explanation for the icon elsewhere.
function qljsIcon(attributes, { currentURI }) {
  return qljsAnyIcon(attributes, { currentURI, isContent: false });
}

function qljsAnyIcon(attributes, { currentURI, isContent }) {
  let icon = getIcon(attributes.name);
  let elementAttributes = {
    class: `logo ${attributes.class || ""}`,
    alt: isContent ? icon.alt : "",
    title: icon.alt,
    "data-icon": attributes.name,
    ...getExtraIconAttributes(attributes),
  };
  if (icon.spriteSheetItem) {
    return icon.spriteSheetItem.makeReferenceHTML({
      externalFileURI: makeRelativeURI(currentURI, iconsSpriteSheetURI),
      attributes: elementAttributes,
    });
  }
  return html`<img${makeAttributesHTML({
    src: makeRelativeIconURI(icon, currentURI),
    ...elementAttributes,
  })} />`;
}

function getExtraIconAttributes(attributes) {
  let out = {};
  if (attributes.size) {
    out.width = attributes.size;
    out.height = attributes.size;
  }
  for (let attrName in attributes) {
    if (attrName.startsWith("aria-")) {
      out[attrName] = attributes[attrName];
    }
  }
  return out;
}

let icons = {
  chocolatey: { path: "chocolatey.svg", alt: "Chocolatey" },
  "arch-linux": { path: "arch-linux.svg", alt: "Arch Linux" },
  biome: { path: "biome.svg", alt: "Biome" },
  "cli-and-lsp-server": {
    path: "gnome-terminal.svg",
    alt: "CLI and LSP server",
  },
  codespaces: { path: "codespaces.png", alt: "GitHub Codespaces" },
  debian: { path: "debian.svg", alt: "Debian" },
  deno: { path: "deno.svg", alt: "Deno" },
  emacs: { path: "emacs.svg", alt: "Emacs", disableSpritesheet: true },
  github: { path: "github.svg", alt: "GitHub" },
  homebrew: { path: "homebrew.svg", alt: "Homebrew" },
  kate: { path: "kate.svg", alt: "Kate", disableSpritesheet: true },
  linux: { path: "linux.svg", alt: "GNU/Linux" },
  macos: { path: "macos.svg", alt: "macOS" },
  neovim: { path: "neovim.svg", alt: "Neovim", disableSpritesheet: true },
  nix: { path: "nix.svg", alt: "Nix", disableSpritesheet: true },
  "notepad-plus-plus": { path: "notepad-plus-plus.svg", alt: "Notepad++" },
  npm: { path: "npm.svg", alt: "npm" },
  "open-vsx": { path: "open-vsx.svg", alt: "Open VSX" },
  oxc: { path: "oxc.png", alt: "Oxc" },
  "quick-lint-js": { path: "dusty.svg", alt: "quick-lint-js" },
  "quick-lint-js-small": { path: "favicon-32x32.png", alt: "quick-lint-js" },
  "sublime-text": {
    path: "sublime-text.svg",
    alt: "Sublime Text",
    disableSpritesheet: true,
  },
  typescript: { path: "typescript.svg", alt: "TypeScript" },
  ubuntu: { path: "ubuntu.svg", alt: "Ubuntu" },
  vim: { path: "vim.gif", alt: "Vim" },
  vscode: { path: "vscode.png", alt: "Visual Studio Code" },
  webstorm: { path: "webstorm.svg", alt: "WebStorm", disableSpritesheet: true },
  windows: { path: "windows.svg", alt: "Windows" },
};

let iconsSpriteSheet = new ExternalSpriteSheet();
for (let [iconName, icon] of Object.entries(icons)) {
  let spriteSheetItem = null;
  if (path.extname(icon.path) === ".svg") {
    // HACK(#818): Some SVGs are broken in Chrome and Safari when spritesheeted.
    // This appears to be a bug in both WebKit and Chromium:
    // https://bugs.webkit.org/show_bug.cgi?id=65344
    // https://bugs.chromium.org/p/chromium/issues/detail?id=109212
    //
    // Remove these offending SVGs from the spritesheet as a workaround.
    if (!icon.disableSpritesheet) {
      spriteSheetItem = iconsSpriteSheet.addSVG(
        path.join(__dirname, icon.path)
      );
    }
  }
  icon.spriteSheetItem = spriteSheetItem;
}

function getIcon(name) {
  if (!Object.prototype.hasOwnProperty.call(icons, name)) {
    throw new Error(`unknown icon: ${name}`);
  }
  return icons[name];
}

function makeRelativeIconURI(icon, currentURI) {
  return path.posix.relative(currentURI, "/" + icon.path);
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
