// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import path from "path";
import { html } from "../src/html-tag.mjs";

export let routes = {
  "/error-box.bundled.js": {
    type: "esbuild",
    esbuildConfig: {
      entryPoints: ["/error-box.mjs"],
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
  let icon = getIcon(attributes.name);
  return html`<img
    src="${makeRelativeIconURI(icon, currentURI)}"
    alt="${icon.alt}"
    title="${icon.alt}"
    class="os-logo ${attributes.class || ""}"
    ${getExtraIconAttributesHTML(attributes)}
  />`;
}

// <qljs-icon name="ubuntu" />
//
// Create an <img> with a descriptive alt attribute. Intended for decorative
// icons, with an explanation for the icon elsewhere.
function qljsIcon(attributes, { currentURI }) {
  let icon = getIcon(attributes.name);
  return html`<img
    src="${makeRelativeIconURI(icon, currentURI)}"
    class="os-logo ${attributes.class || ""}"
    alt=""
    title="${icon.alt}"
    ${getExtraIconAttributesHTML(attributes)}
  />`;
}

function getExtraIconAttributesHTML(attributes) {
  let html = "";
  if (attributes.size) {
    html += ` width="${attributes.size}" height="${attributes.size}"`;
  }
  for (let attrName in attributes) {
    if (attrName.startsWith("aria-")) {
      // TODO(strager): HTML-escape.
      html += ` ${attrName}=${attributes[attrName]}`;
    }
  }
  return html;
}

let icons = {
  chocolatey: { path: "chocolatey.svg", alt: "Chocolatey" },
  "arch-linux": { path: "arch-linux.svg", alt: "Arch Linux" },
  atom: { path: "atom.svg", alt: "Atom" },
  "cli-and-lsp-server": {
    path: "gnome-terminal.svg",
    alt: "CLI and LSP server",
  },
  codespaces: { path: "codespaces.png", alt: "GitHub Codespaces" },
  debian: { path: "debian.svg", alt: "Debian" },
  emacs: { path: "emacs.svg", alt: "Emacs" },
  github: { path: "github.svg", alt: "GitHub" },
  homebrew: { path: "homebrew.svg", alt: "Homebrew" },
  kate: { path: "kate.svg", alt: "Kate" },
  linux: { path: "linux.svg", alt: "GNU/Linux" },
  macos: { path: "macos.svg", alt: "macOS" },
  neovim: { path: "neovim.svg", alt: "Neovim" },
  nix: { path: "nix.svg", alt: "Nix" },
  "notepad-plus-plus": { path: "notepad-plus-plus.svg", alt: "Notepad++" },
  npm: { path: "npm.svg", alt: "npm" },
  "open-vsx": { path: "open-vsx.svg", alt: "Open VSX" },
  "sublime-text": { path: "sublime-text.svg", alt: "Sublime Text" },
  ubuntu: { path: "ubuntu.svg", alt: "Ubuntu" },
  vim: { path: "vim.gif", alt: "Vim" },
  vscode: { path: "vscode.png", alt: "Visual Studio Code" },
  webstorm: { path: "webstorm.svg", alt: "WebStorm" },
  windows: { path: "windows.svg", alt: "Windows" },
};

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
