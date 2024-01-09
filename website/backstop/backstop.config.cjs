// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

let paths = [
  "/",
  "/benchmarks/",
  "/blog/",
  "/blog/bug-journey/",
  "/blog/cpp-vs-rust-build-times/",
  "/blog/show-js-errors-neovim-macos/",
  "/blog/syntax-errors-2021/",
  "/blog/version-1.0/",
  "/blog/version-2.0/",
  "/blog/version-3.0/",
  "/blog/why-another-javascript-linter/",
  "/cli/",
  "/config/",
  "/contact/",
  "/contribute/",
  "/contribute/authors/",
  "/contribute/build-from-source/",
  "/contribute/build-from-source/cross-compiling/",
  "/contribute/build-from-source/linux/",
  // '/contribute/build-from-source/macos/',
  // '/contribute/build-from-source/nix/',
  // '/contribute/build-from-source/windows/',
  // '/contribute/coding-standards/',
  "/contribute/create-diagnostic/",
  // '/contribute/submit/',
  "/crash-report/",
  "/demo/",
  "/docs/",
  "/docs/lsp/",
  "/errors/",
  "/errors/E0001/",
  // (There is little benefit in screenshotting all error pages. Just one is
  // fine.)
  "/hiring/",
  "/install/",
  "/install/cli/",
  "/install/cli/arch-linux/",
  // (There is little benefit in screenshotting all install pages. Just a few is
  // fine.)
  // '/install/cli/chocolatey/',
  // '/install/cli/debian/',
  // '/install/cli/homebrew/',
  // '/install/cli/nix/',
  // '/install/cli/npm/',
  // '/install/cli/scoop/',
  // '/install/cli/static-linux/',
  // '/install/cli/static-macos/',
  // '/install/cli/static-windows/',
  // '/install/cli/winget/',
  // '/install/emacs/',
  // '/install/emacs/arch-linux/',
  // '/install/emacs/chocolatey/',
  // '/install/emacs/configure/',
  // '/install/emacs/debian/',
  // '/install/emacs/homebrew/',
  // '/install/emacs/nix/',
  // '/install/emacs/npm-posix/',
  // '/install/emacs/npm-windows/',
  // '/install/emacs/scoop/',
  // '/install/emacs/static-linux/',
  // '/install/emacs/static-macos/',
  // '/install/emacs/static-windows/',
  // '/install/emacs/winget/',
  // '/install/kate/',
  // '/install/kate/arch-linux/',
  // '/install/kate/chocolatey/',
  // '/install/kate/debian/',
  // '/install/kate/homebrew/',
  // '/install/kate/nix/',
  // '/install/kate/npm-posix/',
  // '/install/kate/npm-windows/',
  // '/install/kate/scoop/',
  // '/install/kate/static-linux/',
  // '/install/kate/static-macos/',
  // '/install/kate/static-windows/',
  // '/install/kate/winget/',
  "/install/neovim/",
  // '/install/neovim/arch-linux/',
  // '/install/neovim/chocolatey/',
  // '/install/neovim/debian/',
  // '/install/neovim/homebrew/',
  // '/install/neovim/nix/',
  // '/install/neovim/npm-posix/',
  // '/install/neovim/npm-windows/',
  // '/install/neovim/scoop/',
  // '/install/neovim/static-linux/',
  // '/install/neovim/static-macos/',
  // '/install/neovim/static-windows/',
  "/install/neovim/winget/",
  "/install/sublime/",
  // '/install/sublime/arch-linux/',
  "/install/sublime/chocolatey/",
  // '/install/sublime/debian/',
  // '/install/sublime/homebrew/',
  // '/install/sublime/nix/',
  // '/install/sublime/scoop/',
  // '/install/sublime/static-linux/',
  // '/install/sublime/static-macos/',
  // '/install/sublime/static-windows/',
  // '/install/sublime/winget/',
  // '/install/vim/',
  // '/install/vim/arch-linux/',
  // '/install/vim/chocolatey/',
  // '/install/vim/debian/',
  // '/install/vim/homebrew/',
  // '/install/vim/nix/',
  // '/install/vim/npm-posix/',
  // '/install/vim/npm-windows/',
  // '/install/vim/scoop/',
  // '/install/vim/static-linux/',
  // '/install/vim/static-macos/',
  // '/install/vim/static-windows/',
  // '/install/vim/winget/',
  "/install/vscode/",
  "/install/vscodium/",
  // The licenses page is slow to screenshot. Don't bother.
  // '/license/',
  "/merch/",
  "/releases/",
];

let baseURI = "http://localhost:9001";

module.exports = {
  id: "backstop_default",
  viewports: [
    {
      label: "phone",
      width: 320,
      height: 480,
    },
    {
      label: "desktop-light",
      width: 1920,
      height: 1080,
      "prefers-color-scheme": "light",
    },
    {
      label: "desktop-dark",
      width: 1920,
      height: 1080,
      "prefers-color-scheme": "dark",
    },
  ],
  onBeforeScript: "backstop-hook.cjs",
  scenarios: paths.map((path) => ({ label: path, url: `${baseURI}${path}` })),
  paths: {
    bitmaps_reference: "backstop_data/bitmaps_reference",
    bitmaps_test: "backstop_data/bitmaps_test",
    engine_scripts: ".",
    html_report: "backstop_data/html_report",
    ci_report: "backstop_data/ci_report",
  },
  misMatchThreshold: 0,
  report: ["browser"],
  engine: "puppeteer",
  engineOptions: {
    args: ["--no-sandbox"],
  },
  asyncCaptureLimit: 5,
  asyncCompareLimit: 50,
  debug: false,
  debugWindow: false,
};

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
