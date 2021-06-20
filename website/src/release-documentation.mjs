// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import MarkdownIt from "markdown-it";

let markdownParser = new MarkdownIt("commonmark");

export function releasesMarkdownToHTML(releasesMarkdown) {
  let env = {};
  let tokens = markdownParser.parse(releasesMarkdown, env);
  tokens = removeTitle(tokens);
  tokens = demoteHeadings(tokens);

  let html = markdownParser.renderer.render(
    tokens,
    markdownParser.options,
    env
  );
  return html;
}

function removeTitle(markdownTokens) {
  let filteredTokens = [];
  let inTitle = false;
  for (let token of markdownTokens) {
    if (token.type === "heading_open" && token.tag === "h1") {
      inTitle = true;
    }
    if (!inTitle) {
      filteredTokens.push(token);
    }
    if (token.type === "heading_close" && token.tag === "h1") {
      inTitle = false;
    }
  }
  return filteredTokens;
}

function demoteHeadings(markdownTokens) {
  let demotions = {
    h2: "h3",
    h3: "h4",
    h4: "h5",
  };
  return markdownTokens.map((token) => {
    if (
      (token.type === "heading_open" || token.type === "heading_close") &&
      demotions.hasOwnProperty(token.tag)
    ) {
      return { ...token, tag: demotions[token.tag] };
    } else {
      return token;
    }
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
