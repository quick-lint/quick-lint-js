// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import MarkdownIt from "markdown-it";
import assert from "node:assert";
import { retagHeadings } from "./markdown.mjs";

let markdownParser = new MarkdownIt("commonmark");

export function releasesMarkdownToHTML(releasesMarkdown) {
  let env = {};
  let tokens = markdownParser.parse(releasesMarkdown, env);
  tokens = removeTitle(tokens);
  tokens = linkifyVersions(tokens);

  let html = markdownParser.renderer.render(
    tokens,
    markdownParser.options,
    env
  );
  return html;
}

export function parseReleaseVersionsFromMarkdown(releasesMarkdown) {
  let env = {};
  let tokens = markdownParser.parse(releasesMarkdown, env);

  let versionHeadingSpanIndexes = getVersionHeadingSpans(tokens, "h2");
  let versions = [];
  for (let [headingBeginIndex, headingEndIndex] of versionHeadingSpanIndexes) {
    let textTokens = tokens.slice(headingBeginIndex + 1, headingEndIndex);
    let text = textTokens.map((token) => token.content).join("");
    let versionInfo = extractVersionInfoFromHeadingText(text);
    if (versionInfo === null) {
      continue;
    }
    versions.push(versionInfo);
  }
  return versions;
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

// @returns Array<[number, number]>
function getVersionHeadingSpans(markdownTokens, headingTag) {
  let spans = [];
  for (let i = 0; i < markdownTokens.length; ++i) {
    let token = markdownTokens[i];
    if (token.tag === headingTag) {
      if (token.type === "heading_open") {
        spans.push([i]);
      }
      if (token.type === "heading_close") {
        spans[spans.length - 1].push(i);
      }
    }
  }
  return spans;
}

function linkifyVersions(markdownTokens) {
  markdownTokens = [...markdownTokens];
  let versionHeadingSpanIndexes = getVersionHeadingSpans(markdownTokens, "h2");
  versionHeadingSpanIndexes.reverse();
  for (let [headingBeginIndex, headingEndIndex] of versionHeadingSpanIndexes) {
    let textTokens = markdownTokens.slice(
      headingBeginIndex + 1,
      headingEndIndex
    );
    let text = textTokens.map((token) => token.content).join("");
    let versionInfo = extractVersionInfoFromHeadingText(text);
    if (versionInfo === null) {
      continue;
    }
    let currentVersionID = versionInfo.version;

    let headingOpenToken = markdownTokens[headingBeginIndex];
    let headingCloseToken = markdownTokens[headingEndIndex];
    let newTokens = [
      {
        ...headingOpenToken,
        attrs: [...(headingOpenToken.attrs ?? []), ["id", currentVersionID]],
      },
      {
        type: "link_open",
        tag: "a",
        attrs: [["href", `#${currentVersionID}`]],
      },
      ...textTokens,
      {
        type: "link_close",
        tag: "a",
      },
      headingCloseToken,
    ];
    markdownTokens.splice(
      headingBeginIndex,
      headingEndIndex - headingBeginIndex,
      ...newTokens
    );
  }

  return markdownTokens;
}

function extractVersionInfoFromHeadingText(text) {
  let match = text.match(
    /^(?<version>(?:\d+\.)*\d+) \((?<date>\d+-\d+-\d+)\)$/
  );
  if (match === null) {
    if (text !== "Unreleased") {
      console.warn(`warning: failed to parse version info from ${text}`);
    }
    return null;
  }
  return {
    version: match.groups.version,
    date: match.groups.date,
  };
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
