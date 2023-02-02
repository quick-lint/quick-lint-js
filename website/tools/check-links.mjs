#!/usr/bin/env node

// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import * as parse5 from "parse5-sax-parser";
import http from "node:http";
import https from "node:https";
import path from "node:path";
import url from "node:url";

let logRedirects = false;

let allowMails = [
  "strager.nds@gmail.com",
  "nicolas@petton.fr",
  "lewing@isc.tamu.edu",
  "don.h@free.fr",
  "Bram@vim.org",
  "maintainer@vim.org",
  "marketing@jetbrains.com",
  "trademarks@archlinux.org",
];

let headers = {
  "User-Agent":
    "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.3",
};

function checkFragment(soup, fragment) {
  if (fragment) {
    return soup.findAllWithID(fragment).length > 0;
  }
  return true;
}

class URLPacket {
  parent;
  url;
  defragedURL;
  fragment;

  constructor(parent, url, defragedURL = null, fragment = null) {
    this.parent = parent;
    this.url = url;
    this.defragedURL = defragedURL;
    this.fragment = fragment;
  }
}

// Raised when requests returns response code other than 200
class URLNotFound extends Error {
  constructor(responseCode) {
    super();
    this.responseCode = responseCode;
  }
}

async function checkResponseAsync(url) {
  let response = await httpRequestAsync(url, {
    method: "HEAD",
    redirect: "follow",
    timeoutMilliseconds: 10 * 1000,
    headers: headers,
  });
  if (!response.ok) {
    if (
      !(await httpRequestAsync(url, { method: "GET", redirect: "follow" })).ok
    ) {
      throw new URLNotFound(response.status);
    }
  }
  return response;
}

export class Crawler {
  initialURL;
  checkExternal;
  visitedURLs = [];
  visitedURLsSoup = new Map();
  externalLinksToCheck = [];
  brokenLinks = [];

  constructor({ initialURL, checkExternal }) {
    this.initialURL = initialURL;
    this.checkExternal = checkExternal;
  }

  isExternalURL(url) {
    return new URL(url).host !== new URL(this.initialURL).host;
  }

  getURLsFromPage(soup) {
    return [...soup.findAllAnchorHrefs(), ...soup.findAllScriptSrcs()];
  }

  inAllowedFileSoup(urlResponse) {
    let url = new URL(urlResponse.url);
    if (url.pathname.endsWith("/")) {
      // URLs which look like directories, such as
      // https://quick-lint-js.com/blog/, should always serve HTML.
      return true;
    }
    return ["text/html"].includes(urlResponse.contentType);
  }

  reportError(error, packet) {
    this.brokenLinks.push(packet.url);
    console.error(`(${error}) ${packet.parent}, ${packet.url}`);
  }

  async startCrawlAsync() {
    await this.crawlAndReportAsync(this.initialURL, this.initialURL);
    if (this.checkExternal) {
      await this.checkExternalLinksAsync(this.externalLinksToCheck);
    }
  }

  async getURLsToCrawlAsync(packet) {
    let urlsFromPage = [];
    let result = await httpRequestAsync(packet.defragedURL, { method: "GET" });
    let soup = await parseHTMLIntoSoupAsync(await result.text());
    this.visitedURLsSoup.set(packet.defragedURL, soup);
    if (!checkFragment(soup, packet.fragment)) {
      this.reportError("fragment missing", packet);
    }
    urlsFromPage = this.getURLsFromPage(soup);
    return urlsFromPage;
  }

  checkMailLink(packet) {
    let mail = packet.url.split(":", 2)[1];
    if (!allowMails.includes(mail)) {
      this.reportError("unknown mail", packet);
    }
  }

  async checkInternalLinksAsync(packet) {
    try {
      let response = await checkResponseAsync(packet.defragedURL);
      if (this.inAllowedFileSoup(response)) {
        let urlsFromPage = await this.getURLsToCrawlAsync(packet);
        for (let link of urlsFromPage) {
          await this.crawlAndReportAsync(response.url, link);
        }
      }
    } catch (e) {
      if (e instanceof URLNotFound) {
        this.reportError(`${e.responseCode} error`, packet);
      } else {
        throw e;
      }
    }
  }

  async checkExternalLinksAsync(urls) {
    await Promise.all(
      urls.map(async (packet) => {
        try {
          await checkResponseAsync(packet.url);
        } catch (e) {
          if (e instanceof URLNotFound) {
            this.reportError(`${e.responseCode} error`, packet);
          } else {
            console.error(
              `${packet.parent}, ${packet.url} generated an exception: ${e}`
            );
          }
        }
      })
    );
  }

  async crawlAndReportAsync(parentURL, link) {
    let urlObject = new URL(link, parentURL);
    let url = urlObject.toString();
    switch (urlObject.protocol) {
      case "mailto:":
        this.checkMailLink(new URLPacket(parentURL, url));
        break;

      case "http:":
      case "https:":
        await this.checkHTTPLinkAsync(parentURL, url);
        break;
    }
  }

  async checkHTTPLinkAsync(parentURL, url) {
    if (this.visitedURLs.includes(url)) {
      return;
    }
    this.visitedURLs.push(url);
    let defragedURL = new URL(url);
    let fragment = defragedURL.hash.replace(/^#/, "");
    defragedURL.hash = "";
    defragedURL = defragedURL.toString();
    if (!this.visitedURLsSoup.has(defragedURL)) {
      if (this.isExternalURL(defragedURL)) {
        this.externalLinksToCheck.push(new URLPacket(parentURL, url));
        this.visitedURLsSoup.set(defragedURL, null);
      } else {
        await this.checkInternalLinksAsync(
          new URLPacket(parentURL, url, defragedURL, fragment)
        );
      }
    } else {
      let soup = this.visitedURLsSoup.get(defragedURL);
      if (soup !== null && !checkFragment(soup, fragment)) {
        this.reportError(
          "fragment missing",
          new URLPacket(parentURL, url, fragment)
        );
      }
    }
  }
}

async function parseHTMLIntoSoupAsync(html) {
  let parser = new parse5.SAXParser();
  let soup = new Soup();

  await new Promise((resolve, reject) => {
    parser.on("startTag", (startTag) => {
      // If there are duplicates, return the last value.
      function getAttr(name) {
        let value = null;
        for (let { name: attrName, value: attrValue } of startTag.attrs) {
          if (attrName === name) {
            value = attrValue;
          }
        }
        return value;
      }

      let id = getAttr("id");
      if (id !== null) {
        let tagNamesForID = soup._tagNamesByID.get(id);
        if (tagNamesForID === undefined) {
          tagNamesForID = [];
          soup._tagNamesByID.set(id, tagNamesForID);
        }
        tagNamesForID.push(startTag.tagName);
      }

      switch (startTag.tagName.toLowerCase()) {
        case "a": {
          let href = getAttr("href");
          if (href !== null) {
            soup._anchorHrefs.push(href);
          }
          break;
        }

        case "script": {
          let src = getAttr("src");
          if (src !== null) {
            soup._scriptSrcs.push(src);
          }
          break;
        }
      }
    });

    parser.on("end", () => {
      resolve();
    });
    parser.write(html);
    parser.end();
  });

  return soup;
}

// TODO(strager): Rename this. Beautiful Soup is a Python HTML processing
// library. We don't use Beautiful Soup anymore, and this class does not
// implement its interface.
class Soup {
  _anchorHrefs = [];
  _scriptSrcs = [];
  _tagNamesByID = new Map();

  findAllAnchorHrefs() {
    return [...this._anchorHrefs];
  }
  findAllScriptSrcs() {
    return [...this._scriptSrcs];
  }

  findAllWithID(id) {
    return this._tagNamesByID.get(id) || [];
  }
}

async function mainAsync() {
  let checkExternal = false;
  let help = false;
  let url = null;

  let args = process.argv.slice(2);
  for (let arg of args) {
    if (arg === "--all") {
      checkExternal = true;
    } else if (arg === "--help" || arg === "-h") {
      help = true;
    } else if (arg.startsWith("-")) {
      console.error(`error: unknown option: ${arg}`);
      process.exit(1);
    } else {
      if (url === null) {
        url = arg;
      } else {
        console.error(`error: unexpected argument: ${arg}`);
        process.exit(1);
      }
    }
  }

  if (help) {
    showHelp();
    process.exit(0);
  }
  if (url === null) {
    console.error(`error: missing URL`);
    showHelp(console.error);
    process.exit(1);
  }

  let crawler = new Crawler({ initialURL: url, checkExternal: checkExternal });
  await crawler.startCrawlAsync();

  if (crawler.brokenLinks.length > 0) {
    process.exit(1);
  }
}

function showHelp(log = console.log) {
  let cwd = process.cwd();
  let [nodePath, scriptPath] = process.argv;
  log(`usage: ${nodePath} ${path.relative(cwd, scriptPath)} [--all] URL`);
}

function zip(xs, ys) {
  return xs.slice(0, ys.length).map((x, index) => [x, ys[index]]);
}

// Poorly-implemented version of 'fetch' for older versions of Node.js.
function httpRequestAsync(
  url,
  {
    method = "GET",
    redirect = "never",
    headers = {},
    timeoutMilliseconds = null,
  }
) {
  return new Promise((resolve, reject) => {
    let httpModule = new URL(url).protocol === "https:" ? https : http;
    let clientRequest = httpModule.request(url, {
      method: method,
      timeout: timeoutMilliseconds === null ? undefined : timeoutMilliseconds,
    });
    clientRequest.on("error", (error) => {
      reject(error);
    });
    clientRequest.on("response", (response) => {
      let isRedirectResponse = [301, 302, 307, 308].includes(
        response.statusCode
      );
      if (isRedirectResponse && redirect === "follow") {
        let location = response.headers.location;
        if (!location) {
          reject(
            new Error(
              `received status ${response.status} but no location header`
            )
          );
          return;
        }
        // TODO(strager): Impose a redirect depth limit.
        // TODO(strager): timeoutMilliseconds should apply to the entire fetch,
        // not just one subfetch.
        let newURL = new URL(location, url).toString();
        if (logRedirects) {
          console.log(`Redirecting from ${url} to ${newURL}...`);
        }
        httpRequestAsync(newURL, {
          method: method,
          redirect: redirect,
          timeoutMilliseconds: timeoutMilliseconds,
        }).then(resolve, reject);
        return;
      }

      response.setEncoding("utf8");
      let textPromise = new Promise((resolveText, rejectText) => {
        let body = "";
        response.on("data", (chunk) => {
          body += chunk;
        });
        response.on("error", (error) => {
          rejectText(error);
        });
        response.on("end", () => {
          resolveText(body);
        });
      });

      resolve({
        status: response.statusCode,
        url: url,
        contentType: response.headers["content-type"],
        get ok() {
          return 200 <= this.status && this.status < 400;
        },
        async text() {
          return await textPromise;
        },
      });
    });

    for (let headerName in headers) {
      if (Object.hasOwnProperty.call(headers, headerName)) {
        clientRequest.setHeader(headerName, headers[headerName]);
      }
    }
    clientRequest.end();
  });
}

let isRunningAsScript = process.argv[1] === url.fileURLToPath(import.meta.url);
if (isRunningAsScript) {
  mainAsync().catch((error) => {
    console.error(error.stack);
    process.exit(1);
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
