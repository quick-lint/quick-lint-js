// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "fs";
import http from "http";
import os from "os";
import path from "path";
import { listenAsync, urlFromServerAddress } from "../src/net.mjs";
import { Crawler } from "../tools/check-links.mjs";

describe("check-links", () => {
  let createdServers = [];

  async function makeServerAsync(requestListener) {
    let server = http.createServer(requestListener);
    await listenAsync(server, { host: "localhost", port: 0 });
    let serverAddress = server.address();
    createdServers.push(server);
    return {
      server: server,
      url: urlFromServerAddress(serverAddress).toString(),
    };
  }

  afterEach(async () => {
    for (let server of createdServers) {
      server.close();
    }
    createdServers = [];
  });

  it("finds broken <a> link on home page", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<a href='/doesnotexist'>link</a>");
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([`${url}doesnotexist`]);
  });

  it("finds broken <a> link on sub page page", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<a href='/subpage'>link</a>");
      } else if (req.url === "/subpage") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<a href='/doesnotexist'>link</a>");
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([`${url}doesnotexist`]);
  });

  it("reports broken <a> link once if repeated on page", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(
          "<!DOCTYPE html>\n" +
            "<a href='/broken'>link 1</a>\n" +
            "<a href='/broken'>link 2</a>"
        );
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([`${url}broken`]);
  });

  it("reports broken <a> link for each linking page if linked from multiple pages", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(
          "<!DOCTYPE html>\n" +
            "<a href='/broken'>link 1</a>\n" +
            "<a href='/subpage'>link 2</a>"
        );
      } else if (req.url === "/subpage") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<a href='/broken'>link</a>");
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([`${url}broken`, `${url}broken`]);
  });

  it("finds broken <script> link", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<script src='/doesnotexist'></script>");
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([`${url}doesnotexist`]);
  });

  it("does not report working <a> link with fragment", async () => {
    // TODO(strager): Simplify this test, putting the fragment on the home page.
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<a href='/subpage#frag'>link</a>");
      } else if (req.url === "/subpage") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<h1 id='frag'>title</h1>");
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([]);
  });

  it("finds broken <a> link with fragment", async () => {
    // TODO(strager): Simplify this test, putting the fragment on the home page.
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<a href='/subpage#frag'>link</a>");
      } else if (req.url === "/subpage") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<h1 id='notfrag'>title</h1>");
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([`${url}subpage#frag`]);
  });

  it("does not request pages multiple times in cycle", async () => {
    let rootHits = 0;
    let subpageHits = 0;
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        rootHits += 1;
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<a href='/subpage'>link</a>");
      } else if (req.url === "/subpage") {
        subpageHits += 1;
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n<a href='/'>link</a>");
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([]);
    expect(rootHits).toEqual(1);
    expect(subpageHits).toEqual(1);
  });

  it("does not request pages multiple times with different fragments", async () => {
    let subpageHits = 0;
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(
          "<!DOCTYPE html>\n" +
            "<a href='/subpage#frag1'>link</a>\n" +
            "<a href='/subpage#frag2'>link</a>"
        );
      } else if (req.url === "/subpage") {
        subpageHits += 1;
        res.writeHead(200, { "content-type": "text/html" });
        res.end("<!DOCTYPE html>\n");
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(subpageHits).toEqual(1);
  });

  it("finds broken <a> link with multiple fragments on same page", async () => {
    let subpageHits = 0;
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(
          "<!DOCTYPE html>\n" +
            "<a href='/subpage#frag1'>link</a>\n" +
            "<a href='/subpage#frag2'>link</a>"
        );
      } else if (req.url === "/subpage") {
        subpageHits += 1;
        notFound(res);
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(subpageHits).toEqual(1);
    expect(crawler.brokenLinks.sort()).toEqual([
      `${url}subpage#frag1`,
      `${url}subpage#frag2`,
    ]);
  });

  it("does not check external links if opted out", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(`<!DOCTYPE html>\n<a href='${externalURL}'>link</a>`);
      } else {
        notFound(res);
      }
    });

    let externalHits = [];
    let { url: externalURL } = await makeServerAsync((req, res) => {
      externalHits.push(req.url);
      notFound(res);
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([]);
    expect(externalHits).toEqual([]);
  });

  it("fetches external links if opted in", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(`<!DOCTYPE html>\n<a href='${externalURL}'>link</a>`);
      } else {
        notFound(res);
      }
    });

    let externalHits = [];
    let { url: externalURL } = await makeServerAsync((req, res) => {
      externalHits.push({ url: req.url, method: req.method });
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(`<!DOCTYPE html>\n<a href='/other'>link</a>`);
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: true });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([]);
    // For performance reasons, we expect a HEAD request, not a GET request.
    expect(externalHits, [{ url: "/", method: "HEAD" }]);
  });

  it("external link uses GET request if HEAD request fails", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(`<!DOCTYPE html>\n<a href='${externalURL}'>link</a>`);
      } else {
        notFound(res);
      }
    });

    let externalHits = [];
    let { url: externalURL } = await makeServerAsync((req, res) => {
      externalHits.push({ url: req.url, method: req.method });
      if (req.url === "/") {
        if (req.method === "HEAD") {
          res.writeHead(500);
          res.end("500 server error (HEAD not supported)");
        } else {
          res.writeHead(200, { "content-type": "text/html" });
          res.end(`<!DOCTYPE html>\n<a href='/other'>link</a>`);
        }
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: true });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([]);
    expect(externalHits, [
      { url: "/", method: "HEAD" },
      { url: "/", method: "GET" },
    ]);
  });

  it("finds broken external links if opted in", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(`<!DOCTYPE html>\n<a href='${externalURL}'>link</a>`);
      } else {
        notFound(res);
      }
    });

    let { url: externalURL } = await makeServerAsync((req, res) => {
      notFound(res);
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: true });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([externalURL]);
  });

  it("finds broken external links after redirect if opted in", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(`<!DOCTYPE html>\n<a href='${externalURL}'>link</a>`);
      } else {
        notFound(res);
      }
    });

    let externalRedirectedHits = 0;
    let { url: externalURL } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(302, { Location: "/redirected" });
        res.end();
      } else if (req.url === "/redirected") {
        if (req.method === "HEAD") {
          // FIXME(strager): Crawler shouldn't send us GET requests.
          externalRedirectedHits += 1;
        }
        notFound(res);
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: true });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([externalURL]);
    expect(externalRedirectedHits).toEqual(1);
  });

  it("user agent for external links is Google Chrome-like", async () => {
    let { url } = await makeServerAsync((req, res) => {
      res.writeHead(200, { "content-type": "text/html" });
      res.end(`<!DOCTYPE html>\n<a href='${externalURL}'>link</a>`);
    });

    let externalUserAgents = [];
    let { url: externalURL } = await makeServerAsync((req, res) => {
      externalUserAgents.push(req.headers["user-agent"]);
      res.writeHead(200, { "content-type": "text/html" });
      res.end("<!DOCTYPE html>\n<h1>hello</h1>");
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: true });
    await crawler.startCrawlAsync();

    expect(externalUserAgents).not.toEqual([]);
    expect(externalUserAgents[0]).toMatch(/Chrome/);
  });

  it("allows good email links", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(
          "<!DOCTYPE html>\n<a href='mailto:strager.nds@gmail.com'>email me</a>"
        );
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual([]);
  });

  it("reports bad email links", async () => {
    let { url } = await makeServerAsync((req, res) => {
      if (req.url === "/") {
        res.writeHead(200, { "content-type": "text/html" });
        res.end(
          "<!DOCTYPE html>\n<a href='mailto:bademail@example.com'>email me</a>"
        );
      } else {
        notFound(res);
      }
    });

    let crawler = new Crawler({ initialURL: url, checkExternal: false });
    await crawler.startCrawlAsync();

    expect(crawler.brokenLinks).toEqual(["mailto:bademail@example.com"]);
  });
});

function notFound(res) {
  res.writeHead(404);
  res.end("404 not found");
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
