// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import { describe, it } from "node:test";
import assert from "node:assert/strict";
import path from "node:path";
import url from "node:url";
import {
  parseApacheTimestamp,
  parseLogFileAsync,
} from "../src/parse-log-file.mjs";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

async function parseLogFileToArrayAsync(path, logFormat) {
  let allLogEntries = [];
  await parseLogFileAsync(path, logFormat, (logEntries) => {
    for (let logEntry of logEntries) {
      allLogEntries.push({ ...logEntry });
    }
  });
  return allLogEntries;
}

describe("parseLogFileAsync", () => {
  it("example-apache.log", async () => {
    let logEntries = await parseLogFileToArrayAsync(
      path.join(__dirname, "example-apache.log"),
      {
        type: "apache",
        format: '%v %h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-Agent}i"',
      }
    );
    assert.deepEqual(logEntries, [
      {
        serverName: "c.quick-lint-js.com",
        remoteHostName: "114.119.132.159",
        remoteLogname: "-",
        remoteUser: "-",
        // 25/Aug/2023:03:05:22 +0000
        requestTimestamp: Date.UTC(2023, 7, 25, 3, 5, 22),
        httpMethod: "GET",
        uri: "/builds/8ce11369a188603c3b8f95305a25752009029eae/?C=M%3BO%3DA",
        httpVersion: "HTTP/1.1",
        finalStatus: 200,
        responseBodySize: 643,
        Apache_Referer:
          "https://c.quick-lint-js.com/builds/8ce11369a188603c3b8f95305a25752009029eae/?C=D%3BO%3DA",
        "Apache_User-Agent":
          "Mozilla/5.0 (Linux; Android 7.0;) AppleWebKit/537.36 (KHTML, like Gecko) Mobile Safari/537.36 (compatible; PetalBot;+https://webmaster.petalsearch.com/site/petalbot)",
      },
      {
        serverName: "c.quick-lint-js.com",
        remoteHostName: "51.222.253.3",
        remoteLogname: "-",
        remoteUser: "-",
        // 25/Aug/2023:03:05:32 +0000
        requestTimestamp: Date.UTC(2023, 7, 25, 3, 5, 32),
        httpMethod: "GET",
        uri: "/builds/c25efb0f2183f50bdad8b06a34908f7212119a19/vim/?C=S;O=D",
        httpVersion: "HTTP/2.0",
        finalStatus: 200,
        responseBodySize: 503,
        Apache_Referer: "-",
        "Apache_User-Agent":
          "Mozilla/5.0 (compatible; AhrefsBot/7.0; +http://ahrefs.com/robot/)",
      },
      {
        serverName: "c.quick-lint-js.com",
        remoteHostName: "216.244.66.236",
        remoteLogname: "-",
        remoteUser: "-",
        // 25/Aug/2023:01:49:59 +0000
        requestTimestamp: Date.UTC(2023, 7, 25, 1, 49, 59),
        httpMethod: "GET",
        uri: "/debian-testing/dists/experimental/main/Contents-amd64",
        httpVersion: "HTTP/1.1",
        finalStatus: 404,
        responseBodySize: 282,
        Apache_Referer: "-",
        "Apache_User-Agent":
          "Mozilla/5.0 (compatible; DotBot/1.2; +https://opensiteexplorer.org/dotbot; help@moz.com)",
      },
    ]);
  });

  it("apache-escape.log", async () => {
    let logEntries = await parseLogFileToArrayAsync(
      path.join(__dirname, "apache-escape.log"),
      {
        type: "apache",
        format: '%v %h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-Agent}i"',
      }
    );
    assert.equal(logEntries[0]["Apache_Referer"], "'escape\\test\"");
    assert.equal(logEntries[0]["Apache_User-Agent"], "escape\ttest");
  });

  it("apache-malicious.log malicious request is ignored", async () => {
    let logEntries = await parseLogFileToArrayAsync(
      path.join(__dirname, "apache-malicious.log"),
      {
        type: "apache",
        format: '%v %h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-Agent}i"',
      }
    );
    assert.deepEqual(
      logEntries.map((entry) => entry.remoteHostName),
      [
        "51.222.253.5",
        // "43.158.214.10",  // Malicious entry dropped.
        "114.119.132.38",
      ]
    );
  });

  it("apache-junk.log broken log lines are ignored", async () => {
    let logEntries = await parseLogFileToArrayAsync(
      path.join(__dirname, "apache-junk.log"),
      {
        type: "apache",
        format: '%v %h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-Agent}i"',
      }
    );
    assert.deepEqual(
      logEntries.map((entry) => entry.remoteHostName),
      [
        // 51.222.253.3 entries should be ignored.
        "114.119.132.159",
      ]
    );
  });

  it("example-apache.log.gz", async () => {
    let logFormat = {
      type: "apache",
      format: '%v %h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-Agent}i"',
    };
    let logEntries = await parseLogFileToArrayAsync(
      path.join(__dirname, "example-apache.log.gz"),
      logFormat
    );

    // example-apache.log.gz is the same as example-apache.log but gzipped.
    let expectedLogEntries = await parseLogFileToArrayAsync(
      path.join(__dirname, "example-apache.log"),
      logFormat
    );
    assert.deepEqual(logEntries, expectedLogEntries);
  });
});

describe("parseApacheTimestamp", () => {
  for (let [input, expected] of [
    // FIXME(strager): Are month days 0-padded?
    ["01/Jan/1990:01:02:03 +0000", Date.UTC(1990, 0, 1, 1, 2, 3)],
    ["18/Sep/2011:19:18:28 -0400", Date.UTC(2011, 8, 18, 23, 18, 28)],
    ["25/Aug/2023:03:05:22 +0000", Date.UTC(2023, 7, 25, 3, 5, 22)],
  ]) {
    it(input, () => {
      assert.equal(parseApacheTimestamp(input), expected);
    });
  }
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
