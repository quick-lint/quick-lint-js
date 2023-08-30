// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "node:fs";
import nodeUtil from "node:util";
import stream from "node:stream";
import zlib from "node:zlib";

let pipe = nodeUtil.promisify(stream.pipeline);

let batchSize = 10000;

// Calls onLogEntries zero or more times. Each call receives an array of one or
// more log entries found in the given file.
//
// onLogEntries might return the same array and subobjects each call.
export async function parseLogFileAsync(path, logFormat, onLogEntries) {
  if (
    !(
      logFormat.type === "apache" &&
      logFormat.format ===
        '%v %h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-Agent}i"'
    )
  ) {
    throw new Error("unsupported log format");
  }
  let logData = await readPossiblyGZippedFileAsTextAsync(path);

  let entries = [];
  for (
    let batchEntryIndex = 0;
    batchEntryIndex < batchSize;
    ++batchEntryIndex
  ) {
    entries.push({
      serverName: "",
      remoteHostName: "",
      remoteLogname: "",
      remoteUser: "",
      requestTimestamp: null,
      httpMethod: "",
      uri: "",
      httpVersion: "",
      finalStatus: 0,
      responseBodySize: 0,
      Apache_Referer: "",
      "Apache_User-Agent": "",
    });
  }

  let stringIndex = 0;
  let valid = true;
  let nextLineIndex;

  function parseUntilAndSkip(terminator) {
    let result = parseUntil(terminator);
    stringIndex += terminator.length;
    return result;
  }
  function parseUntil(terminator) {
    let beginIndex = stringIndex;
    skipUntil(terminator);
    return logData.slice(beginIndex, stringIndex);
  }
  function skipUntil(terminator) {
    let terminatorIndex = logData.indexOf(terminator, stringIndex);
    stringIndex = terminatorIndex;
  }
  function skipThrough(terminator) {
    skipUntil(terminator);
    stringIndex += terminator.length;
  }
  function expectSkip(expected) {
    if (logData.startsWith(expected, stringIndex)) {
      stringIndex += expected.length;
    } else {
      valid = false;
    }
  }

  function parseQuotedStringWithApacheEscapes(s) {
    expectSkip('"');
    let result = "";
    for (;;) {
      let escapeIndex = logData.indexOf("\\", stringIndex);
      let endIndex = logData.indexOf('"', stringIndex);
      if (endIndex === -1) {
        stringIndex = logData.length;
        break;
      }
      if (escapeIndex === -1 || endIndex < escapeIndex) {
        result += logData.slice(stringIndex, endIndex);
        stringIndex = endIndex + 1;
        break;
      }
      if (nextLineIndex < endIndex) {
        // Found EOL before '"'.
        valid = false;
        return "";
      }
      result += logData.slice(stringIndex, escapeIndex);
      let escapedCharacter = logData[escapeIndex + 1];
      if (escapedCharacter === "t") {
        escapedCharacter = "\t";
      }
      result += escapedCharacter;
      stringIndex = escapeIndex + 2;
    }
    return result;
  }

  let batchEntryIndex = 0;
  // TODO(strager): Make this parser robust in case components are missing.
  while (stringIndex < logData.length) {
    nextLineIndex = logData.indexOf("\n", stringIndex);
    if (nextLineIndex === -1) {
      nextLineIndex = logData.length;
    } else {
      nextLineIndex += 1;
    }
    valid = true;

    let entry = entries[batchEntryIndex];

    // Example:
    // c.quick-lint-js.com 114.119.132.159 - - [25/Aug/2023:03:05:22 +0000] "GET /builds/8ce11369a188603c3b8f95305a25752009029eae/?C=M%3BO%3DA HTTP/1.1" 200 643 "https://c.quick-lint-js.com/builds/8ce11369a188603c3b8f95305a25752009029eae/?C=D%3BO%3DA" "Mozilla/5.0 (Linux; Android 7.0;) AppleWebKit/537.36 (KHTML, like Gecko) Mobile Safari/537.36 (compatible; PetalBot;+https://webmaster.petalsearch.com/site/petalbot)"
    entry.serverName = parseUntilAndSkip(" ");
    entry.remoteHostName = parseUntilAndSkip(" ");
    entry.remoteLogname = parseUntilAndSkip(" ");
    entry.remoteUser = parseUntilAndSkip(" ");
    expectSkip("[");
    entry.requestTimestamp = parseApacheTimestamp(parseUntil("]"));
    expectSkip("]");
    expectSkip(" ");

    let requestLine = parseQuotedStringWithApacheEscapes();
    let requestLineParts = requestLine.split(" ");
    if (requestLineParts.length === 3) {
      [entry.httpMethod, entry.uri, entry.httpVersion] = requestLineParts;
    } else {
      valid = false;
    }

    expectSkip(" ");
    entry.finalStatus = parseInt(parseUntilAndSkip(" "), 10);
    // TODO(strager): Parse '-' as 0.
    entry.responseBodySize = parseInt(parseUntilAndSkip(" "), 10);
    entry["Apache_Referer"] = parseQuotedStringWithApacheEscapes();
    expectSkip(" ");
    entry["Apache_User-Agent"] = parseQuotedStringWithApacheEscapes();
    expectSkip("\n");

    if (stringIndex !== nextLineIndex) {
      valid = false;
    }
    if (valid) {
      batchEntryIndex += 1;
      if (batchEntryIndex === entries.length) {
        onLogEntries(entries);
        batchEntryIndex = 0;
      }
    }

    stringIndex = nextLineIndex;
  }

  if (batchEntryIndex > 0) {
    onLogEntries(entries.slice(0, batchEntryIndex));
  }
}

// A Node.js stream.Writable which stores written data into memory. The final
// string can be accessed via the data property.
//
// Only supports Buffer writes.
class ToStringStream extends stream.Writable {
  #chunks = [];

  get data() {
    let decoder = new TextDecoder("utf-8", { fatal: true });
    let pieces = [];
    let options = { stream: true };
    for (let chunk of this.#chunks) {
      pieces.push(decoder.decode(chunk, options));
    }
    pieces.push(decoder.decode());
    return pieces.join("");
  }

  _write(chunk, _encoding, callback) {
    if (!(chunk instanceof Buffer)) {
      throw new TypeError("ToStringStream only works with Buffer");
    }
    this.#chunks.push(chunk);
    callback(null);
  }
}

async function readPossiblyGZippedFileAsTextAsync(path) {
  let fileHandle = await fs.promises.open(path, "r");
  try {
    let magic = new Uint8Array(2);
    let readResult = await fileHandle.read(magic, 0, 2, 0);
    let isGzipped =
      readResult.bytesRead === 2 && magic[0] === 0x1f && magic[1] === 0x8b;

    if (isGzipped) {
      let gunzipStream = zlib.createGunzip();
      let fileStream = fileHandle.createReadStream({
        autoClose: false,
        start: 0,
        encoding: null,
      });
      let stringStream = new ToStringStream();
      await pipe(fileStream, gunzipStream, stringStream);
      return stringStream.data;
    } else {
      return fileHandle.readFile({ encoding: "utf-8" });
    }
  } finally {
    await fileHandle.close();
  }
}

// Parse a timestamp emitted by Apache LogFormat's %t. For example:
// "18/Sep/2011:19:18:28 -0400"
//
// Returns null on failure.
export function parseApacheTimestamp(s) {
  let match = s.match(
    /^(\d{2})\/(...)\/(\d{4}):(\d{2}):(\d{2}):(\d{2}) ([-+])(\d{2})(\d{2})$/
  );
  if (match === null) {
    return null;
  }
  let dayOfMonth = parseInt(match[1], 10);
  let monthIndex = apacheMonthNames.indexOf(match[2]);
  let year = parseInt(match[3], 10);
  let hour = parseInt(match[4], 10);
  let minute = parseInt(match[5], 10);
  let second = parseInt(match[6], 10);
  let timeZoneOffsetIsPositive = match[7] === "+";
  let timeZoneOffsetHours = parseInt(match[8], 10);
  let timeZoneOffsetMinutes = parseInt(match[9], 10);

  let date = Date.UTC(year, monthIndex, dayOfMonth, hour, minute, second);
  if (!(timeZoneOffsetHours === 0 && timeZoneOffsetMinutes === 0)) {
    let timeZoneOffsetMillisecondsUnsigned =
      timeZoneOffsetHours * 60 * 60 * 1000 + timeZoneOffsetMinutes * 60 * 1000;
    date +=
      timeZoneOffsetMillisecondsUnsigned * (timeZoneOffsetIsPositive ? -1 : 1);
  }
  return date;
}

// prettier-ignore
let apacheMonthNames = [
  "Jan", "Feb", "Mar", "Apr",
  "May", "Jun", "Jul", "Aug",
  "Sep", "Oct", "Nov", "Dec",
];

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
