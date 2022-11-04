// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "assert";
import {
  TraceEventType,
  TraceReader,
  TraceReaderInvalidCompressionMode,
  TraceReaderInvalidMagic,
  TraceReaderInvalidUUID,
  TraceReaderSizeTooLarge,
  TraceReaderUnknownEventType,
} from "../public/trace.mjs";

// prettier-ignore
let examplePacketHeader = new Uint8Array([
  // CTF magic
  0xc1, 0x1f, 0xfc, 0xc1,
  // quick-lint-js metadata UUID
  0x71, 0x75, 0x69, 0x63, 0x6b, 0x2d, 0x5f, 0x49,
  0x3e, 0xb9, 0x6c, 0x69, 0x6e, 0x74, 0x6a, 0x73,
  // Thread ID
  0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  // Compression mode
  0x00,
]).buffer;

describe("trace", () => {
  it("read full header", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    expect(reader.hasHeader).toBeTrue();
    expect(reader.error).toBeNull();
    expect(reader.threadID).toEqual(0x1234n);
    expect(reader.pullNewEvents()).toEqual([]);
  });

  it("read header in two parts", () => {
    for (
      let firstChunkSize = 1;
      firstChunkSize < examplePacketHeader.byteLength - 1;
      ++firstChunkSize
    ) {
      let secondChunkSize = examplePacketHeader.byteLength - firstChunkSize;

      let reader = new TraceReader();
      reader.appendBytes(examplePacketHeader.slice(0, firstChunkSize));
      reader.appendBytes(examplePacketHeader.slice(firstChunkSize));

      expect(reader.hasHeader).toBeTrue();
      expect(reader.error).toBeNull();
      expect(reader.threadID).toEqual(0x1234n);
      expect(reader.pullNewEvents()).toEqual([]);
    }
  });

  it("read partial header", () => {
    let reader = new TraceReader();
    // Cut the header off in the middle of the thread ID.
    reader.appendBytes(examplePacketHeader.slice(0, 4 + 16 + 3));
    expect(reader.hasHeader).toBeFalse();
    expect(reader.error).toBeNull();
    expect(reader.threadID).toBeNull();
    expect(reader.pullNewEvents()).toEqual([]);
  });

  it("read full header from bigger buffer with offset", () => {
    let header = new Uint8Array(examplePacketHeader.byteLength + 4);
    header[0] = 0xfe;
    header[1] = 0xef;
    header[2] = 0xc0;
    header[3] = 0x0c;
    header.set(new Uint8Array(examplePacketHeader), 4);

    let reader = new TraceReader();
    reader.appendBytes(header.buffer, 4);
    expect(reader.hasHeader).toBeTrue();
    expect(reader.error).toBeNull();
    expect(reader.threadID).toEqual(0x1234n);
    expect(reader.pullNewEvents()).toEqual([]);
  });

  it("invalid magic reports error", () => {
    let header = new Uint8Array(new Uint8Array(examplePacketHeader));
    header[0] = 0xc0;
    header[3] = 0xc0;

    let reader = new TraceReader();
    reader.appendBytes(header);
    expect(reader.hasHeader).toBeTrue();
    expect(reader.error).toBeInstanceOf(TraceReaderInvalidMagic);
    expect(reader.threadID).toEqual(0x1234n);
    expect(reader.pullNewEvents()).toEqual([]);
  });

  it("invalid UUID reports error", () => {
    let header = new Uint8Array(new Uint8Array(examplePacketHeader));
    header[7] = 0xff;

    let reader = new TraceReader();
    reader.appendBytes(header);
    expect(reader.hasHeader).toBeTrue();
    expect(reader.error).toBeInstanceOf(TraceReaderInvalidUUID);
    expect(reader.threadID).toEqual(0x1234n);
    expect(reader.pullNewEvents()).toEqual([]);
  });

  it("invalid compression mode reports error", () => {
    let header = new Uint8Array(new Uint8Array(examplePacketHeader));
    header[4 + 16 + 8] = 0xfe;

    let reader = new TraceReader();
    reader.appendBytes(header);
    expect(reader.hasHeader).toBeTrue();
    expect(reader.error).toBeInstanceOf(TraceReaderInvalidCompressionMode);
    expect(reader.threadID).toEqual(0x1234n);
    expect(reader.pullNewEvents()).toEqual([]);
  });

  it("init event", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID
      0x01,
      // Version
      ord('1'), ord('.'), ord('0'), ord('.'), ord('0'), ord('\0'),
    ]).buffer);
    expect(reader.error).toBeNull();
    expect(reader.pullNewEvents()).toEqual([
      { timestamp: 0x5678n, eventType: TraceEventType.INIT, version: "1.0.0" },
    ]);
  });

  it("vs code document opened event", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID
      0x02,
      // Document ID
      0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // URI
      0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      ord('t'), 0, ord('e'), 0, ord('s'), 0, ord('t'), 0, ord('.'), 0, ord('j'), 0, ord('s'), 0,
      // Language ID
      0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      ord('j'), 0, ord('s'), 0,
      // Content
      0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      ord('h'), 0, ord('i'), 0,
    ]).buffer);
    expect(reader.error).toBeNull();
    expect(reader.pullNewEvents()).toEqual([
      {
        timestamp: 0x5678n,
        eventType: TraceEventType.VSCODE_DOCUMENT_OPENED,
        documentID: 0x1234n,
        uri: "test.js",
        languageID: "js",
        content: "hi",
      },
    ]);
  });

  it("vs code document closed event", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID
      0x03,
      // Document ID
      0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // URI
      0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      ord('t'), 0, ord('e'), 0, ord('s'), 0, ord('t'), 0, ord('.'), 0, ord('j'), 0, ord('s'), 0,
      // Language ID
      0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      ord('j'), 0, ord('s'), 0,
    ]).buffer);
    expect(reader.error).toBeNull();
    expect(reader.pullNewEvents()).toEqual([
      {
        timestamp: 0x5678n,
        eventType: TraceEventType.VSCODE_DOCUMENT_CLOSED,
        documentID: 0x1234n,
        uri: "test.js",
        languageID: "js",
      },
    ]);
  });

  it("vs code document changed event", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID
      0x04,
      // Document ID
      0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Change count
      0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

      // Change 0 range
      0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Start line
      0x22, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Start character
      0x33, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // End line
      0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // End character
      // Change 0 range offset
      0x55, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Change 0 range length
      0x66, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Change 0 text
      0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      ord('h'), 0, ord('i'), 0,

      // Change 1 range
      0xaa, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Start line
      0xbb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Start character
      0xcc, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // End line
      0xdd, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // End character
      // Change 1 range offset
      0xee, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Change 1 range length
      0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Change 1 text
      0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      ord('b'), 0, ord('y'), 0, ord('e'), 0,
    ]).buffer);
    expect(reader.error).toBeNull();
    expect(reader.pullNewEvents()).toEqual([
      {
        timestamp: 0x5678n,
        eventType: TraceEventType.VSCODE_DOCUMENT_CHANGED,
        documentID: 0x1234n,
        changes: [
          {
            range: {
              start: { line: 0x11n, character: 0x22n },
              end: { line: 0x33n, character: 0x44n },
            },
            rangeOffset: 0x55n,
            rangeLength: 0x66n,
            text: "hi",
          },
          {
            range: {
              start: { line: 0xaan, character: 0xbbn },
              end: { line: 0xccn, character: 0xddn },
            },
            rangeOffset: 0xeen,
            rangeLength: 0xffn,
            text: "bye",
          },
        ],
      },
    ]);
  });

  it("vs code document sync event", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID
      0x05,
      // Document ID
      0x34, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // URI
      0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      ord('t'), 0, ord('e'), 0, ord('s'), 0, ord('t'), 0, ord('.'), 0, ord('j'), 0, ord('s'), 0,
      // Language ID
      0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      ord('j'), 0, ord('s'), 0,
      // Content
      0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      ord('h'), 0, ord('i'), 0,
    ]).buffer);
    expect(reader.error).toBeNull();
    expect(reader.pullNewEvents()).toEqual([
      {
        timestamp: 0x5678n,
        eventType: TraceEventType.VSCODE_DOCUMENT_SYNC,
        documentID: 0x1234n,
        uri: "test.js",
        languageID: "js",
        content: "hi",
      },
    ]);
  });

  it("LSP client to server message", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID
      0x06,
      // Body
      2, 0, 0, 0, 0, 0, 0, 0,  // Size
      ord('{'), ord('}'),
    ]).buffer);
    expect(reader.error).toBeNull();
    expect(reader.pullNewEvents()).toEqual([
      {
        timestamp: 0x5678n,
        eventType: TraceEventType.LSP_CLIENT_TO_SERVER_MESSAGE,
        body: "{}",
      },
    ]);
  });

  it("read LSP client to server message in two parts", () => {
    // prettier-ignore
    let message = new Uint8Array([
      // Timestamp
      0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID
      0x06,
      // Body
      2, 0, 0, 0, 0, 0, 0, 0,  // Size
      ord('{'), ord('}'),
    ]).buffer;

    for (
      let firstChunkSize = 1;
      firstChunkSize < message.byteLength - 1;
      ++firstChunkSize
    ) {
      let secondChunkSize = message.byteLength - firstChunkSize;

      let reader = new TraceReader();
      reader.appendBytes(examplePacketHeader);
      reader.appendBytes(message.slice(0, firstChunkSize));
      reader.appendBytes(message.slice(firstChunkSize));

      expect(reader.error).toBeNull();
      expect(reader.pullNewEvents()).toEqual([
        {
          timestamp: 0x5678n,
          eventType: TraceEventType.LSP_CLIENT_TO_SERVER_MESSAGE,
          body: "{}",
        },
      ]);
    }
  });

  it("vector max size histogram by owner", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

      // Event ID
      0x07,

      // Entry count
      2, 0, 0, 0, 0, 0, 0, 0,

      // Entry 0 owner
      ord('o'), ord('1'), 0,
      // Entry 0 max size entries
      2, 0, 0, 0, 0, 0, 0, 0,  // Count
      0, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 0 max size
      4, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 0 count
      1, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 1 max size
      3, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 1 count

      // Entry 1 owner
      ord('o'), ord('2'), 0,
      // Entry 1 max size entries
      1, 0, 0, 0, 0, 0, 0, 0,  // Count
      3, 0, 0, 0, 0, 0, 0, 0,  // Max size entry 0 max size
      7, 0, 0, 0, 0, 0, 0, 0,
    ]).buffer);
    expect(reader.error).toBeNull();
    expect(reader.pullNewEvents()).toEqual([
      {
        timestamp: 0x5678n,
        eventType: TraceEventType.VECTOR_MAX_SIZE_HISTOGRAM_BY_OWNER,
        entries: [
          {
            owner: "o1",
            maxSizeEntries: [
              { maxSize: 0n, count: 4n },
              { maxSize: 1n, count: 3n },
            ],
          },
          {
            owner: "o2",
            maxSizeEntries: [{ maxSize: 3n, count: 7n }],
          },
        ],
      },
    ]);
  });

  it("process ID", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x78, 0x56, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

      // Event ID
      0x08,

      // Process ID
      0x23, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ]).buffer);
    expect(reader.error).toBeNull();
    expect(reader.pullNewEvents()).toEqual([
      {
        timestamp: 0x5678n,
        eventType: TraceEventType.PROCESS_ID,
        processID: 0x0123n,
      },
    ]);
  });

  it("many messages", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x11, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID (INIT)
      0x01,
      // Version
      ord('1'), ord('.'), ord('0'), ord('.'), ord('0'), ord('\0'),

      // Timestamp
      0x22, 0x22, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID (LSP_CLIENT_TO_SERVER_MESSAGE)
      0x06,
      // Body
      2, 0, 0, 0, 0, 0, 0, 0,  // Size
      ord('{'), ord('}'),

      // Timestamp
      0x33, 0x33, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID (INIT)
      0x01,
      // Version
      ord('1'), ord('.'), ord('0'), ord('.'), ord('0'), ord('\0'),
    ]).buffer);
    expect(reader.error).toBeNull();
    expect(reader.pullNewEvents()).toEqual([
      { timestamp: 0x1111n, eventType: TraceEventType.INIT, version: "1.0.0" },
      {
        timestamp: 0x2222n,
        eventType: TraceEventType.LSP_CLIENT_TO_SERVER_MESSAGE,
        body: "{}",
      },
      { timestamp: 0x3333n, eventType: TraceEventType.INIT, version: "1.0.0" },
    ]);
  });

  it("invalid header stops all message parsing", () => {
    let header = new Uint8Array(new Uint8Array(examplePacketHeader));
    header[0] = 0xc0;
    header[3] = 0xc0;

    let reader = new TraceReader();
    reader.appendBytes(header);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x11, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID (INIT)
      0x01,
      // Version
      ord('1'), ord('.'), ord('0'), ord('.'), ord('0'), ord('\0'),
    ]).buffer);

    expect(reader.error).toBeInstanceOf(TraceReaderInvalidMagic);
    expect(reader.pullNewEvents()).toEqual([]);
  });

  it("huge string size stops all message parsing", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x22, 0x22, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID (LSP_CLIENT_TO_SERVER_MESSAGE)
      0x06,
      // Body
      0xff, 0, 0, 0, 0, 0, 0, 0xff,  // Size (huge)
    ]).buffer);

    expect(reader.error).toBeInstanceOf(TraceReaderSizeTooLarge);
    expect(reader.pullNewEvents()).toEqual([]);

    let data = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8]).buffer;
    for (let i = 0; i < 100; ++i) {
      reader.appendBytes(data);
    }

    expect(reader.error).toBeInstanceOf(TraceReaderSizeTooLarge);
    expect(reader.pullNewEvents()).toEqual([]);
  });

  it("invalid event stops all message parsing", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);
    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x11, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID (garbage)
      0xfe,

      // Timestamp
      0x11, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID (INIT)
      0x01,
      // Version
      ord('1'), ord('.'), ord('0'), ord('.'), ord('0'), ord('\0'),
    ]).buffer);

    expect(reader.error).toBeInstanceOf(TraceReaderUnknownEventType);
    expect(reader.pullNewEvents()).toEqual([]);
  });

  it("pulling events removes from queue", () => {
    let reader = new TraceReader();
    reader.appendBytes(examplePacketHeader);

    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID
      0x01,
      // Version
      ord('1'), ord('\0'),
    ]).buffer);

    let events0 = reader.pullNewEvents();
    expect(events0.length).toEqual(1);

    let events1 = reader.pullNewEvents();
    expect(events1).toEqual([]);

    // prettier-ignore
    reader.appendBytes(new Uint8Array([
      // Timestamp
      0x22, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      // Event ID
      0x01,
      // Version
      ord('2'), ord('\0'),
    ]).buffer);

    let events2 = reader.pullNewEvents();
    expect(events2.length).toEqual(1);

    let events3 = reader.pullNewEvents();
    expect(events3).toEqual([]);
  });
});

function ord(character) {
  return character.charCodeAt(0);
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
