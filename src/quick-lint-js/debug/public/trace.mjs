// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

export const TraceEventType = {
  INIT: 1,
  VSCODE_DOCUMENT_OPENED: 2,
  VSCODE_DOCUMENT_CLOSED: 3,
  VSCODE_DOCUMENT_CHANGED: 4,
  VSCODE_DOCUMENT_SYNC: 5,
  LSP_CLIENT_TO_SERVER_MESSAGE: 6,
  VECTOR_MAX_SIZE_HISTOGRAM_BY_OWNER: 7,
  PROCESS_ID: 8,
};

// Reads quick-lint-js trace streams as documented in docs/TRACING.md.
export class TraceReader {
  hasHeader = false;
  error = null;
  threadID = null;
  _events = [];

  _queue = new Uint8Array();

  appendBytes(buffer, offset = 0) {
    if (this.error !== null) {
      return;
    }
    this._queueBytes(buffer, offset);

    let r = new TraceByteReader(this._queue.buffer, this._queue.byteOffset);
    let committedOffset = 0;
    try {
      for (;;) {
        this._parseOne(r);
        committedOffset = r._offset - this._queue.byteOffset;
      }
    } catch (e) {
      if (e instanceof RangeError) {
        // Not enough data available. Stop parsing.
      } else if (e instanceof TraceReaderError) {
        this._error(e);
        return;
      } else {
        throw e;
      }
    }

    this._queue = this._queue.slice(committedOffset);
  }

  pullNewEvents() {
    let events = this._events;
    this._events = [];
    return events;
  }

  _queueBytes(buffer, offset) {
    if (this._queue.byteLength > 0) {
      let newQueue = new Uint8Array(
        this._queue.byteLength + buffer.byteLength - offset
      );
      newQueue.set(this._queue, 0);
      newQueue.set(new Uint8Array(buffer, offset), this._queue.byteLength);
      this._queue = newQueue;
    } else {
      this._queue = new Uint8Array(buffer, offset);
    }
  }

  _parseOne(r) {
    if (this.hasHeader) {
      let timestamp = r.u64BigInt();
      let eventType = r.u8();

      switch (eventType) {
        case TraceEventType.INIT: {
          let version = r.utf8ZString();
          this._event({ timestamp, eventType, version });
          return;
        }

        case TraceEventType.VSCODE_DOCUMENT_OPENED: {
          let documentID = r.u64BigInt();
          let uri = r.utf16String();
          let languageID = r.utf16String();
          let content = r.utf16String();
          this._event({
            timestamp,
            eventType,
            documentID,
            uri,
            languageID,
            content,
          });
          return;
        }

        case TraceEventType.VSCODE_DOCUMENT_CLOSED: {
          let documentID = r.u64BigInt();
          let uri = r.utf16String();
          let languageID = r.utf16String();
          this._event({
            timestamp,
            eventType,
            documentID,
            uri,
            languageID,
          });
          return;
        }

        case TraceEventType.VSCODE_DOCUMENT_CHANGED: {
          let documentID = r.u64BigInt();
          let changes = [];
          let changeCount = r.u64BigInt();
          for (let i = 0n; i < changeCount; ++i) {
            changes.push({
              range: {
                start: {
                  line: r.u64BigInt(),
                  character: r.u64BigInt(),
                },
                end: {
                  line: r.u64BigInt(),
                  character: r.u64BigInt(),
                },
              },
              rangeOffset: r.u64BigInt(),
              rangeLength: r.u64BigInt(),
              text: r.utf16String(),
            });
          }
          this._event({
            timestamp,
            eventType,
            documentID,
            changes,
          });
          return;
        }

        case TraceEventType.VSCODE_DOCUMENT_SYNC: {
          let documentID = r.u64BigInt();
          let uri = r.utf16String();
          let languageID = r.utf16String();
          let content = r.utf16String();
          this._event({
            timestamp,
            eventType,
            documentID,
            uri,
            languageID,
            content,
          });
          return;
        }

        case TraceEventType.LSP_CLIENT_TO_SERVER_MESSAGE: {
          let body = r.utf8String();
          this._event({ timestamp, eventType, body });
          return;
        }

        case TraceEventType.VECTOR_MAX_SIZE_HISTOGRAM_BY_OWNER: {
          let entryCount = r.u64BigInt();
          let entries = [];
          for (let i = 0n; i < entryCount; ++i) {
            let owner = r.utf8ZString();
            let maxSizeEntryCount = r.u64BigInt();
            let maxSizeEntries = [];
            for (let j = 0n; j < maxSizeEntryCount; ++j) {
              maxSizeEntries.push({
                maxSize: r.u64BigInt(),
                count: r.u64BigInt(),
              });
            }
            entries.push({ owner, maxSizeEntries });
          }
          this._event({ timestamp, eventType, entries });
          return;
        }

        case TraceEventType.PROCESS_ID: {
          let processID = r.u64BigInt();
          this._event({ timestamp, eventType, processID });
          return;
        }

        default:
          throw new TraceReaderUnknownEventType();
      }
    } else {
      let magic = r.u32();
      let uuid0 = r.u32();
      let uuid1 = r.u32();
      let uuid2 = r.u32();
      let uuid3 = r.u32();
      let threadID = r.u64BigInt();
      let compressionMode = r.u8();

      this.hasHeader = true;
      this.threadID = threadID;

      if (magic !== 0xc1_fc_1f_c1) {
        throw new TraceReaderInvalidMagic();
      }
      if (
        !(
          uuid0 === 0x63_69_75_71 &&
          uuid1 === 0x49_5f_2d_6b &&
          uuid2 === 0x69_6c_b9_3e &&
          uuid3 === 0x73_6a_74_6e
        )
      ) {
        throw new TraceReaderInvalidUUID();
      }
      if (compressionMode !== 0x00) {
        throw new TraceReaderInvalidCompressionMode();
      }

      return;
    }
  }

  // This is a separate function for easy breakpointing.
  _error(error) {
    this.error = error;
  }

  // This is a separate function for easy breakpointing.
  _event(event) {
    this._events.push(event);
  }
}

class TraceByteReader {
  constructor(buffer, offset) {
    this._buffer = buffer;
    this._offset = offset;
    this._dataView = new DataView(buffer);
  }

  // Returns a Number.
  u8() {
    let result = this._dataView.getUint8(this._offset);
    this._offset += 1;
    return result;
  }

  // Returns a Number.
  u32() {
    let result = this._dataView.getUint32(this._offset, /*littleEndian=*/ true);
    this._offset += 4;
    return result;
  }

  // Returns a BigInt.
  u64BigInt() {
    let result = this._dataView.getBigUint64(
      this._offset,
      /*littleEndian=*/ true
    );
    this._offset += 8;
    return result;
  }

  // Returns a String.
  utf8ZString() {
    let data = new Uint8Array(this._buffer, this._offset);
    let terminatorIndex = data.indexOf(0x00);
    if (terminatorIndex === -1) {
      return 0;
    }
    let result = utf8Decoder.decode(data.subarray(0, terminatorIndex));
    this._offset += terminatorIndex + 1;
    return result;
  }

  // Returns a String.
  utf8String() {
    let length = this.u64BigInt();
    if (length > BigInt(Number.MAX_SAFE_INTEGER)) {
      throw new TraceReaderSizeTooLarge();
    }
    length = Number(length);
    let result = utf8Decoder.decode(
      new Uint8Array(this._buffer, this._offset, length)
    );
    this._offset += length;
    return result;
  }

  // Returns a String.
  utf16String() {
    let length = this.u64BigInt();
    if (length > BigInt(Number.MAX_SAFE_INTEGER)) {
      throw new TraceReaderSizeTooLarge();
    }
    length = Number(length);
    let result = utf16Decoder.decode(
      new Uint8Array(this._buffer, this._offset, length * 2)
    );
    this._offset += length * 2;
    return result;
  }
}

export class TraceReaderError extends Error {}

export class TraceReaderInvalidMagic extends TraceReaderError {
  constructor() {
    super("invalid magic");
  }
}

export class TraceReaderInvalidUUID extends TraceReaderError {
  constructor() {
    super("invalid magic UUID");
  }
}

export class TraceReaderInvalidCompressionMode extends TraceReaderError {
  constructor() {
    super("invalid compression mode");
  }
}

export class TraceReaderUnknownEventType extends TraceReaderError {
  constructor() {
    super("unrecognized event type");
  }
}

export class TraceReaderSizeTooLarge extends TraceReaderError {
  constructor() {
    super("size is too large");
  }
}

let utf16Decoder = new TextDecoder("utf-16le", { fatal: false });
let utf8Decoder = new TextDecoder("utf-8", { fatal: true });

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
