// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import { TraceReaderError, parseEvent } from "./trace-generated.mjs";

export const TraceEventType = {
  INIT: 1,
  VSCODE_DOCUMENT_OPENED: 2,
  VSCODE_DOCUMENT_CLOSED: 3,
  VSCODE_DOCUMENT_CHANGED: 4,
  VSCODE_DOCUMENT_SYNC: 5,
  LSP_CLIENT_TO_SERVER_MESSAGE: 6,
  VECTOR_MAX_SIZE_HISTOGRAM_BY_OWNER: 7,
  PROCESS_ID: 8,
  LSP_DOCUMENTS: 9,
};

export const TraceLSPDocumentType = {
  UNKNOWN: 0,
  CONFIG: 1,
  LINTABLE: 2,
};

// Reads quick-lint-js trace streams as documented in docs/TRACING.md.
export class TraceReader {
  hasHeader = false;
  error = null;
  threadID = null;
  _events = [];

  _queue = new Uint8Array();

  /**
   * @param {Uint8Array} bytes
   * @param {number} offset
   */
  appendBytes(bytes, offset = 0) {
    if (!(bytes instanceof Uint8Array)) {
      throw new TypeError("appendBytes expects a Uint8Array");
    }
    if (this.error !== null) {
      return;
    }
    this._queueBytes(bytes, offset);

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

  /**
   * @param {Uint8Array} bytes
   */
  _queueBytes(bytes, offset) {
    if (this._queue.byteLength > 0) {
      let newQueue = new Uint8Array(
        this._queue.byteLength + bytes.byteLength - offset
      );
      newQueue.set(this._queue, 0);
      newQueue.set(bytes.subarray(offset), this._queue.byteLength);
      this._queue = newQueue;
    } else {
      this._queue = bytes.subarray(offset);
    }
  }

  _parseOne(r) {
    if (this.hasHeader) {
      this._event(parseEvent(r));
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

  // Parse a u64 then calls the given function that many times.
  sizedArray(parseItem) {
    let count = this.u64BigInt();
    let items = [];
    for (let i = 0n; i < count; ++i) {
      items.push(parseItem());
    }
    return items;
  }
}

export { TraceReaderError };
export {
  TraceReaderUnknownEventType,
  TraceReaderInvalidLSPDocumentType,
} from "./trace-generated.mjs";

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
