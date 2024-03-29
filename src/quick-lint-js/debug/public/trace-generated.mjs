// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// Code generated by tools/generate-trace-sources.cpp. DO NOT EDIT.
// source: src/quick-lint-js/logging/trace-types.h

export class TraceReaderError extends Error {}

export class TraceReaderUnknownEventType extends TraceReaderError {
  constructor() {
    super("unrecognized event type");
  }
}

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

class TraceVSCodeDocumentPosition {
  line;
  character;

  static parse(r) {
    let result = {}; // TODO(strager): new TraceVSCodeDocumentPosition()
    result.line = r.u64BigInt();
    result.character = r.u64BigInt();
    return result;
  }
}

class TraceVSCodeDocumentRange {
  start;
  end;

  static parse(r) {
    let result = {}; // TODO(strager): new TraceVSCodeDocumentRange()
    result.start = TraceVSCodeDocumentPosition.parse(r);
    result.end = TraceVSCodeDocumentPosition.parse(r);
    return result;
  }
}

class TraceVSCodeDocumentChange {
  range;
  rangeOffset;
  rangeLength;
  text;

  static parse(r) {
    let result = {}; // TODO(strager): new TraceVSCodeDocumentChange()
    result.range = TraceVSCodeDocumentRange.parse(r);
    result.rangeOffset = r.u64BigInt();
    result.rangeLength = r.u64BigInt();
    result.text = r.utf16String();
    return result;
  }
}

class TraceVectorMaxSizeHistogramEntry {
  maxSize;
  count;

  static parse(r) {
    let result = {}; // TODO(strager): new TraceVectorMaxSizeHistogramEntry()
    result.maxSize = r.u64BigInt();
    result.count = r.u64BigInt();
    return result;
  }
}

class TraceVectorMaxSizeHistogramByOwnerEntry {
  owner;
  maxSizeEntries;

  static parse(r) {
    let result = {}; // TODO(strager): new TraceVectorMaxSizeHistogramByOwnerEntry()
    result.owner = r.utf8ZString();
    // prettier-ignore
    result.maxSizeEntries = r.sizedArray(() => TraceVectorMaxSizeHistogramEntry.parse(r));
    return result;
  }
}

export class TraceReaderInvalidLSPDocumentType extends TraceReaderError {
  constructor() {
    super("invalid lsp_document_type");
  }
}

export const TraceLSPDocumentType = {
  UNKNOWN: 0,
  CONFIG: 1,
  LINTABLE: 2,

  parse(r) {
    let value = r.u8();
    switch (value) {
      case TraceLSPDocumentType.UNKNOWN:
      case TraceLSPDocumentType.CONFIG:
      case TraceLSPDocumentType.LINTABLE:
        return value;
      default:
        throw new TraceReaderInvalidLSPDocumentType();
    }
  },
};

class TraceLSPDocumentState {
  type;
  uri;
  text;
  languageID;

  static parse(r) {
    let result = {}; // TODO(strager): new TraceLSPDocumentState()
    result.type = TraceLSPDocumentType.parse(r);
    result.uri = r.utf8String();
    result.text = r.utf8String();
    result.languageID = r.utf8String();
    return result;
  }
}

export function parseEvent(r) {
  let timestamp = r.u64BigInt();
  let eventType = r.u8();
  switch (eventType) {
    case TraceEventType.INIT:
      return {
        timestamp: timestamp,
        eventType: eventType,
        version: r.utf8ZString(),
      };

    case TraceEventType.VSCODE_DOCUMENT_OPENED:
      return {
        timestamp: timestamp,
        eventType: eventType,
        documentID: r.u64BigInt(),
        uri: r.utf16String(),
        languageID: r.utf16String(),
        content: r.utf16String(),
      };

    case TraceEventType.VSCODE_DOCUMENT_CLOSED:
      return {
        timestamp: timestamp,
        eventType: eventType,
        documentID: r.u64BigInt(),
        uri: r.utf16String(),
        languageID: r.utf16String(),
      };

    case TraceEventType.VSCODE_DOCUMENT_CHANGED:
      return {
        timestamp: timestamp,
        eventType: eventType,
        documentID: r.u64BigInt(),
        // prettier-ignore
        changes: r.sizedArray(() => TraceVSCodeDocumentChange.parse(r)),
      };

    case TraceEventType.VSCODE_DOCUMENT_SYNC:
      return {
        timestamp: timestamp,
        eventType: eventType,
        documentID: r.u64BigInt(),
        uri: r.utf16String(),
        languageID: r.utf16String(),
        content: r.utf16String(),
      };

    case TraceEventType.LSP_CLIENT_TO_SERVER_MESSAGE:
      return {
        timestamp: timestamp,
        eventType: eventType,
        body: r.utf8String(),
      };

    case TraceEventType.VECTOR_MAX_SIZE_HISTOGRAM_BY_OWNER:
      return {
        timestamp: timestamp,
        eventType: eventType,
        // prettier-ignore
        entries: r.sizedArray(() => TraceVectorMaxSizeHistogramByOwnerEntry.parse(r)),
      };

    case TraceEventType.PROCESS_ID:
      return {
        timestamp: timestamp,
        eventType: eventType,
        processID: r.u64BigInt(),
      };

    case TraceEventType.LSP_DOCUMENTS:
      return {
        timestamp: timestamp,
        eventType: eventType,
        // prettier-ignore
        documents: r.sizedArray(() => TraceLSPDocumentState.parse(r)),
      };

    default:
      throw new TraceReaderUnknownEventType();
  }
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
