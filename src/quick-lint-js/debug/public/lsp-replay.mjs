// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

export class LSPReplayer {
  // Sentinel object indicating that a document has unknowable contents.
  static UNKNOWN_DOCUMENT_TEXT = Symbol("UNKNOWN_DOCUMENT_TEXT");

  // Sentinel object indicating that a document has an unknowable language ID.
  static UNKNOWN_DOCUMENT_LANGUAGE_ID = Symbol("UNKNOWN_DOCUMENT_LANGUAGE_ID");

  _messages = [];
  _documents;

  // Returns the index of the added message.
  appendClientToServerMessage(message) {
    let index = this._messages.length;
    this._messages.push(message);
    return index;
  }

  getOpenedDocumentsBeforeMessageIndex(messageIndex) {
    if (messageIndex < 0 || messageIndex > this._messages.length) {
      throw new RangeError(`invalid message index: ${messageIndex}`);
    }

    this._documents = [];
    for (let i = 0; i < messageIndex; ++i) {
      let { method, params } = this._messages[i];
      switch (method) {
        case "textDocument/didOpen": {
          let textDocument = params.textDocument;
          this._documents.push({
            uri: textDocument.uri,
            version: textDocument.version,
            languageID: textDocument.languageId,
            text: textDocument.text,
          });
          break;
        }
        case "textDocument/didClose": {
          let index = this._findDocumentIndex(params.textDocument.uri);
          if (index !== null) {
            this._documents.splice(index, 1);
          }
          break;
        }
        case "textDocument/didChange": {
          let textDocument = params.textDocument;
          let doc = this._findDocument(textDocument.uri);
          if (doc === null) {
            doc = {
              uri: textDocument.uri,
              version: textDocument.version,
              languageID: LSPReplayer.UNKNOWN_DOCUMENT_LANGUAGE_ID,
              text: LSPReplayer.UNKNOWN_DOCUMENT_TEXT,
            };
            this._documents.push(doc);
          }
          doc.version = textDocument.version;
          for (let change of params.contentChanges) {
            doc.text = updateLSPDocumentText(doc.text, change);
          }
          break;
        }
      }
    }
    return this._documents;
  }

  // Returns null if no such document is open.
  _findDocument(uri) {
    let docIndex = this._findDocumentIndex(uri);
    return docIndex === null ? null : this._documents[docIndex];
  }

  // Returns null if no such document is open.
  _findDocumentIndex(uri) {
    for (let i = 0; i < this._documents.length; ++i) {
      if (this._documents[i].uri === uri) {
        return i;
      }
    }
    return null;
  }
}

export function updateLSPDocumentText(text, contentChange) {
  if (typeof contentChange.range === "object") {
    if (text === LSPReplayer.UNKNOWN_DOCUMENT_TEXT) {
      return text;
    }
    let { start, end } = contentChange.range;
    let startIndex = indexFromLSPLocation(text, start.line, start.character);
    let endIndex = indexFromLSPLocation(text, end.line, end.character);
    if (startIndex === null || endIndex === null) {
      return LSPReplayer.UNKNOWN_DOCUMENT_TEXT;
    }
    return (
      text.substr(0, startIndex) + contentChange.text + text.substr(endIndex)
    );
  } else {
    return contentChange.text;
  }
}

export function indexFromLSPLocation(text, lineIndex, utf16CodeUnitIndex) {
  if (lineIndex < 0 || utf16CodeUnitIndex < 0) {
    return null;
  }

  let lineStartRE = /\r\n|\r|\n/g;
  for (
    let currentLineIndex = 0;
    currentLineIndex < lineIndex;
    ++currentLineIndex
  ) {
    let match = lineStartRE.exec(text);
    if (match === null) {
      return text.length;
    }
    // The next call to lineStartRE.exec will search from the start of this
    // line.
  }

  let lineStartIndex = lineStartRE.lastIndex;
  let match = lineStartRE.exec(text);
  let lineEndIndex = match === null ? text.length : match.index;

  return Math.min(lineStartIndex + utf16CodeUnitIndex, lineEndIndex);
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
