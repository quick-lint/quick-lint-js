// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import { TraceReader, TraceEventType } from "./trace.mjs";

let DEBUG_LSP_LOG = false;

class VectorProfileView {
  constructor(element) {
    this.element = element;
    this.maxSizeHistogramElementByOwner = new Map();
  }

  updateMaxSizeHistogram({ owner, maxSizeEntries }) {
    let el = this.maxSizeHistogramElementByOwner.get(owner);
    if (el === undefined) {
      el = document.createElement("div");
      el.classList.add("histogram");
      el.classList.add("max-size-histogram");

      let titleEl = document.createElement("h3");
      titleEl.textContent = owner;
      el.appendChild(titleEl);

      let tableEl = document.createElement("table");
      tableEl.appendChild(document.createElement("tbody"));
      el.appendChild(tableEl);

      this.element.appendChild(el);
      this.maxSizeHistogramElementByOwner.set(owner, el);
    }

    let tableBodyEl = el.querySelector("table tbody");
    let totalCount = maxSizeEntries.reduce(
      (acc, entry) => entry.count + acc,
      0
    );
    let maxCount = maxSizeEntries.reduce(
      (acc, entry) => Math.max(entry.count, acc),
      0
    );

    let lastMaxSize = -1;
    for (let { maxSize, count } of maxSizeEntries) {
      for (let i = lastMaxSize + 1; i < maxSize; ++i) {
        addRow(i, 0);
      }
      addRow(maxSize, count);
      lastMaxSize = maxSize;
    }

    function addRow(maxSize, count) {
      let rowEl = tableBodyEl.children[maxSize];
      if (rowEl === undefined) {
        rowEl = document.createElement("tr");
        let rowHeaderEl = document.createElement("th");
        rowHeaderEl.textContent = `${maxSize}`;
        rowEl.appendChild(rowHeaderEl);
        rowEl.appendChild(document.createElement("td"));
        tableBodyEl.appendChild(rowEl);
      }
      let rowDataEl = rowEl.querySelector("td");
      rowDataEl.textContent = `${((count / totalCount) * 100).toFixed(1)}%`;
      rowDataEl.style.setProperty(
        "--histogram-percentage",
        `${(count / maxCount) * 100}%`
      );
    }
  }
}

let vectorProfileView = new VectorProfileView(
  document.getElementById("vector-profile-data")
);

class EventEmitter {
  _eventListeners = new Map();

  on(eventName, listener) {
    let listeners = this._eventListeners.get(eventName);
    if (listeners === undefined) {
      listeners = [];
      this._eventListeners.set(eventName, listeners);
    }
    listeners.push(listener);
  }

  emit(eventName, ...args) {
    let listeners = this._eventListeners.get(eventName);
    if (listeners !== undefined) {
      for (let listener of listeners) {
        listener(...args);
      }
    }
  }
}

class DebugServerSocket extends EventEmitter {
  constructor(webSocket) {
    super();

    this.webSocket = webSocket;
    this.traceReaders = new Map(); // Key is the thread index.

    this.webSocket.addEventListener("message", (event) => {
      this._onMessage(event);
    });
  }

  _onMessage(event) {
    let messageData = event.data;
    let threadIndex = new DataView(messageData).getBigUint64(
      0,
      /*littleEndian=*/ true
    );

    let reader = this.traceReaders.get(threadIndex);
    if (reader === undefined) {
      reader = new TraceReader();
      this.traceReaders.set(threadIndex, reader);
    }

    if (reader.error !== null) {
      // We already reported this error. Skip processing.
      return;
    }

    reader.appendBytes(messageData, 8);
    for (let event of reader.pullNewEvents()) {
      switch (event.eventType) {
        case TraceEventType.INIT:
          this.emit("initEvent", event);
          break;
        case TraceEventType.VSCODE_DOCUMENT_OPENED:
          this.emit("vscodeDocumentOpenedEvent", event);
          break;
        case TraceEventType.VSCODE_DOCUMENT_CLOSED:
          this.emit("vscodeDocumentClosedEvent", event);
          break;
        case TraceEventType.VSCODE_DOCUMENT_CHANGED:
          this.emit("vscodeDocumentChangedEvent", event);
          break;
        case TraceEventType.VSCODE_DOCUMENT_SYNC:
          this.emit("vscodeDocumentSyncEvent", event);
          break;
        case TraceEventType.LSP_CLIENT_TO_SERVER_MESSAGE:
          this.emit("lspClientToServerMessageEvent", event);
          break;
        case TraceEventType.VECTOR_MAX_SIZE_HISTOGRAM_BY_OWNER:
          // TODO(strager): Add 'Event' suffix to this name.
          this.emit("vectorMaxSizeHistogramByOwner", event);
          break;
        case TraceEventType.PROCESS_ID:
          this.emit("processIDEvent", event);
          break;
        default:
          this.emit("unknownTraceEvent", event);
          break;
      }
    }
    if (reader.error !== null) {
      this.emit("error", reader.error);
    }
  }

  static connectAsync() {
    return new Promise((resolve, reject) => {
      let url = new URL(window.location);
      url.pathname = "/api/trace";
      url.protocol = "ws:";
      url.hash = "";
      let webSocket = new WebSocket(url.toString());
      webSocket.binaryType = "arraybuffer";
      webSocket.addEventListener("open", (_event) => {
        resolve(new DebugServerSocket(webSocket));
      });
      webSocket.addEventListener("error", (error) => {
        reject(new Error(`failed to connect to WebSocket at ${url}`));
      });
    });
  }
}

class LSPLogDetailsView {
  constructor(rootElement) {
    this._rootElement = rootElement;
    this._paramsElement = rootElement.querySelector("#lsp-log-params");
  }

  setMessage(message) {
    this._paramsElement.replaceChildren();

    let hasParams = "params" in message;
    if (hasParams) {
      this._paramsElement.classList.add("lsp-params");
      let paramNames = Object.keys(message.params).sort();
      for (let paramName of paramNames) {
        this._paramsElement.appendChild(createElementWithText("dt", paramName));
        let paramValue = message.params[paramName];
        let paramValueText = JSON.stringify(paramValue, null, 2);
        this._paramsElement.appendChild(
          createElementWithText("dd", paramValueText)
        );
      }
    }
  }
}

class LSPLogView {
  constructor(rootElement) {
    this._rootElement = rootElement;
    this._dataElement = rootElement.querySelector("#lsp-log-data");
    this._detailsView = new LSPLogDetailsView(
      rootElement.querySelector("#lsp-log-details")
    );

    this._dataElement.addEventListener("click", (event) => {
      this._onClick(event);
    });

    this._elementToMessage = new Map();

    this._selectedMessageElement = null;
  }

  addClientToServerMessage(_timestamp, json) {
    let message = JSON.parse(json);

    let element = document.createElement("li");
    element.classList.add("lsp-message");
    element.classList.add("lsp-client-to-server");

    let hasID = "id" in message;
    let hasMethod = "method" in message;
    let hasError = "error" in message;
    let hasParams = "params" in message;
    element.classList.toggle("lsp-request", hasID && hasMethod);
    element.classList.toggle("lsp-response", hasID && !hasMethod);
    element.classList.toggle("lsp-notification", !hasID && hasMethod);
    element.classList.toggle("lsp-error", hasError);

    let summaryElement = document.createElement("summary");
    if (hasMethod) {
      summaryElement.appendChild(createElementWithText("span", message.method));
    }
    element.appendChild(summaryElement);

    this._dataElement.appendChild(element);

    this._elementToMessage.set(element, message);
  }

  _onClick(event) {
    let element = event.target;
    while (element !== document && element !== event.currentTarget) {
      let message = this._elementToMessage.get(element);
      if (message !== undefined) {
        this._onMessageClicked(element, message);
        break;
      }
      element = element.parentNode;
    }
  }

  _onMessageClicked(element, message) {
    if (this._selectedMessageElement !== null) {
      this._selectedMessageElement.classList.remove("selected");
    }
    element.classList.add("selected");
    this._selectedMessageElement = element;

    this._detailsView.setMessage(message);
  }
}

function createElementWithText(tagName, textContent) {
  let element = document.createElement(tagName);
  element.textContent = textContent;
  return element;
}

let lspLog = new LSPLogView(document.getElementById("lsp-log"));

class ServerInfoView {
  constructor(rootElement) {
    this._dlElement = rootElement.querySelector("dl");

    this._dlElement.appendChild(createElementWithText("dt", "Version"));
    this._versionElement = createElementWithText("dd", "???");
    this._dlElement.appendChild(this._versionElement);

    this._dlElement.appendChild(createElementWithText("dt", "Process ID"));
    this._processIDElement = createElementWithText("dd", "???");
    this._dlElement.appendChild(this._processIDElement);
  }

  setProcessID(value) {
    this._processIDElement.textContent = value;
  }

  setVersion(value) {
    this._versionElement.textContent = value;
  }
}

let serverInfo = new ServerInfoView(document.getElementById("server-info"));

DebugServerSocket.connectAsync().then((socket) => {
  socket.on("error", (error) => {
    console.error(error);
  });
  socket.on("initEvent", ({ version }) => {
    serverInfo.setVersion(version);
  });
  socket.on("processIDEvent", ({ processID }) => {
    serverInfo.setProcessID(processID);
  });
  socket.on("lspClientToServerMessageEvent", ({ timestamp, body }) => {
    lspLog.addClientToServerMessage(timestamp, body);
  });
  socket.on("vectorMaxSizeHistogramByOwner", ({ timestamp, entries }) => {
    for (let { owner, maxSizeEntries } of entries) {
      vectorProfileView.updateMaxSizeHistogram({
        owner,
        maxSizeEntries: maxSizeEntries.map((entry) => ({
          maxSize: Number(entry.maxSize),
          count: Number(entry.count),
        })),
      });
    }
  });
});

class TabBarView {
  constructor(tabBarElement, tabbedContainerElement) {
    this._tabBarElement = tabBarElement;
    this._tabbedContainerElement = tabbedContainerElement;

    window.addEventListener("hashchange", (_event) => {
      this._checkCurrentHash();
    });
    this._checkCurrentHash();
  }

  _checkCurrentHash() {
    let currentHash = window.location.hash;
    let currentID = currentHash.replace("#/", "");
    for (let aElement of this._tabBarElement.querySelectorAll("li > a")) {
      aElement.classList.toggle(
        "active",
        new URL(aElement.href).hash === currentHash
      );
    }
    for (let tabbedElement of this._tabbedContainerElement.querySelectorAll(
      ".tab"
    )) {
      tabbedElement.classList.toggle("active", tabbedElement.id === currentID);
    }
  }
}

new TabBarView(
  document.querySelector("#tool-bar .tab-bar"),
  document.querySelector("main.tabbed-container")
);

if (DEBUG_LSP_LOG) {
  for (let i = 0; i < 10; ++i) {
    lspLog.addClientToServerMessage(
      0,
      `{"method":"textDocument/didChange","jsonrpc":"2.0","params":{"contentChanges":[{"text":"console.log('hello world');\\n\\n"}],"textDocument":{"uri":"file:///home/strager/Projects/quicklint-js/hello.js","version":4264}}}`
    );
    lspLog.addClientToServerMessage(
      0,
      `{"method":"textDocument/didChange","jsonrpc":"2.0","params":{"contentChanges":[{"text":"console.log('hello world');\\n"}],"textDocument":{"uri":"file:///home/strager/Projects/quicklint-js/hello.js","version":4265}}}`
    );
    lspLog.addClientToServerMessage(
      0,
      `{"method":"textDocument/didChange","jsonrpc":"2.0","params":{"contentChanges":[{"text":"console.log('hello world');\\n"}],"textDocument":{"uri":"file:///home/strager/Projects/quicklint-js/hello.js","version":4266}}}`
    );
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
