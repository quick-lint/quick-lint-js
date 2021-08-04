// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

"use strict";

// TODO(strager): Make this configurable.
// For build instructions, see wasm/README.md.
let VSCODE_WASM_MODULE_PATH = "./dist/quick-lint-js-vscode.wasm";

let DocumentLinterState = {
  // A DocumentForVSCode hasn't been created yet.
  NO_WASM_DOC: "NO_WASM_DOC",

  // A DocumentForVSCode is in the process of being created.
  CREATING_WASM_DOC: "CREATING_WASM_DOC",

  // A DocumentForVSCode has been created, but it has no text.
  WASM_DOC_UNINITIALIZED: "WASM_DOC_UNINITIALIZED",

  // A DocumentForVSCode has been created, and its text is up-to-date with the
  // vscode.Document.
  WASM_DOC_LOADED: "WASM_DOC_LOADED",

  // The DocumentForVSCode's Process crashed, and we are creating a new Process
  // and DocumentForVSCode.
  //
  // Document changes should be queued.
  RECOVERING: "RECOVERING",

  DISPOSED: "DISPOSED",
};

class LintingCrashed extends Error {
  constructor(originalError) {
    super(String(originalError));
    this.originalError = originalError;
  }
}
exports.LintingCrashed = LintingCrashed;

class DocumentLinterDisposed extends Error {}
exports.DocumentLinterDisposed = DocumentLinterDisposed;

class AbstractSyncedDocument {
  constructor(documentProcessManager) {
    this._documentProcessManager = documentProcessManager;
    this._state = DocumentLinterState.NO_WASM_DOC;

    // Used only in states: CREATING_WASM_DOC
    this._wasmDocPromise = null;

    // Used only in states: WASM_DOC_UNINITIALIZED, WASM_DOC_LOADED
    this._wasmDoc = null;

    // Used only in states: WASM_DOC_LOADED, RECOVERING
    this._pendingChanges = [];

    // Used only in states: RECOVERING
    this._recoveryPromise = null;
  }

  async _createWASMDoc() {
    assertEqual(this._state, DocumentLinterState.NO_WASM_DOC);
    this._state = DocumentLinterState.CREATING_WASM_DOC;
    this._wasmDocPromise = (async () => {
      let wasmDoc;
      try {
        let workspace =
          await this._documentProcessManager.getOrCreateWorkspaceAsync();
        wasmDoc = workspace.createDocument();
      } catch (e) {
        if (e instanceof ProcessCrashed) {
          throw new LintingCrashed(e);
        } else {
          throw e;
        }
      }

      if (this._state === DocumentLinterState.DISPOSED) {
        wasmDoc.dispose();
        throw new DocumentLinterDisposed();
      }
      assertEqual(this._state, DocumentLinterState.CREATING_WASM_DOC);
      this._wasmDoc = wasmDoc;
      this._state = DocumentLinterState.WASM_DOC_UNINITIALIZED;
      return wasmDoc;
    })();
    return await this._wasmDocPromise;
  }

  async disposeAsync() {
    let oldState = this._state;
    this._state = DocumentLinterState.DISPOSED;
    switch (oldState) {
      case DocumentLinterState.NO_WASM_DOC:
        break;

      case DocumentLinterState.CREATING_WASM_DOC:
      case DocumentLinterState.WASM_DOC_UNINITIALIZED:
      case DocumentLinterState.WASM_DOC_LOADED: {
        try {
          await this._wasmDocPromise;
        } catch (e) {
          if (e instanceof DocumentLinterDisposed) {
            // Ignore.
          } else if (e instanceof LintingCrashed) {
            // Ignore.
          } else {
            throw e;
          }
        }
        if (this._wasmDoc !== null) {
          try {
            this._wasmDoc.dispose();
          } catch (e) {
            if (e instanceof ProcessCrashed) {
              // Ignore.
            } else {
              throw e;
            }
          }
        }
        break;
      }

      case DocumentLinterState.DISPOSED:
        // TODO(strager): Should double-dispose be okay?
        throw new DocumentLinterDisposed();

      default:
        throw new Error(`Unexpected linter state: ${this._state}`);
    }
  }

  async editorChangedVisibilityAsync() {
    switch (this._state) {
      case DocumentLinterState.NO_WASM_DOC:
        await this._createWASMDoc();
        await this.editorChangedVisibilityAsync();
        break;

      case DocumentLinterState.CREATING_WASM_DOC:
        await this._wasmDocPromise;
        await this.editorChangedVisibilityAsync();
        break;

      case DocumentLinterState.WASM_DOC_UNINITIALIZED:
        await this._initializeWASMDocAsync();
        break;

      case DocumentLinterState.WASM_DOC_LOADED:
        // No changes could have been made with the editor closed. Ignore.
        break;

      case DocumentLinterState.DISPOSED:
        throw new DocumentLinterDisposed();

      default:
        throw new Error(`Unexpected linter state: ${this._state}`);
    }
  }

  async textChangedAsync(changes) {
    // BEGIN CRITICAL SECTION (no awaiting below)
    switch (this._state) {
      case DocumentLinterState.NO_WASM_DOC:
        // END CRITICAL SECTION (no awaiting above)
        await this._createWASMDoc();
        await this._initializeWASMDocAsync();
        break;

      case DocumentLinterState.CREATING_WASM_DOC:
        // END CRITICAL SECTION (no awaiting above)
        await this._wasmDocPromise;
        await this._initializeWASMDocAsync();
        break;

      case DocumentLinterState.WASM_DOC_UNINITIALIZED:
        // END CRITICAL SECTION (no awaiting above)
        await this._initializeWASMDocAsync();
        break;

      case DocumentLinterState.WASM_DOC_LOADED:
        this._pendingChanges.push(...changes);
        try {
          for (let change of this._pendingChanges) {
            this._wasmDoc.replaceText(change.range, change.text);
          }
          this._pendingChanges.length = 0;
          // END CRITICAL SECTION (no awaiting above)

          this._documentSynced();
        } catch (e) {
          // END CRITICAL SECTION (no awaiting above)
          if (e instanceof ProcessCrashed) {
            await this._recoverFromCrashAsync(e);
          } else {
            throw e;
          }
        }
        break;

      case DocumentLinterState.RECOVERING:
        this._pendingChanges.push(...changes);
        // END CRITICAL SECTION (no awaiting above)
        await this._recoveryPromise;
        // Changes should have been applied during recovery.
        assertEqual(this._pendingChanges.includes(changes[0]), false);
        break;

      case DocumentLinterState.DISPOSED:
        throw new DocumentLinterDisposed();

      default:
        throw new Error(`Unexpected linter state: ${this._state}`);
    }
  }

  // Transition: WASM_DOC_UNINITIALIZED -> WASM_DOC_LOADED (or NO_WASM_DOC on error)
  async _initializeWASMDocAsync() {
    // BEGIN CRITICAL SECTION (no awaiting below)
    assertEqual(this._state, DocumentLinterState.WASM_DOC_UNINITIALIZED);
    try {
      this._wasmDoc.replaceText(
        {
          start: { line: 0, character: 0 },
          end: { line: 0, character: 0 },
        },
        this._getText()
      );
      this._pendingChanges.length = 0;
      this._state = DocumentLinterState.WASM_DOC_LOADED;
      // END CRITICAL SECTION (no awaiting above)

      this._documentSynced();
    } catch (e) {
      if (e instanceof ProcessCrashed) {
        await this._recoverFromCrashAsync(e);
      } else {
        throw e;
      }
    }
  }

  // Transition: any -> RECOVERING -> WASM_DOC_LOADED (or NO_WASM_DOC on error)
  async _recoverFromCrashAsync(error) {
    // BEGIN CRITICAL SECTION (no awaiting below)
    console.warn(
      `[quick-lint-js] warning: Parser process crashed. Recovering: ${error.stack}`
    );
    this._state = DocumentLinterState.RECOVERING;
    this._recoveryPromise = (async () => {
      try {
        let workspace =
          await this._documentProcessManager.getOrCreateWorkspaceAsync();
        let wasmDoc = workspace.createDocument();

        // BEGIN CRITICAL SECTION (no awaiting below)
        assertEqual(this._state, DocumentLinterState.RECOVERING);
        wasmDoc.replaceText(
          {
            start: { line: 0, character: 0 },
            end: { line: 0, character: 0 },
          },
          this._getText()
        );
        this._pendingChanges.length = 0;
        this._wasmDoc = wasmDoc;
        this._state = DocumentLinterState.WASM_DOC_LOADED;
        // END CRITICAL SECTION (no awaiting above)

        this._documentSynced();
      } catch (e) {
        this._wasmDoc = null;
        this._wasmDocPromise = null;
        this._state = DocumentLinterState.NO_WASM_DOC;
        if (e instanceof ProcessCrashed) {
          throw new LintingCrashed(e);
        } else {
          throw e;
        }
      }
    })();
    // END CRITICAL SECTION (no awaiting above)
    await this._recoveryPromise;
  }

  // Abstract:
  _getText() {
    throw new Error("Please override _getText in a derived class");
  }

  // Abstract:
  _documentSynced() {
    throw new Error("Please override _documentSynced in a derived class");
  }
}

class DocumentLinter extends AbstractSyncedDocument {
  // document has the following methods:
  //
  //   getText(): string;
  //   setDiagnostics(diagnostics: Object[]): void;
  //   removeDiagnostics(): void;
  constructor(document, documentProcessManager) {
    super(documentProcessManager);
    this._document = document;
  }

  // Override:
  _getText() {
    return this._document.getText();
  }

  // Override:
  _documentSynced() {
    let diags = this._wasmDoc.lint();
    this._document.setDiagnostics(diags);
  }

  // Override:
  async disposeAsync() {
    await super.disposeAsync();
    this._document.removeDiagnostics();
  }
}
exports.DocumentLinter = DocumentLinter;

class DocumentProcessManager {
  constructor() {
    // TODO(strager): Allow developers to reload the .wasm file.
    this._processFactoryPromise = createProcessFactoryAsync();
    this._processesCreated = 0;
    this._processAndWorkspacePromise = null;
  }

  async _createNewProcessAndWorkspaceAsync() {
    let processFactory = await this._processFactoryPromise;
    let process = await processFactory.createProcessAsync();
    this._processesCreated += 1;
    let workspace = process.createWorkspaceForVSCode();
    return { process, workspace };
  }

  async getOrCreateWorkspaceAsync() {
    // BEGIN CRITICAL SECTION (no awaiting below)
    if (this._processAndWorkspacePromise === null) {
      this._processAndWorkspacePromise =
        this._createNewProcessAndWorkspaceAsync();
      // END CRITICAL SECTION (no awaiting above)
    } else {
      // END CRITICAL SECTION (no awaiting above)
    }
    let { process, workspace } = await this._processAndWorkspacePromise;
    // BEGIN CRITICAL SECTION (no awaiting below)
    while (process.isTainted()) {
      this._processAndWorkspacePromise =
        this._createNewProcessAndWorkspaceAsync();
      // END CRITICAL SECTION (no awaiting above)
      ({ process, workspace } = await this._processAndWorkspacePromise);
    }
    return workspace;
  }

  // For testing only.
  get numberOfProcessesEverCreated() {
    return this._processesCreated;
  }
}
exports.DocumentProcessManager = DocumentProcessManager;

async function createProcessFactoryAsync() {
  if (typeof window === "undefined") {
    // Node.js.
    let fs = require("fs");
    let path = require("path");

    let wasmCode = await fs.promises.readFile(
      path.join(__dirname, VSCODE_WASM_MODULE_PATH)
    );
    let wasmModule = await WebAssembly.compile(wasmCode);
    return new ProcessFactory(wasmModule);
  } else {
    // Browser.
    let wasmModule = await WebAssembly.compileStreaming(
      fetch(VSCODE_WASM_MODULE_PATH)
    );
    return new ProcessFactory(wasmModule);
  }
}
exports.createProcessFactoryAsync = createProcessFactoryAsync;

class ProcessCrashed extends Error {}
exports.ProcessCrashed = ProcessCrashed;

class ProcessAborted extends ProcessCrashed {}
exports.ProcessAborted = ProcessAborted;

class ProcessCrashedWithUnknownError extends ProcessCrashed {
  constructor(originalError) {
    super(originalError.stack);
    this.originalError = originalError;
  }
}
exports.ProcessCrashedWithUnknownError = ProcessCrashedWithUnknownError;

// A compiled WebAssembly module.
class ProcessFactory {
  constructor(wasmModule) {
    this._wasmModule = wasmModule;
  }

  async createProcessAsync() {
    let wasmInstance = await WebAssembly.instantiate(this._wasmModule, {
      wasi_snapshot_preview1: {
        fd_close: () => {
          throw new Error("Not implemented: fd_close");
        },
        fd_read: () => {
          throw new Error("Not implemented: fd_read");
        },
        fd_seek: () => {
          throw new Error("Not implemented: fd_seek");
        },

        environ_get: (environ, environBuf) => {
          return 0;
        },

        environ_sizes_get: (outEnvironc, outEnvironBufSize) => {
          let heap = wasmInstance.exports.memory.buffer;
          new Uint32Array(heap, outEnvironc)[0] = 0;
          new Uint32Array(heap, outEnvironBufSize)[0] = 0;
          return 0;
        },

        proc_exit: () => {
          throw new ProcessAborted("quick-lint-js process exited");
        },

        fd_write: (fd, iovsData, iovsSize, outWrittenSize) => {
          let heap = wasmInstance.exports.memory.buffer;
          let iovs = new Uint32Array(heap, iovsData, iovsSize * 8);
          let bytesWritten = 0;
          for (let i = 0; i < iovsSize; ++i) {
            let bufferPointer = iovs[i * 2 + 0];
            let bufferSize = iovs[i * 2 + 1];
            let buffer = new Uint8Array(heap, bufferPointer, bufferSize);
            // TODO(strager): Visual Studio Code doesn't like writing to stdout.
            // Should we use console instead?
            process.stdout.write(buffer);
            bytesWritten += buffer.byteLength;
          }
          new Uint32Array(heap, outWrittenSize)[0] = bytesWritten;
          return 0;
        },
      },
    });
    wasmInstance.exports._initialize();
    return new Process(wasmInstance);
  }
}

let nextProcessIDForDebugging = 1;

// A WebAssembly instance.
//
// If a Process crashes, every DocumentForVSCode or DocumentForWebDemo
// associated with its creating Process is tainted.
class Process {
  constructor(wasmInstance) {
    this._idForDebugging = nextProcessIDForDebugging++;
    this._wasmInstance = wasmInstance;
    this._crashedException = null;

    let process = this;
    function wrap(name) {
      if (!Object.prototype.hasOwnProperty.call(wasmInstance.exports, name)) {
        throw new TypeError(`WASM does not export function: ${name}`);
      }
      let func = wasmInstance.exports[name];
      return (...args) => {
        try {
          exports.maybeInjectFault(process, name);
          try {
            return func(...args);
          } catch (e) {
            throw new ProcessCrashedWithUnknownError(e);
          }
        } catch (e) {
          if (e instanceof ProcessCrashed) {
            process._taint(e);
          }
          throw e;
        }
      };
    }

    this._heap = wasmInstance.exports.memory.buffer;

    this._malloc = wrap("malloc");
    this._free = wrap("free");
    this._vscodeCreateSourceDocument = wrap(
      "qljs_vscode_create_source_document"
    );
    this._vscodeCreateWorkspace = wrap("qljs_vscode_create_workspace");
    this._vscodeDestroyDocument = wrap("qljs_vscode_destroy_document");
    this._vscodeDestroyWorkspace = wrap("qljs_vscode_destroy_workspace");
    this._vscodeLint = wrap("qljs_vscode_lint");
    this._vscodeReplaceText = wrap("qljs_vscode_replace_text");
    this._webDemoCreateDocument = wrap("qljs_web_demo_create_document");
    this._webDemoDestroyDocument = wrap("qljs_web_demo_destroy_document");
    this._webDemoLint = wrap("qljs_web_demo_lint");
    this._webDemoSetText = wrap("qljs_web_demo_set_text");
  }

  isTainted() {
    return this._crashedException !== null;
  }

  _taint(exception) {
    this._crashedException = exception;

    function tainted() {
      throw this._crashedException;
    }

    // Reduce memory usage.
    this._wasmInstance = null;
    this._heap = null;

    // Make future calls crash and also reduce memory usage.
    this._malloc = tainted;
    this._free = tainted;
    this._vscodeCreateSourceDocument = tainted;
    this._vscodeCreateWorkspace = tainted;
    this._vscodeDestroyDocument = tainted;
    this._vscodeDestroyWorkspace = tainted;
    this._vscodeLint = tainted;
    this._vscodeReplaceText = tainted;
    this._webDemoCreateDocument = tainted;
    this._webDemoDestroyDocument = tainted;
    this._webDemoLint = tainted;
    this._webDemoSetText = tainted;
  }

  toString() {
    return `Process(id=${this._idForDebugging})`;
  }

  createWorkspaceForVSCode() {
    return new WorkspaceForVSCode(this);
  }

  async createDocumentForWebDemoAsync() {
    return new DocumentForWebDemo(this);
  }
}

class WorkspaceForVSCode {
  constructor(process) {
    this._process = process;
    this._wasmWorkspace = this._process._vscodeCreateWorkspace();
  }

  // Returns a DocumentForVSCode
  createDocument() {
    return new DocumentForVSCode(this);
  }

  dispose() {
    this._process._vscodeDestroyWorkspace(this._wasmWorkspace);
    this._wasmWorkspace = null;
  }
}

class DocumentForVSCode {
  // Private. Call WorkspaceForVSCode#createDocument instead.
  constructor(workspace) {
    if (!(workspace instanceof WorkspaceForVSCode)) {
      throw new TypeError(`Expected WorkspaceForVSCode, but got ${workspace}`);
    }
    this._process = workspace._process;
    this._wasmDoc = this._process._vscodeCreateSourceDocument(
      workspace._wasmWorkspace
    );
  }

  replaceText(range, replacementText) {
    let utf8ReplacementText = encodeUTF8String(replacementText, this._process);
    try {
      this._process._vscodeReplaceText(
        this._wasmDoc,
        range.start.line,
        range.start.character,
        range.end.line,
        range.end.character,
        utf8ReplacementText.pointer,
        utf8ReplacementText.byteSize
      );
      utf8ReplacementText.dispose();
    } catch (e) {
      if (e instanceof ProcessCrashed) {
        // Don't touch the Process.
      } else {
        utf8ReplacementText.dispose();
      }
      throw e;
    }
  }

  lint() {
    let diagnosticsPointer = this._process._vscodeLint(this._wasmDoc);

    let rawDiagnosticsU32 = new Uint32Array(
      this._process._heap,
      diagnosticsPointer
    );
    let rawDiagnosticsPtr = new Uint32Array(
      this._process._heap,
      diagnosticsPointer
    );
    // struct qljs_vscode_diagnostic {
    //   const char* message;
    //   const char* code;
    //   qljs_vscode_severity severity;
    //   int start_line;
    //   int start_character;
    //   int end_line;
    //   int end_character;
    // };
    let ERROR = {
      message: 0,
      code: 1,
      severity: 2,
      start_line: 3,
      start_character: 4,
      end_line: 5,
      end_character: 6,

      _ptr_size: 7,
      _u32_size: 7,
    };
    let diagnostics = [];
    for (let i = 0; ; ++i) {
      let messagePtr = rawDiagnosticsPtr[i * ERROR._ptr_size + ERROR.message];
      if (messagePtr === 0) {
        break;
      }
      let codePtr = rawDiagnosticsPtr[i * ERROR._ptr_size + ERROR.code];
      diagnostics.push({
        code: decodeUTF8CString(new Uint8Array(this._process._heap, codePtr)),
        message: decodeUTF8CString(
          new Uint8Array(this._process._heap, messagePtr)
        ),
        severity: rawDiagnosticsU32[i * ERROR._u32_size + ERROR.severity],
        startLine: rawDiagnosticsU32[i * ERROR._u32_size + ERROR.start_line],
        startCharacter:
          rawDiagnosticsU32[i * ERROR._u32_size + ERROR.start_character],
        endLine: rawDiagnosticsU32[i * ERROR._u32_size + ERROR.end_line],
        endCharacter:
          rawDiagnosticsU32[i * ERROR._u32_size + ERROR.end_character],
      });
    }
    return diagnostics;
  }

  dispose() {
    this._process._vscodeDestroyDocument(this._wasmDoc);
    this._wasmDoc = null;
    this._process._vscodeDestroyWorkspace(this._wasmWorkspace);
    this._wasmWorkspace = null;
  }

  get process() {
    return this._process;
  }
}

class DocumentForWebDemo {
  constructor(process) {
    this._process = process;
    this._wasmDoc = this._process._webDemoCreateDocument();
  }

  setText(text) {
    let utf8Text = encodeUTF8String(text, this._process);
    try {
      this._process._webDemoSetText(
        this._wasmDoc,
        utf8Text.pointer,
        utf8Text.byteSize
      );
    } finally {
      utf8Text.dispose();
    }
  }

  lint() {
    let diagnosticsPointer = this._process._webDemoLint(this._wasmDoc);

    let rawDiagnosticsU32 = new Uint32Array(
      this._process._heap,
      diagnosticsPointer
    );
    let rawDiagnosticsPtr = new Uint32Array(
      this._process._heap,
      diagnosticsPointer
    );
    // struct qljs_web_demo_diagnostic {
    //   const char* message;
    //   const char* code;
    //   qljs_vscode_severity severity;
    //   int begin_offset;
    //   int end_offset;
    // };
    let ERROR = {
      message: 0,
      code: 1,
      severity: 2,
      begin_offset: 3,
      end_offset: 4,

      _ptr_size: 5,
      _u32_size: 5,
    };
    let diagnostics = [];
    for (let i = 0; ; ++i) {
      let messagePtr = rawDiagnosticsPtr[i * ERROR._ptr_size + ERROR.message];
      if (messagePtr === 0) {
        break;
      }
      let codePtr = rawDiagnosticsPtr[i * ERROR._ptr_size + ERROR.code];
      diagnostics.push({
        code: decodeUTF8CString(new Uint8Array(this._process._heap, codePtr)),
        message: decodeUTF8CString(
          new Uint8Array(this._process._heap, messagePtr)
        ),
        severity: rawDiagnosticsU32[i * ERROR._u32_size + ERROR.severity],
        begin: rawDiagnosticsU32[i * ERROR._u32_size + ERROR.begin_offset],
        end: rawDiagnosticsU32[i * ERROR._u32_size + ERROR.end_offset],
      });
    }
    return diagnostics;
  }

  dispose() {
    this._process._webDemoDestroyDocument(this._wasmDoc);
    this._wasmDoc = null;
  }
}

let DiagnosticSeverity = {
  ERROR: 1,
  WARNING: 2,
};
exports.DiagnosticSeverity = DiagnosticSeverity;

function encodeUTF8String(string, process) {
  let maxUTF8BytesPerUTF16CodeUnit = Math.ceil(
    Math.max(
      3 / 1, // U+0000..U+ffff: 1..3 UTF-8 bytes, 1 UTF-16 code unit
      5 / 2 // U+10000..: 5 UTF-8 bytes, 2 UTF-16 code units
    )
  );
  let maxSize = string.length * maxUTF8BytesPerUTF16CodeUnit;
  let textUTF8Pointer = process._malloc(maxSize);
  try {
    let encoder = new TextEncoder();
    let encodeResult = encoder.encodeInto(
      string,
      new Uint8Array(process._heap, textUTF8Pointer, maxSize)
    );
    if (encodeResult.read !== string.length) {
      throw new Error(
        `Assertion failure: expected encodeResult.read (${encodeResult.read}) to equal string.length (${string.length})`
      );
    }
    let textUTF8Size = encodeResult.written;
    return {
      pointer: textUTF8Pointer,
      byteSize: textUTF8Size,
      dispose: dispose,
    };
  } catch (e) {
    dispose();
    throw e;
  }

  function dispose() {
    process._free(textUTF8Pointer);
  }
}

function decodeUTF8CString(bytes) {
  let nullTerminatorIndex = bytes.indexOf(0);
  if (nullTerminatorIndex < 0) {
    throw new Error("null terminator not found in C string");
  }
  return new TextDecoder().decode(bytes.subarray(0, nullTerminatorIndex));
}

function assertEqual(actual, expected) {
  if (actual !== expected) {
    throw new Error(
      `Assertion failure: expected ${expected} but got ${actual}`
    );
  }
}

// This function is called when functions in this module call C++ functions.
//
// Replace this function with a function which throws an exception to simulate
// errors such as segfaults and OOMs.
exports.maybeInjectFault = (_process, _functionName) => {};

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
