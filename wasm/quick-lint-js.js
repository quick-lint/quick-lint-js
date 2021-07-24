// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

"use strict";

// TODO(strager): Make this configurable.
// For build instructions, see plugin/vscode/BUILDING.md.
let VSCODE_WASM_MODULE_PATH = "./dist/quick-lint-js-vscode.wasm";

let DocumentLinterState = {
  // A DocumentForVSCode hasn't been created yet.
  NO_PARSER: "NO_PARSER",

  // A DocumentForVSCode is in the process of being created.
  CREATING_PARSER: "CREATING_PARSER",

  // A DocumentForVSCode has been created, but it has no text.
  PARSER_UNINITIALIZED: "PARSER_UNINITIALIZED",

  // A DocumentForVSCode has been created, and its text is up-to-date with the
  // vscode.Document.
  PARSER_LOADED: "PARSER_LOADED",

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

class DocumentLinter {
  // document has the following methods:
  //
  //   getText(): string;
  //   setDiagnostics(diagnostics: Object[]): void;
  //   removeDiagnostics(): void;
  constructor(document, documentProcessManager) {
    this._document = document;
    this._documentProcessManager = documentProcessManager;
    this._state = DocumentLinterState.NO_PARSER;

    // Used only in states: CREATING_PARSER
    this._parserPromise = null;

    // Used only in states: PARSER_UNINITIALIZED, PARSER_LOADED
    this._parser = null;

    // Used only in states: PARSER_LOADED, RECOVERING
    this._pendingChanges = [];

    // Used only in states: RECOVERING
    this._recoveryPromise = null;
  }

  async _createParser() {
    assertEqual(this._state, DocumentLinterState.NO_PARSER);
    this._state = DocumentLinterState.CREATING_PARSER;
    this._parserPromise = (async () => {
      let process;
      let parser;
      try {
        process = await this._documentProcessManager.getOrCreateProcessAsync();
        parser = process.createDocumentForVSCode();
      } catch (e) {
        if (e instanceof ProcessCrashed) {
          this._documentProcessManager.processCrashed(process);
          throw new LintingCrashed(e);
        } else {
          throw e;
        }
      }

      if (this._state === DocumentLinterState.DISPOSED) {
        parser.dispose();
        throw new DocumentLinterDisposed();
      }
      assertEqual(this._state, DocumentLinterState.CREATING_PARSER);
      this._parser = parser;
      this._state = DocumentLinterState.PARSER_UNINITIALIZED;
      return parser;
    })();
    return await this._parserPromise;
  }

  async disposeAsync() {
    let oldState = this._state;
    this._state = DocumentLinterState.DISPOSED;
    switch (oldState) {
      case DocumentLinterState.NO_PARSER:
        break;

      case DocumentLinterState.CREATING_PARSER:
      case DocumentLinterState.PARSER_UNINITIALIZED:
      case DocumentLinterState.PARSER_LOADED: {
        try {
          await this._parserPromise;
        } catch (e) {
          if (e instanceof DocumentLinterDisposed) {
            // Ignore.
          } else if (e instanceof LintingCrashed) {
            // Ignore.
          } else {
            throw e;
          }
        }
        if (this._parser !== null) {
          try {
            this._parser.dispose();
          } catch (e) {
            if (e instanceof ProcessCrashed) {
              this._documentProcessManager.processCrashed(this._parser.process);
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
    this._document.removeDiagnostics();
  }

  dispose() {
    logAsyncErrors(async () => {
      await this.disposeAsync();
    });
  }

  async editorChangedVisibilityAsync() {
    switch (this._state) {
      case DocumentLinterState.NO_PARSER:
        await this._createParser();
        await this.editorChangedVisibilityAsync();
        break;

      case DocumentLinterState.CREATING_PARSER:
        await this._parserPromise;
        await this.editorChangedVisibilityAsync();
        break;

      case DocumentLinterState.PARSER_UNINITIALIZED:
        await this._initializeParserAsync();
        break;

      case DocumentLinterState.PARSER_LOADED:
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
      case DocumentLinterState.NO_PARSER:
        // END CRITICAL SECTION (no awaiting above)
        await this._createParser();
        await this._initializeParserAsync();
        break;

      case DocumentLinterState.CREATING_PARSER:
        // END CRITICAL SECTION (no awaiting above)
        await this._parserPromise;
        await this._initializeParserAsync();
        break;

      case DocumentLinterState.PARSER_UNINITIALIZED:
        // END CRITICAL SECTION (no awaiting above)
        await this._initializeParserAsync();
        break;

      case DocumentLinterState.PARSER_LOADED:
        this._pendingChanges.push(...changes);
        try {
          for (let change of this._pendingChanges) {
            this._parser.replaceText(change.range, change.text);
          }
          this._pendingChanges.length = 0;
          // END CRITICAL SECTION (no awaiting above)

          let diags = this._parser.lint();
          this._document.setDiagnostics(diags);
        } catch (e) {
          // END CRITICAL SECTION (no awaiting above)
          if (e instanceof ProcessCrashed) {
            this._documentProcessManager.processCrashed(this._parser.process);
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

  // Transition: PARSER_UNINITIALIZED -> PARSER_LOADED (or NO_PARSER on error)
  async _initializeParserAsync() {
    // BEGIN CRITICAL SECTION (no awaiting below)
    assertEqual(this._state, DocumentLinterState.PARSER_UNINITIALIZED);
    try {
      this._parser.replaceText(
        {
          start: { line: 0, character: 0 },
          end: { line: 0, character: 0 },
        },
        this._document.getText()
      );
      this._pendingChanges.length = 0;
      this._state = DocumentLinterState.PARSER_LOADED;
      // END CRITICAL SECTION (no awaiting above)

      let diags = this._parser.lint();
      this._document.setDiagnostics(diags);
    } catch (e) {
      if (e instanceof ProcessCrashed) {
        this._documentProcessManager.processCrashed(this._parser.process);
        await this._recoverFromCrashAsync(e);
      } else {
        throw e;
      }
    }
  }

  // Transition: any -> RECOVERING -> PARSER_LOADED (or NO_PARSER on error)
  async _recoverFromCrashAsync(error) {
    // BEGIN CRITICAL SECTION (no awaiting below)
    console.warn(
      `[quick-lint-js] warning: Parser process crashed. Recovering: ${error.stack}`
    );
    this._state = DocumentLinterState.RECOVERING;
    this._recoveryPromise = (async () => {
      let diags;
      let process;
      try {
        process = await this._documentProcessManager.getOrCreateProcessAsync();
        let parser = process.createDocumentForVSCode();

        // BEGIN CRITICAL SECTION (no awaiting below)
        assertEqual(this._state, DocumentLinterState.RECOVERING);
        parser.replaceText(
          {
            start: { line: 0, character: 0 },
            end: { line: 0, character: 0 },
          },
          this._document.getText()
        );
        this._pendingChanges.length = 0;
        this._parser = parser;
        this._state = DocumentLinterState.PARSER_LOADED;
        // END CRITICAL SECTION (no awaiting above)

        diags = parser.lint();
      } catch (e) {
        this._parser = null;
        this._parserPromise = null;
        this._state = DocumentLinterState.NO_PARSER;
        if (e instanceof ProcessCrashed) {
          this._documentProcessManager.processCrashed(process);
          throw new LintingCrashed(e);
        } else {
          throw e;
        }
      }
      this._document.setDiagnostics(diags);
    })();
    // END CRITICAL SECTION (no awaiting above)
    await this._recoveryPromise;
  }
}
exports.DocumentLinter = DocumentLinter;

class DocumentProcessManager {
  constructor() {
    // TODO(strager): Allow developers to reload the .wasm file.
    this._processFactoryPromise = createProcessFactoryAsync();
    this._processesCreated = 0;
    this._processPromise = null;
    this._process = null;
  }

  processCrashed(process) {
    if (!(process instanceof Process)) {
      throw new TypeError(`Expected Process, but got ${process}`);
    }
    if (this._process === process) {
      this._process = null;
      this._processPromise = null;
    }
  }

  async createNewProcessAsync() {
    let processFactory = await this._processFactoryPromise;
    let process = await processFactory.createProcessAsync();
    this._processesCreated += 1;
    this._process = process;
    return process;
  }

  async getOrCreateProcessAsync() {
    // BEGIN CRITICAL SECTION (no awaiting below)
    if (this._processPromise === null) {
      this._processPromise = this.createNewProcessAsync();
      // END CRITICAL SECTION (no awaiting above)
    }
    return await this._processPromise;
  }

  _forgetCurrentProcess() {
    this._process = null;
    this._processPromise = null;
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
        if (process._crashedException !== null) {
          throw process._crashedException;
        }
        try {
          exports.maybeInjectFault(process, name);
          try {
            return func(...args);
          } catch (e) {
            throw new ProcessCrashedWithUnknownError(e);
          }
        } catch (e) {
          if (e instanceof ProcessCrashed) {
            process._crashedException = e;
          }
          throw e;
        }
      };
    }

    this._heap = wasmInstance.exports.memory.buffer;

    this._malloc = wrap("malloc");
    this._free = wrap("free");
    this._vscodeCreateDocument = wrap("qljs_vscode_create_document");
    this._vscodeDestroyDocument = wrap("qljs_vscode_destroy_document");
    this._vscodeLint = wrap("qljs_vscode_lint");
    this._vscodeReplaceText = wrap("qljs_vscode_replace_text");
    this._webDemoCreateDocument = wrap("qljs_web_demo_create_document");
    this._webDemoDestroyDocument = wrap("qljs_web_demo_destroy_document");
    this._webDemoLint = wrap("qljs_web_demo_lint");
    this._webDemoSetText = wrap("qljs_web_demo_set_text");
  }

  toString() {
    return `Process(id=${this._idForDebugging})`;
  }

  createDocumentForVSCode() {
    return new DocumentForVSCode(this);
  }

  async createDocumentForWebDemoAsync() {
    return new DocumentForWebDemo(this);
  }
}

class DocumentForVSCode {
  constructor(process) {
    this._process = process;
    this._parser = this._process._vscodeCreateDocument();
  }

  replaceText(range, replacementText) {
    let utf8ReplacementText = encodeUTF8String(replacementText, this._process);
    try {
      this._process._vscodeReplaceText(
        this._parser,
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
    let diagnosticsPointer = this._process._vscodeLint(this._parser);

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
    this._process._vscodeDestroyDocument(this._parser);
    this._parser = null;
  }

  get process() {
    return this._process;
  }
}

class DocumentForWebDemo {
  constructor(process) {
    this._process = process;
    this._parser = this._process._webDemoCreateDocument();
  }

  setText(text) {
    let utf8Text = encodeUTF8String(text, this._process);
    try {
      this._process._webDemoSetText(
        this._parser,
        utf8Text.pointer,
        utf8Text.byteSize
      );
    } finally {
      utf8Text.dispose();
    }
  }

  lint() {
    let diagnosticsPointer = this._process._webDemoLint(this._parser);

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
    this._process._webDemoDestroyDocument(this._parser);
    this._parser = null;
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
