// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

"use strict";

// TODO(strager): Make this configurable.
// For build instructions, see plugin/vscode/BUILDING.md.
let VSCODE_WASM_MODULE_PATH = "./dist/quick-lint-js-vscode.wasm";

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

// A WebAssembly instance.
//
// If a Process crashes, every ParserForVSCode or ParserForWebDemo associated
// with its creating Process is tainted and should not be used anymore.
class Process {
  constructor(wasmInstance) {
    this._wasmInstance = wasmInstance;

    function wrap(name) {
      let func = wasmInstance.exports[name];
      return (...args) => {
        exports.maybeInjectFault(name);
        try {
          return func(...args);
        } catch (e) {
          throw new ProcessCrashedWithUnknownError(e);
        }
      };
    }

    this._heap = wasmInstance.exports.memory.buffer;

    this._malloc = wrap("malloc");
    this._free = wrap("free");
    this._vscodeCreateParser = wrap("qljs_vscode_create_parser");
    this._vscodeDestroyParser = wrap("qljs_vscode_destroy_parser");
    this._vscodeLint = wrap("qljs_vscode_lint");
    this._vscodeReplaceText = wrap("qljs_vscode_replace_text");
    this._webDemoCreateParser = wrap("qljs_web_demo_create_parser");
    this._webDemoDestroyParser = wrap("qljs_web_demo_destroy_parser");
    this._webDemoLint = wrap("qljs_web_demo_lint");
    this._webDemoSetText = wrap("qljs_web_demo_set_text");
  }

  async createParserForVSCodeAsync() {
    return new ParserForVSCode(this);
  }

  async createParserForWebDemoAsync() {
    return new ParserForWebDemo(this);
  }
}

class ParserForVSCode {
  constructor(process) {
    this._process = process;
    this._parser = this._process._vscodeCreateParser();
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
    } finally {
      utf8ReplacementText.dispose();
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
    this._process._vscodeDestroyParser(this._parser);
    this._parser = null;
  }
}

class ParserForWebDemo {
  constructor(process) {
    this._process = process;
    this._parser = this._process._webDemoCreateParser();
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
    this._process._webDemoDestroyParser(this._parser);
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

// This function is called when functions in this module call C++ functions.
//
// Replace this function with a function which throws an exception to simulate
// errors such as segfaults and OOMs.
exports.maybeInjectFault = () => {};

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
