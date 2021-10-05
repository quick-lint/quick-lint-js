// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

const fs = require("fs");
const path = require("path");

class PerformanceWriter {
  constructor(extensionContext) {
    if (extensionContext !== undefined) {
      this._extensionContext = extensionContext;
      this._tmpLogLines = [];
      this._currentIndex = null;
      this._fileIndex = 0;
      this._timerActivated = false;
      this._currentSessionMap = {};
      fs.mkdir(this._extensionContext.logUri.fsPath, (err) => {
        if (!err) {
          this._writableStream = fs.createWriteStream(
            this._performanceTxtPath,
            {
              encoding: "utf8",
              flags: "a",
            }
          );
          this.writeHeaderLine();
        }
      });

      this._performanceTxtPath = path.join(
        this._extensionContext.logUri.fsPath,
        "qljsPerformanceTrace.txt"
      );
    }
  }

  fileNameOptimizer(fileName) {
    if (
      Object.prototype.hasOwnProperty.call(this._currentSessionMap, fileName)
    ) {
      this._currentIndex = this._currentSessionMap[fileName];
    } else {
      this._fileIndex++;
      this._currentSessionMap[fileName] = this._fileIndex;
      this._currentIndex = this._fileIndex;
      this.writeNewFileLine(fileName);
    }
  }

  logLintDuration(timeTaken, fileName) {
    this.fileNameOptimizer(fileName);
    this.writeExistingFileLine(timeTaken);
    this.setWriteToFileTimer();
  }

  writeHeaderLine() {
    this._tmpLogLines.push(
      "timestamp",
      "file-ref",
      "event-id",
      "file-name-or-duration"
    );
  }

  writeNewFileLine(fileName) {
    this._tmpLogLines.push(Date.now(), this._currentIndex, "n", fileName);
  }

  writeExistingFileLine(timeTaken) {
    this._tmpLogLines.push(Date.now(), this._currentIndex, "p", timeTaken);
  }

  setWriteToFileTimer() {
    if (!this._timerActivated) {
      this._timerActivated = true;
      setTimeout(() => {
        this.writeToFile();
      }, 1000);
    }
  }

  writeToFile() {
    while (this._tmpLogLines.length > 0) {
      let tracingLine = this._tmpLogLines.splice(0, 4);
      let isHeaderLine = typeof tracingLine[0] === "string";
      if (isHeaderLine) {
        tracingLine = tracingLine.join(",");
      } else {
        tracingLine = "\n" + tracingLine.join(",");
      }

      this._writableStream.write(tracingLine);
    }
    this._timerActivated = false;
  }
}

module.exports = PerformanceWriter;

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
