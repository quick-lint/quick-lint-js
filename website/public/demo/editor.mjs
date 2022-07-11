// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

let docmarks = [];
export function markEditorText(editor, window, marks) {
  //let marker = new EditorMarker(editor, window, sanitizeMarks(marks));
  //marker.markNodes();
}

export function sanitizeMarks(marks) {
  marks = [...marks];
  marks.sort((a, b) => {
    if (a.begin < b.begin) {
      return -1;
    }
    if (a.begin > b.begin) {
      return +1;
    }
    if (a.end < b.end) {
      return +1;
    }
    if (a.end > b.end) {
      return -1;
    }
    return 0;
  });
  let result = [];
  for (let mark of marks) {
    let markAlreadyExists = result.some(
      (resultMark) => resultMark.begin === mark.begin
    );
    if (markAlreadyExists) {
      continue;
    }
    result.push(mark);
  }
  return result;
}

class EditorMarker {
  constructor(editor, window, marks) {
    this._editor = editor;
    this._window = window;
    this._marks = marks;

    this._currentOffset = 0;
    this._currentMarkIndex = 0;

    this._markBeginNode = null;
    this._markEndNode = null;
  }

  markNodes() {
    let currentNode = this._editor.firstChild;
    while (currentNode !== null) {
      switch (currentNode.nodeType) {
        case this._window.Node.ELEMENT_NODE:
          currentNode = this.handleElement(currentNode);
          break;
        case this._window.Node.TEXT_NODE:
          currentNode = this.handleTextNode(currentNode);
          break;
        default:
          throw new Error("Unsupported node type");
      }
    }
  }

  handleElement(currentNode) {
    if (currentNode.tagName === "BR") {
      this._currentOffset += 1; // "\n"
      return currentNode.nextSibling;
    } else {
      return this.handleElementWithChildren(currentNode);
    }
  }

  handleElementWithChildren(currentNode) {
    let currentNodeIndex = indexOfChildNode(this._editor, currentNode);
    let previousSibling = currentNode.previousSibling;
    let childNodes = [...currentNode.childNodes];
    currentNode.replaceWith(...childNodes);

    if (previousSibling === null) {
      return this._editor.firstChild;
    } else {
      return previousSibling.nextSibling;
    }
  }

  handleTextNode(currentNode) {
    let self = this;

    let currentMark =
      this._currentMarkIndex < this._marks.length
        ? this._marks[this._currentMarkIndex]
        : null;
    if (currentMark !== null) {
      if (currentNodeContainsOffset(currentMark.begin)) {
        let splitIndex = currentMark.begin - self._currentOffset;
        this._markBeginNode = splitNodeAtMarkBegin(splitIndex);
      }

      if (currentNodeContainsOffset(currentMark.end)) {
        let splitIndex = currentMark.end - this._currentOffset;
        this._markEndNode = splitNodeAtMarkEnd(splitIndex);

        let mark = this._window.document.createElement("mark");

        if (currentMark.message && currentMark.code && currentMark.severity) {
          mark.setAttribute("data-message", currentMark.message);
          mark.setAttribute("data-code", currentMark.code);
          mark.setAttribute("data-severity", currentMark.severity);
        }

        if (this._markBeginNode === null) {
          // Special case: insert an empty <mark> at the end.
          this._editor.appendChild(mark);
        } else if (
          this._markEndNode === null ||
          this._markBeginNode === this._markEndNode.nextSibling
        ) {
          // Special case: insert an empty <mark>.
          if (currentMark.begin !== currentMark.end) {
            throw new Error(
              "Unexpected: markBeginNode comes after markEndNode, but this should only happen if the current mark is empty"
            );
          }
          this._editor.insertBefore(mark, this._markBeginNode);
        } else {
          wrapNodes(mark, this._markBeginNode, this._markEndNode);
        }

        this._currentMarkIndex += 1;
        this._currentOffset += splitIndex;
        return mark.nextSibling;
      }
    }

    this._currentOffset += currentNode.textContent.length;
    return currentNode.nextSibling;

    function currentNodeContainsOffset(offset) {
      let currentNodeBeginOffset = self._currentOffset;
      let currentNodeEndOffset = currentNodeBeginOffset + currentNode.length;
      return currentNodeBeginOffset <= offset && offset <= currentNodeEndOffset;
    }

    // Returns the first node which should be inside the <mark>.
    function splitNodeAtMarkBegin(splitIndex) {
      if (splitIndex === 0) {
        return currentNode;
      } else if (splitIndex === currentNode.textContent.length) {
        return currentNode.nextSibling;
      } else {
        let nextNode = splitTextNode(currentNode, splitIndex, self._window);
        return nextNode;
      }
    }

    // Returns the last node which should be inside the <mark>.
    function splitNodeAtMarkEnd(splitIndex) {
      if (splitIndex === 0) {
        return currentNode.previousSibling;
      } else if (splitIndex === currentNode.textContent.length) {
        return currentNode;
      } else {
        let nextNode = splitTextNode(currentNode, splitIndex, self._window);
        return currentNode;
      }
    }
  }
}

function indexOfChildNode(parentNode, childNode) {
  let i = 0;
  let n = parentNode.firstChild;
  for (;;) {
    if (n === null) {
      return null;
    }
    if (n === childNode) {
      return i;
    }
    n = n.nextSibling;
    ++i;
  }
}

function splitTextNode(node, index, window) {
  let text = node.textContent;
  let leftText = text.substr(0, index);
  if (leftText === "") {
    throw new Error("Cannot split node at beginning");
  }
  let rightText = text.substr(index);
  if (rightText === "") {
    throw new Error("Cannot split node at end");
  }
  let rightNode = window.document.createTextNode(rightText);

  node.parentNode.insertBefore(rightNode, node.nextSibling);
  node.textContent = leftText;

  return rightNode;
}

function wrapNodes(wrapperElement, firstChildNode, lastChildNode) {
  lastChildNode.parentNode.insertBefore(
    wrapperElement,
    lastChildNode.nextSibling
  );
  for (let n = firstChildNode; n !== null; ) {
    let next = n.nextSibling;
    wrapperElement.appendChild(n);
    if (n === lastChildNode) {
      break;
    }
    n = next;
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
