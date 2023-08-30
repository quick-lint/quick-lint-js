// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import { dataToRows } from "../src/data-to-rows.mjs";

describe("dataToRows", () => {
  it("two inputs with same key arrays", () => {
    expect(
      dataToRows(
        [
          [1, 2, 3],
          [1, 2, 3],
        ],
        [
          [100, 200, 300],
          [11, 22, 33],
        ]
      )
    ).toEqual([
      [1, 100, 11],
      [2, 200, 22],
      [3, 300, 33],
    ]);
  });

  it("three inputs with same key arrays", () => {
    expect(
      dataToRows(
        [
          [1, 2, 3],
          [1, 2, 3],
          [1, 2, 3],
        ],
        [
          [100, 200, 300],
          [11, 22, 33],
          [9, 8, 7],
        ]
      )
    ).toEqual([
      [1, 100, 11, 9],
      [2, 200, 22, 8],
      [3, 300, 33, 7],
    ]);
  });

  it("one input with more keys at end", () => {
    expect(
      dataToRows(
        [
          [1, 2, 3],
          [1, 2, 3, 4, 5],
        ],
        [
          [100, 200, 300],
          [11, 22, 33, 44, 55],
        ]
      )
    ).toEqual([
      [1, 100, 11],
      [2, 200, 22],
      [3, 300, 33],
      [4, 0, 44],
      [5, 0, 55],
    ]);

    expect(
      dataToRows(
        [
          [1, 2, 3, 4, 5],
          [1, 2, 3],
        ],
        [
          [100, 200, 300, 400, 500],
          [11, 22, 33],
        ]
      )
    ).toEqual([
      [1, 100, 11],
      [2, 200, 22],
      [3, 300, 33],
      [4, 400, 0],
      [5, 500, 0],
    ]);
  });

  it("no common keys, interleaved", () => {
    expect(
      dataToRows(
        [
          [1, 3, 5],
          [2, 4, 6],
        ],
        [
          [100, 300, 500],
          [22, 44, 66],
        ]
      )
    ).toEqual([
      [1, 100, 0],
      [2, 0, 22],
      [3, 300, 0],
      [4, 0, 44],
      [5, 500, 0],
      [6, 0, 66],
    ]);
  });
});

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
