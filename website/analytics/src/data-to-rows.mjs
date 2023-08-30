// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

export function dataToRows(keyArrays, valueArrays) {
  let seriesCount = keyArrays.length;
  let rowIndices = Array(seriesCount).fill(0);
  let rows = [];
  for (;;) {
    let minimumKey = Infinity;
    for (let seriesIndex = 0; seriesIndex < seriesCount; ++seriesIndex) {
      let keyIndex = rowIndices[seriesIndex];
      if (keyIndex < keyArrays[seriesIndex].length) {
        let key = keyArrays[seriesIndex][keyIndex];
        if (key < minimumKey) {
          minimumKey = key;
        }
      }
    }
    if (minimumKey === Infinity) {
      break;
    }

    let row = [minimumKey];
    for (let seriesIndex = 0; seriesIndex < seriesCount; ++seriesIndex) {
      let keyIndex = rowIndices[seriesIndex];
      let key = keyArrays[seriesIndex][keyIndex]; // Possibly out of bounds.
      if (key === minimumKey) {
        row.push(valueArrays[seriesIndex][keyIndex]);
        rowIndices[seriesIndex] = keyIndex + 1;
      } else {
        row.push(0);
      }
    }
    rows.push(row);
  }
  return rows;
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
