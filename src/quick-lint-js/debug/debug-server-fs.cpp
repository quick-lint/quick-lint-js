// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/feature.h>

#if QLJS_FEATURE_DEBUG_SERVER

#include <quick-lint-js/debug/debug-server-fs.h>
#include <quick-lint-js/port/char8.h>

namespace quick_lint_js {
string8_view debug_server_index_html =
    u8R"html(<!DOCTYPE html>
<style>
#vector-profile-data {
  display: flex;
  flex-wrap: wrap;
}

.max-size-histogram {
  margin: 0.5rem;
  max-size: 33%;
}

.histogram table {
  width: 100%;
}

.histogram td {
  --bar-color: #eecccc;
  --histogram-percentage: 0%;
  background: linear-gradient(
    90deg,
    var(--bar-color) 0%,
    var(--bar-color) var(--histogram-percentage),
    transparent var(--histogram-percentage),
    transparent 100%);
}
</style>

<h1>quick-lint-js debug server</h1>

<section id="vector-profile">
<h2>vector profile</h2>
<div id="vector-profile-data"></div>
</section>

<script type="module">
class VectorProfileView {
  constructor(element) {
    this.element = element;
    this.maxSizeHistogramElementByOwner = new Map();
  }

  updateMaxSizeHistogram({owner, countBySize}) {
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

    let totalCount = countBySize.reduce((x, acc) => x + acc, 0);
    let maxCount = Math.max(...countBySize);

    let tableBodyEl = el.querySelector("table tbody");
    for (let size = 0; size < countBySize.length; ++size) {
      let count = countBySize[size];

      let rowEl = tableBodyEl.children[size];
      if (rowEl === undefined) {
        rowEl = document.createElement("tr");
        let rowHeaderEl = document.createElement("th");
        rowHeaderEl.textContent = `${size}`;
        rowEl.appendChild(rowHeaderEl);
        rowEl.appendChild(document.createElement("td"));
        tableBodyEl.appendChild(rowEl);
      }
      let rowDataEl = rowEl.querySelector("td");
      rowDataEl.textContent = `${(count / totalCount * 100).toFixed(1)}%`;
      rowDataEl.style.setProperty("--histogram-percentage", `${count / maxCount * 100}%`);
    }
  }
}

let vectorProfileView = new VectorProfileView(document.getElementById("vector-profile-data"));

pollVectorProfileDataContinuouslyAsync()
  .catch((e) => { console.error(e); });

async function pollVectorProfileDataContinuouslyAsync() {
  for (;;) {
    await pollVectorProfileDataAsync();
    await sleepAsync(1000);
  }
}

async function pollVectorProfileDataAsync() {
  let data = await (await fetch("/vector-profiler-stats")).json();
  let maxSizeHistogramByOwner = data.maxSizeHistogramByOwner;
  for (let owner in maxSizeHistogramByOwner) {
    if (!Object.prototype.hasOwnProperty.call(maxSizeHistogramByOwner, owner)) {
      continue;
    }
    let countBySize = maxSizeHistogramByOwner[owner];
    vectorProfileView.updateMaxSizeHistogram({owner, countBySize});
  }
}

function sleepAsync(durationMilliseconds) {
  return new Promise((resolve, _reject) => {
    setTimeout(() => { resolve(); }, durationMilliseconds);
  });
}

</script>
)html"_sv;
}

#endif

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
