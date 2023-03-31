// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import assert from "assert";
import ejs from "ejs";
import fs from "fs";
import { html } from "../../src/html-tag.mjs";

export function makeBenchmarkHTML(seriess) {
  let maxMS = Math.max(...seriess.map((series) => series.maxMS));

  let linearTickCount = 7; // Excluding the 0 tick.
  let msPerLinearTick = Math.ceil(maxMS / linearTickCount / 25) * 25;
  let linearTicksMS = [];
  for (let i = 0; i < linearTickCount; ++i) {
    linearTicksMS.push((i + 1) * msPerLinearTick);
  }
  assert.strictEqual(linearTicksMS.length, linearTickCount);

  let chartMaxMS = linearTicksMS[linearTicksMS.length - 1];
  let logTicksMS = [0.1, 1, 10, 100, chartMaxMS];

  let qljsSeries = seriess.find((series) => series.name === "quick-lint-js");

  return html`
    <div style="--chart-max-x: ${chartMaxMS.toFixed(3)};" class="chart linear">
      <span class="tick" style="--x: 0;">
        <span class="label linear-only">0 ms</span>
        <span class="marker"></span>
      </span>
      <span class="tick lower" style="--x: 16.66666;">
        <span class="label" title="16.67 ms">60 FPS</span>
        <span class="marker"></span>
      </span>

      ${linearTicksMS.map(
        (tickMS) => html`
          <span class="tick linear-only" style="--x: ${tickMS.toFixed(3)};">
            <span class="label">${tickMS} ms</span>
            <span class="marker"></span>
          </span>
        `
      )}
      ${logTicksMS.map(
        (tickMS) => html`
          <span class="tick log-only" style="--x: ${tickMS.toFixed(3)};">
            <span class="label">${tickMS} ms</span>
            <span class="marker"></span>
          </span>
        `
      )}
      ${seriess.map(
        (series) => html`
          <div
            data-series="${ejs.escapeXML(series.name)}"
            style="--hue: ${series.hue.toFixed(
              1
            )}; --average-x: ${series.avgMS.toFixed(
              3
            )}; --min-x: ${series.minMS.toFixed(
              3
            )}; --max-x: ${series.maxMS.toFixed(3)};"
            class="series"
          >
            <span class="label">${ejs.escapeXML(series.name)}</span>
            <span class="bar"></span>
            <span class="error-bars"></span>
          </div>
        `
      )}

      <div class="x-axis-label">response time (lower is better)</div>
      <label
        class="log-scale javascript-only"
        title="checked: logarithmic X axis (emphasizing ratios).
unchecked: linear X axis (emphasizing time)."
        ><input type="checkbox" class="log-scale-select" /> log scale</label
      >
    </div>

    <details class="benchmark-table">
      <summary>Results data table</summary>
      <table>
        <thead>
          <tr>
            <th rowspan="2">linter</th>
            <th colspan="4">response time (milliseconds)</th>
          </tr>
          <tr>
            <th class="numbers unimportant">
              <abbr title="minimum">min</abbr>
            </th>
            <th class="numbers"><abbr title="average">avg</abbr></th>
            <th class="numbers unimportant">
              <abbr title="maximum">max</abbr>
            </th>
            <th class="numbers">
              <abbr title="slowdown compared to">÷</abbr>

              <span style="--hue: 0;" class="linter-name"
                ><abbr title="quick-lint-js" style="display: none;">qljs</abbr
                ><span class="unabbreviated">quick-lint-js</span></span
              >
            </th>
          </tr>
        </thead>
        <tbody>
          ${seriess.map(
            (series) => html`
              <tr
                style="--hue: ${series.hue.toFixed(1)};"
                data-series="${ejs.escapeXML(series.name)}"
              >
                <th>${ejs.escapeXML(series.name)}</th>
                <td class="unimportant">${series.minMS.toFixed(2)}</td>
                <td>${series.avgMS.toFixed(2)}</td>
                <td class="unimportant">${series.maxMS.toFixed(2)}</td>
                <td>${(series.avgMS / qljsSeries.avgMS).toFixed(1)}×</td>
              </tr>
            `
          )}
        </tbody>
      </table>
    </details>
  `;
}

export function parseBenchmarkLSPJSON(benchmarkLSPJSONFile, seriesOptions) {
  let benchmarkResults = JSON.parse(
    fs.readFileSync(benchmarkLSPJSONFile, "utf-8")
  );
  let seriess = [];
  for (let series of benchmarkResults.data) {
    let name = series.benchmarkName.split("/")[0];
    let durations = series.samples.durationPerIteration;
    let mean = average(durations);
    let min = Math.min(...durations);
    let max = Math.max(...durations);
    seriess.push(
      new Series({
        name: name,
        ...seriesOptions[name],
        minMS: min * 1000,
        avgMS: mean * 1000,
        maxMS: max * 1000,
      })
    );
  }
  seriess.sort((a, b) => {
    if (a.avgMS < b.avgMS) return -1;
    if (a.avgMS > b.avgMS) return +1;
    return 0;
  });
  return seriess;
}

function average(numbers) {
  return numbers.reduce((a, b) => a + b, 0) / numbers.length;
}

class Series {
  constructor({ name, minMS, avgMS, maxMS, hue }) {
    if (typeof hue === "undefined") {
      throw new Error(`Missing hue for series: ${name}`);
    }

    this.name = name;
    this.minMS = minMS;
    this.avgMS = avgMS;
    this.maxMS = maxMS;
    this.hue = hue;
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
