// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

import assert from "assert";
import ejs from "ejs";
import fs from "fs";

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
        title="checked: logarithmic X axis (emphasizing ratios).&#013;unchecked: linear X axis (emphasizing time)."
        ><input type="checkbox" class="log-scale-select" /> log scale</label
      >
    </div>

    <table class="benchmark-table">
      <thead>
        <tr>
          <th rowspan="2">linter</th>
          <th colspan="4">response time (milliseconds)</th>
        </tr>
        <tr>
          <th class="numbers"><abbr title="minimum">min</abbr></th>
          <th class="numbers"><abbr title="average">avg</abbr></th>
          <th class="numbers"><abbr title="maximum">max</abbr></th>
          <th class="numbers">
            <abbr title="slowdown compared to">÷</abbr>
            <span style="--hue: 0;" class="linter-name">quick-lint-js</span>
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
              <td>${series.minMS.toFixed(2)}</td>
              <td>${series.avgMS.toFixed(2)}</td>
              <td>${series.maxMS.toFixed(2)}</td>
              <td>${(series.avgMS / qljsSeries.avgMS).toFixed(1)}×</td>
            </tr>
          `
        )}
      </tbody>
    </table>
  `;
}

export function parseCriterionJSON(criterionJSONFile) {
  let criterion = JSON.parse(fs.readFileSync(criterionJSONFile, "utf-8"));
  let seriess = [];
  for (let series of criterion[2]) {
    let name = series.reportName.split("/")[0];
    let meanData = series.reportAnalysis.anMean;
    let mean = meanData.estPoint;
    let min = mean - meanData.estError.confIntLDX;
    let max = mean + meanData.estError.confIntUDX;
    seriess.push(
      new Series({
        name: name,
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

class Series {
  constructor({ name, minMS, avgMS, maxMS }) {
    this.name = name;
    this.minMS = minMS;
    this.avgMS = avgMS;
    this.maxMS = maxMS;
  }

  get hue() {
    let hues = {
      "quick-lint-js": 0.0,
      "eslint-server": 60.0,
      RSLint: 120.0,
      Flow: 180.0,
      Deno: 240.0,
      "TypeScript-Theia": 300.0,
    };
    return hues[this.name];
  }
}

function html(strings, ...expressions) {
  assert.strictEqual(strings.length, expressions.length + 1);
  let out = [];
  for (let i = 0; i < expressions.length; ++i) {
    out.push(strings[i]);
    if (Array.isArray(expressions[i])) {
      out.push(...expressions[i]);
    } else {
      out.push(expressions[i]);
    }
  }
  out.push(strings[expressions.length]);
  return out.join("");
}

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
