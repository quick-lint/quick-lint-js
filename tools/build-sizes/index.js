// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

(() => {
  async function mainAsync() {
    let allSeries;
    try {
      allSeries = await loadBuildSizeDataAsync();
    } catch (error) {
      document.getElementById(
        "load-error"
      ).innerText = `Loading data.json failed: ${error}`;
      throw error;
    }
    await loadGoogleChartsAsync();

    let exeSeries = allSeries.filter((s) => s.type === "exe");
    makeChart(exeSeries, {
      title: "executables",
    });
    makeChart(
      allSeries.filter((s) => s.type === "archive"),
      {
        title: "archives",
      }
    );
    makeChart(
      allSeries.filter((s) => s.type === "file"),
      {
        title: "misc files",
      }
    );

    for (let currentSeries of exeSeries) {
      let seriesForThisExecutable = allSeries.filter(
        (s) =>
          s.type === "section" &&
          arraysAreEqual(s.name.slice(0, s.name.length - 1), currentSeries.name)
      );
      if (seriesForThisExecutable.length > 0) {
        makeChart(seriesForThisExecutable, {
          subtitle: "sections",
          stripLastNameFromTitle: true,
        });
      }
    }
  }

  async function loadGoogleChartsAsync() {
    google.charts.load("current", { packages: ["corechart"] });
    await new Promise((resolve, reject) => {
      google.charts.setOnLoadCallback(resolve);
    });
  }

  async function loadBuildSizeDataAsync() {
    let response = await fetch("data.json");
    if (!response.ok) {
      throw new Error(`${response.url}: ${response.statusText}`);
    }
    let data = await response.json();
    return data;
  }

  let lastChartID = 0;

  function makeChart(
    series,
    { title = null, subtitle = null, stripLastNameFromTitle = false } = {}
  ) {
    let seriesMinMax = series.map((s) => {
      let sizes = s.sizes.map((size) => size.size);
      return [Math.min(...sizes), Math.max(...sizes)];
    });
    let chartHeight = Math.max(...seriesMinMax.map(([min, max]) => max - min));

    let data = new google.visualization.DataTable();
    data.addColumn("string", "Commit");
    for (let s of series) {
      data.addColumn("number", s.name[s.name.length - 1]);
    }
    for (let i = 0; i < series[0].sizes.length; ++i) {
      let row = [formatCommit(series[0].sizes[i].commit)];
      for (let s of series) {
        row.push(s.sizes[i].size);
      }
      data.addRows([row]);
    }

    if (title === null) {
      title = formatSeriesName(
        stripLastNameFromTitle
          ? series[0].name.slice(0, series[0].name.length - 1)
          : series[0].name
      );
    }
    if (subtitle) {
      title += ` (${subtitle})`;
    }
    let options = {
      title: title,
      legend: { position: "bottom" },
      series: series.map((_s, index) => ({ targetAxisIndex: index })),
      vAxes: seriesMinMax.map(([min, max]) => ({
        minValue: min,
        maxValue: min + chartHeight,
      })),
      vAxis: { textPosition: "none" },
    };

    let chartID = `chart-${++lastChartID}`;

    let chartElement = document.createElement("div");
    chartElement.classList.add("binary-size-chart");
    chartElement.id = chartID;
    document.body.appendChild(chartElement);
    let chart = new google.visualization.LineChart(chartElement);
    chart.draw(data, options);

    let chartIndexLinkElement = document.createElement("a");
    chartIndexLinkElement.href = `#${chartID}`;
    chartIndexLinkElement.innerText = title;
    let chartIndexElement = document.createElement("li");
    chartIndexElement.appendChild(chartIndexLinkElement);
    document.getElementById("chart-index").appendChild(chartIndexElement);
  }

  function formatCommit(commit) {
    return commit.substr(0, 10);
  }

  function formatSeriesName(name) {
    return name.filter((name) => name !== ".").join(" ");
  }

  function arraysAreEqual(xs, ys) {
    if (xs.length !== ys.length) return false;
    for (let i = 0; i < xs.length; ++i) {
      if (xs[i] !== ys[i]) return false;
    }
    return true;
  }

  mainAsync().catch((error) => {
    document.body.classList.add("error");
    return Promise.reject(error);
  });
})();

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
