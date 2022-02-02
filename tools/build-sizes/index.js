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
    let canvas = document.createElement("canvas");

    function datasetColor(datasetIndex) {
      return `hsl(${datasetIndex * 360 / (series.length + 1)}, 60%, 50%)`;
    }

    function datasetLightenedColor(datasetIndex) {
      return `hsla(${datasetIndex * 360 / (series.length + 1)}, 60%, 50%, 0.1)`;
    }

    let datasets = series.map((s, i) => {
      let color = datasetColor(i);
      return {
        label: s.name[s.name.length - 1],
        data: s.sizes.map(cell => cell.size),
        backgroundColor: color,
        borderColor: color,
      };
    });
    let labels = series[0].sizes.map(cell => formatCommit(cell.commit));

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

    function updateLegendSelection(selectedDatasetIndex) {
      let changed = false;
      for (let [datasetIndex, dataset] of chart.data.datasets.entries()) {
        let color = selectedDatasetIndex === null || datasetIndex === selectedDatasetIndex ? datasetColor(datasetIndex) : datasetLightenedColor(datasetIndex);
        if (dataset.borderColor !== color || dataset.backgroundColor !== color) {
          dataset.borderColor = color;
          dataset.backgroundColor = color;
          changed = true;
        }
      };
      if (changed) {
        chart.update();
      }
    }
    let legendElement = document.createElement("ul");
    legendElement.addEventListener("mousemove", (event) => {
      let hoveredDatasetIndex = event.target.dataset.datasetIndex;
      updateLegendSelection(hoveredDatasetIndex === undefined ? null : parseInt(hoveredDatasetIndex, 10));
    });
    legendElement.addEventListener("mouseleave", (event) => {
      updateLegendSelection(null);
    });
    legendElement.classList.add("legend");
    let legendPlugin = {
      id: 'quick-lint-js-legend',
      afterUpdate(chart, args, options) {
        legendElement.innerHTML = '';
        let labels = chart.options.plugins.legend.labels.generateLabels(chart);
        for (let [datasetIndex, label] of labels.entries()) {
          let labelElement = document.createElement('li');
          labelElement.dataset.datasetIndex = datasetIndex;
          labelElement.classList.toggle("hidden", label.hidden);
          labelElement.textContent = label.text;
          labelElement.style.color = label.fillStyle;
          labelElement.addEventListener("click", (_event) => {
            chart.setDatasetVisibility(label.datasetIndex, !chart.isDatasetVisible(label.datasetIndex));
            chart.update();
          });
          legendElement.appendChild(labelElement);
        }
      },
    };

    let chart = new Chart(canvas.getContext("2d"), {
      type: 'line',
      data: {
        datasets: datasets,
        labels: labels,
      },
      options: {
        maintainAspectRatio: false,
        animation: false,
        interaction: {
          axis: 'x',
          intersect: false,
          mode: 'nearest',
        },
        scales: {
          x: {
            grid: {
              display: false,
            },
            ticks: {
              minRotation: 45,
              maxRotation: 45,
            },
          },
        },
        radius: 0,
        showLines: false,
        plugins: {
          title: {
            display: true,
            text: title,
            font: {
              size: 24,
            },
          },
          legend: {
            // We instead render the legend ourselves with legendPlugin.
            display: false,
          },
        },
      },
      plugins: [legendPlugin],
    });

    let canvasWrapperElement = document.createElement("div");
    canvasWrapperElement.appendChild(canvas);

    let chartID = `chart-${++lastChartID}`;
    let containerElement = document.createElement("div");
    containerElement.classList.add("binary-size-chart");
    containerElement.appendChild(canvasWrapperElement);
    containerElement.appendChild(legendElement);
    containerElement.id = chartID;
    document.body.appendChild(containerElement);

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
