// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

class Benchmark {
  constructor(rootElement) {
    this.chartElement = rootElement.querySelector(".chart");
    this.tableElement = rootElement.querySelector(".benchmark-table");
    this.logScaleCheckboxElement =
      this.chartElement.querySelector(".log-scale-select");
  }

  setUp() {
    this.saveDataInAttributes();
    this.setUpLogScaleButton();
    this.setUpFitOnResize();
    this.setUpSelection();
    this.setUpInitialLogScale();
  }

  saveDataInAttributes() {
    function saveCSSVariableInDataAttribute(
      element,
      variableName,
      attributeName
    ) {
      let style = window.getComputedStyle(element);
      let value = style.getPropertyValue(variableName);
      element.dataset[attributeName] = value;
    }
    for (let seriesElement of this.chartElement.querySelectorAll(".series")) {
      saveCSSVariableInDataAttribute(seriesElement, "--min-x", "minX");
      saveCSSVariableInDataAttribute(seriesElement, "--max-x", "maxX");
      saveCSSVariableInDataAttribute(seriesElement, "--average-x", "averageX");
    }
    for (let tickElement of this.chartElement.querySelectorAll(".tick")) {
      saveCSSVariableInDataAttribute(tickElement, "--x", "x");
    }
  }

  setUpLogScaleButton() {
    this.logScaleCheckboxElement.addEventListener("change", () => {
      // Measure bars to determine label fit.
      this.disableAnimations();
      this.updateChartScale({
        isLogScale: this.logScaleCheckboxElement.checked,
      });
      let fitLabels = this.prepareLabelFit();

      // Undo changes to bars in order to animate them.
      this.updateChartScale({
        isLogScale: !this.logScaleCheckboxElement.checked,
      });

      // Animate everything.
      this.enableAnimations();
      fitLabels();
      this.updateChartScale({
        isLogScale: this.logScaleCheckboxElement.checked,
      });
    });
  }

  setUpFitOnResize() {
    if (typeof window.ResizeObserver === "undefined") {
      window.addEventListener("resize", () => {
        this.fitLabels();
      });
    } else {
      new window.ResizeObserver(() => {
        this.fitLabels();
      }).observe(this.chartElement);
    }
  }

  setUpSelection() {
    for (let seriesElement of this.chartElement.querySelectorAll(".series")) {
      seriesElement.addEventListener("mouseover", () => {
        this.updateSelection(seriesElement);
      });
      seriesElement.addEventListener("mouseout", () => {
        this.updateSelection(null);
      });
    }
    for (let rowElement of this.tableElement.querySelectorAll("tbody tr")) {
      rowElement.addEventListener("mouseover", () => {
        this.updateSelection(rowElement);
      });
      rowElement.addEventListener("mouseout", () => {
        this.updateSelection(null);
      });
    }
  }

  setUpInitialLogScale() {
    this.disableAnimations();
    this.updateChartScale({ isLogScale: this.logScaleCheckboxElement.checked });
    this.fitLabels();
    this.enableAnimations();
  }

  // Measure labels and the bars in the chart. Figure out which labels fit
  // within the bar, and which labels must be placed outside the bar.
  //
  // prepareLabelFit returns a function which moves the labels inside or outside
  // the bar based on these measurements.
  prepareLabelFit() {
    let work = [];
    this.chartElement.classList.toggle("fitting", true);
    for (let seriesElement of this.chartElement.querySelectorAll(".series")) {
      let barLabelElement = seriesElement.querySelector(".label");
      let barElement = seriesElement.querySelector(".bar");
      let labelFitsInBar =
        barElement.clientWidth >= barLabelElement.clientWidth;
      work.push({ barLabelElement, labelFitsInBar });
    }
    this.chartElement.classList.toggle("fitting", false);
    return () => {
      for (let { barLabelElement, labelFitsInBar } of work) {
        barLabelElement.classList.toggle("no-fit", !labelFitsInBar);
      }
    };
  }

  fitLabels() {
    this.prepareLabelFit()();
  }

  updateChartScale({ isLogScale }) {
    let chartMaxX = parseFloat(
      window
        .getComputedStyle(this.chartElement)
        .getPropertyValue("--chart-max-x")
    );
    let seriesElements = [...this.chartElement.querySelectorAll(".series")];

    let minX = Math.min(
      ...seriesElements.map((element) => parseFloat(element.dataset.minX))
    );
    let maxX = Math.max(
      ...seriesElements.map((element) => parseFloat(element.dataset.maxX))
    );

    function updateProperty(element, propertyName, attributeName) {
      let value = parseFloat(element.dataset[attributeName]);
      let newValue = isLogScale ? logScale(value) : value;
      element.style.setProperty(propertyName, newValue);
    }
    function logScale(x) {
      if (x === 0) {
        return 0;
      }
      let logMinX = minX / 10;
      // logMaxX is solved from 'logScale(maxX) = maxX'.
      let logMaxX =
        Math.pow(10, chartMaxX / maxX - 1) *
        Math.pow(maxX / minX, chartMaxX / maxX) *
        minX;
      return (
        (Math.log10(x / logMinX) / Math.log10(logMaxX / logMinX)) * chartMaxX
      );
    }

    for (let seriesElement of seriesElements) {
      updateProperty(seriesElement, "--average-x", "averageX");
      updateProperty(seriesElement, "--min-x", "minX");
      updateProperty(seriesElement, "--max-x", "maxX");
    }
    for (let tickElement of this.chartElement.querySelectorAll(".tick")) {
      updateProperty(tickElement, "--x", "x");
    }

    this.chartElement.classList.toggle("linear", !isLogScale);
    this.chartElement.classList.toggle("log", isLogScale);
  }

  updateSelection(selectedElement) {
    function getSeries(element) {
      if (element === null) {
        return null;
      }
      return element.dataset.series;
    }

    let seriesToSelect = getSeries(selectedElement);
    for (let seriesElement of this.chartElement.querySelectorAll(".series")) {
      seriesElement.classList.toggle(
        "selected",
        getSeries(seriesElement) === seriesToSelect
      );
    }
    for (let rowElement of this.tableElement.querySelectorAll("tbody tr")) {
      rowElement.classList.toggle(
        "selected",
        getSeries(rowElement) === seriesToSelect
      );
    }

    this.fitLabels();
  }

  enableAnimations() {
    triggerCSSTransitions(this.chartElement);
    this.chartElement.classList.toggle("disable-animations", false);
  }

  disableAnimations() {
    this.chartElement.classList.toggle("disable-animations", true);
  }
}

function triggerCSSTransitions(element) {
  element.clientHeight;
}

for (let benchmarkElement of document.querySelectorAll(".benchmark")) {
  let benchmark = new Benchmark(benchmarkElement);
  benchmark.setUp();
}

document.body.classList.add("javascript");

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
