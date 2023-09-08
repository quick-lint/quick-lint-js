#!/usr/bin/env node
// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "node:fs";
import path from "node:path";
import url from "node:url";
import { AnalyticsDB } from "./analytics-db.mjs";
import { dataToRows } from "./data-to-rows.mjs";
import { loadConfigAsync } from "./config.mjs";

let __filename = url.fileURLToPath(import.meta.url);
let __dirname = path.dirname(__filename);

async function mainAsync() {
  let config = await loadConfigAsync();
  let db = AnalyticsDB.fromFile(config["db.file"]);

  let outputDirectory = config["chart.directory"];
  await fs.promises.mkdir(outputDirectory, { recursive: true });

  let dailyWebDownloadersLabels = [];
  let dailyWebDownloadersKeys = [];
  let dailyWebDownloadersValues = [];

  let weeklyWebDownloadersLabels = [];
  let weeklyWebDownloadersKeys = [];
  let weeklyWebDownloadersValues = [];

  let allURLs = db.getWebDownloadedURLs();

  for (let { groupName, urls } of [
    {
      groupName: "Debian apt-update",
      urls: ["https://c.quick-lint-js.com/debian/dists/experimental/InRelease"],
    },
    {
      groupName: "Debian",
      urls: allURLs.filter((url) =>
        /\/debian\/pool\/.*\/quick-lint-js.*\.deb$/.test(url)
      ),
    },
    {
      groupName: "sources",
      urls: allURLs.filter((url) =>
        /\/releases\/.*\/source\/quick-lint-js.*\.tar.*$/.test(url)
      ),
    },
    {
      groupName: "manual builds",
      urls: allURLs.filter((url) =>
        /\/releases\/.*\/manual\/.*\.(zip|tar.*)$/.test(url)
      ),
    },
    {
      groupName: "winget",
      urls: allURLs.filter((url) =>
        /\/releases\/.*\/windows\/.*\.msix$/.test(url)
      ),
    },
    {
      groupName: "Vim ZIP",
      urls: allURLs.filter((url) => /\/releases\/.*\/vim\/.*\.zip$/.test(url)),
    },
  ]) {
    {
      let r = db.countDailyWebDownloaders(urls);
      dailyWebDownloadersLabels.push(groupName);
      dailyWebDownloadersKeys.push(r.dates);
      dailyWebDownloadersValues.push(r.counts);
    }

    {
      let r = db.countWeeklyWebDownloaders(urls);
      weeklyWebDownloadersLabels.push(groupName);
      weeklyWebDownloadersKeys.push(r.dates);
      weeklyWebDownloadersValues.push(r.counts);
    }
  }

  let dailyVSCodeDownloads = db.countDailyVSCodeDownloads();
  dailyWebDownloadersLabels.push("VS Code");
  dailyWebDownloadersKeys.push(dailyVSCodeDownloads.dates);
  dailyWebDownloadersValues.push(dailyVSCodeDownloads.counts);

  let weeklyVSCodeDownloads = db.countWeeklyVSCodeDownloads();
  weeklyWebDownloadersLabels.push("VS Code");
  weeklyWebDownloadersKeys.push(weeklyVSCodeDownloads.dates);
  weeklyWebDownloadersValues.push(weeklyVSCodeDownloads.counts);

  let data = {
    dailyWebDownloaders: {
      labels: dailyWebDownloadersLabels,
      rows: dataToRows(dailyWebDownloadersKeys, dailyWebDownloadersValues),
    },
    weeklyWebDownloaders: {
      labels: weeklyWebDownloadersLabels,
      rows: dataToRows(weeklyWebDownloadersKeys, weeklyWebDownloadersValues),
    },
  };

  db.close();

  await fs.promises.writeFile(
    path.join(outputDirectory, "data.json"),
    JSON.stringify(data, null, 2)
  );
  await fs.promises.copyFile(
    path.join(__dirname, "chart.html"),
    path.join(outputDirectory, "index.html")
  );
}

mainAsync().catch((e) => {
  console.error(e?.stack || e);
  process.exit(1);
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
