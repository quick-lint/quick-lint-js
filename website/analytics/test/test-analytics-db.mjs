// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { AnalyticsDB } from "../src/analytics-db.mjs";

describe("AnalyticsDB", () => {
  describe("file", () => {
    let tempDir;
    beforeEach(() => {
      tempDir = fs.mkdtempSync(os.tmpdir() + path.sep);
    });
    afterEach(() => {
      fs.rmSync(tempDir, { recursive: true });
    });

    it("recreate does not fail", () => {
      let dbPath = path.join(tempDir, "db");
      let db1 = AnalyticsDB.fromFile(dbPath);
      db1.close();
      // Shouldn't crash with a duplicate table exception, for example.
      let db2 = AnalyticsDB.fromFile(dbPath);
      db2.close();
    });
  });

  describe("addWebDownload", () => {
    it("add new web download", () => {
      let db = AnalyticsDB.newInMemory();
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 12, 14),
        url: "https://example.com/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "CoolBrowser version 1.0",
      });
      expect(db.getWebDownloadedURLs()).toEqual(["https://example.com/"]);
      expect(db.countDailyWebDownloaders(["https://example.com/"])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0)],
        counts: [1],
      });
      expect(db.countDailyWebDownloads(["https://example.com/"])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0)],
        counts: [1],
      });
    });

    it("completely identical web downloads are deduplicated", () => {
      let db = AnalyticsDB.newInMemory();
      for (let i = 0; i < 3; ++i) {
        db.addWebDownload({
          timestamp: Date.UTC(2020, 0, 1, 10, 12, 14),
          url: "https://example.com/",
          downloaderIP: "192.168.1.1",
          downloaderUserAgent: "CoolBrowser version 1.0",
        });
      }
      expect(db.getWebDownloadedURLs()).toEqual(["https://example.com/"]);
      expect(db.countDailyWebDownloads(["https://example.com/"])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0)],
        counts: [1],
      });
    });

    it("adding download with null user agent deduplicates into existing user agents", () => {
      let db = AnalyticsDB.newInMemory();
      for (let i = 0; i < 3; ++i) {
        db.addWebDownload({
          timestamp: Date.UTC(2020, 0, 1, 10, 12, 14),
          url: "https://example.com/",
          downloaderIP: "192.168.1.1",
          downloaderUserAgent: `ua${i}`,
        });
      }
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 12, 14),
        url: "https://example.com/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: null,
      });
      expect(db.countDailyWebDownloads(["https://example.com/"])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0)],
        counts: [3],
      });
    });

    it("adding download deduplicates into existing download with null user agent", () => {
      let db = AnalyticsDB.newInMemory();
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 12, 14),
        url: "https://example.com/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: null,
      });
      for (let i = 0; i < 3; ++i) {
        db.addWebDownload({
          timestamp: Date.UTC(2020, 0, 1, 10, 12, 14),
          url: "https://example.com/",
          downloaderIP: "192.168.1.1",
          downloaderUserAgent: `ua${i}`,
        });
      }
      expect(db.countDailyWebDownloads(["https://example.com/"])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0)],
        counts: [3],
      });
    });

    it("URLs are deduplicated", () => {
      let db = AnalyticsDB.newInMemory();
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 12, 14),
        url: "https://example.com/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "CoolBrowser version 1.0",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2021, 0, 1, 10, 12, 14),
        url: "https://example.com/",
        downloaderIP: "192.168.1.2",
        downloaderUserAgent: "CoolBrowser version 2.0",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2022, 0, 1, 10, 12, 14),
        url: "https://example.com/other/",
        downloaderIP: "192.168.1.3",
        downloaderUserAgent: "CoolBrowser version 3.0",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2023, 0, 1, 10, 12, 14),
        url: "https://example.com/other/",
        downloaderIP: "192.168.1.4",
        downloaderUserAgent: "CoolBrowser version 4.0",
      });
      expect(db.getWebDownloadedURLs()).toEqual([
        "https://example.com/",
        "https://example.com/other/",
      ]);
    });

    describe("batch insert", () => {
      it("URLs are deduplicated", () => {
        let db = AnalyticsDB.newInMemory();
        db.addWebDownloadBatch([
          {
            timestamp: Date.UTC(2020, 0, 1, 10, 12, 14),
            url: "https://example.com/",
            downloaderIP: "192.168.1.1",
            downloaderUserAgent: "CoolBrowser version 1.0",
          },
          {
            timestamp: Date.UTC(2021, 0, 1, 10, 12, 14),
            url: "https://example.com/",
            downloaderIP: "192.168.1.2",
            downloaderUserAgent: "CoolBrowser version 2.0",
          },
          {
            timestamp: Date.UTC(2022, 0, 1, 10, 12, 14),
            url: "https://example.com/other/",
            downloaderIP: "192.168.1.3",
            downloaderUserAgent: "CoolBrowser version 3.0",
          },
          {
            timestamp: Date.UTC(2023, 0, 1, 10, 12, 14),
            url: "https://example.com/other/",
            downloaderIP: "192.168.1.4",
            downloaderUserAgent: "CoolBrowser version 4.0",
          },
        ]);
        expect(db.getWebDownloadedURLs()).toEqual([
          "https://example.com/",
          "https://example.com/other/",
        ]);
      });
    });
  });

  describe("countDailyWebDownloaders", () => {
    it("multiple downloaders on multiple days", () => {
      let db = AnalyticsDB.newInMemory();

      let url = "https://example.com/";

      // Second day:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 2, 8, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua1",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 2, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.3",
        downloaderUserAgent: "ua3",
      });

      // First day:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 8, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua1",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 9, 0, 0),
        url: url,
        downloaderIP: "192.168.1.2",
        downloaderUserAgent: "ua2",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.3",
        downloaderUserAgent: "ua3",
      });

      expect(db.countDailyWebDownloaders([url])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0), Date.UTC(2020, 0, 2, 0, 0, 0)],
        counts: [3, 2],
      });
    });

    it("multiple URLs", () => {
      let db = AnalyticsDB.newInMemory();

      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 2, 8, 0, 0),
        url: "https://example.com/a/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua1",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 2, 10, 0, 0),
        url: "https://example.com/b/",
        downloaderIP: "192.168.1.3",
        downloaderUserAgent: "ua3",
      });

      expect(
        db.countDailyWebDownloaders([
          "https://example.com/a/",
          "https://example.com/b/",
        ])
      ).toEqual({
        dates: [Date.UTC(2020, 0, 2, 0, 0, 0)],
        counts: [2],
      });
    });

    it("ignores other URLs", () => {
      let db = AnalyticsDB.newInMemory();

      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 8, 0, 0),
        url: "https://example.com/a/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 9, 0, 0),
        url: "https://example.com/b/",
        downloaderIP: "192.168.1.2",
        downloaderUserAgent: "",
      });

      expect(db.countDailyWebDownloaders(["https://example.com/a/"])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0)],
        counts: [1],
      });
    });

    it("identical downloaders for a URL are deduplicated", () => {
      let db = AnalyticsDB.newInMemory();

      let url = "https://example.com/";

      // First day:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 8, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua",
      });

      // Second day:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 2, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua",
      });

      expect(db.countDailyWebDownloaders([url])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0), Date.UTC(2020, 0, 2, 0, 0, 0)],
        counts: [1, 1],
      });
    });

    it("identical downloaders across URLs are deduplicated", () => {
      let db = AnalyticsDB.newInMemory();

      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 8, 0, 0),
        url: "https://example.com/a/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 0, 0),
        url: "https://example.com/b/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua",
      });

      expect(
        db.countDailyWebDownloaders([
          "https://example.com/a/",
          "https://example.com/b/",
        ])
      ).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0)],
        counts: [1],
      });
    });

    it("downloaders with same IP but different user agents are not deduplicated", () => {
      let db = AnalyticsDB.newInMemory();

      let url = "https://example.com/";
      let ip = "192.168.1.1";

      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 8, 0, 0),
        url: url,
        downloaderIP: ip,
        downloaderUserAgent: "ua1",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 0, 0),
        url: url,
        downloaderIP: ip,
        downloaderUserAgent: "ua2",
      });

      expect(db.countDailyWebDownloaders([url])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0)],
        counts: [2],
      });
    });

    it("downloaders with same user agent but different IPs are not deduplicated", () => {
      let db = AnalyticsDB.newInMemory();

      let url = "https://example.com/";
      let userAgent = "CoolBrowser version 1.0";

      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 8, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: userAgent,
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.2",
        downloaderUserAgent: userAgent,
      });

      expect(db.countDailyWebDownloaders([url])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0)],
        counts: [2],
      });
    });

    it("no URLs gives no results", () => {
      let db = AnalyticsDB.newInMemory();

      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 2, 8, 0, 0),
        url: "https://example.com/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua1",
      });

      expect(db.countDailyWebDownloaders([])).toEqual({
        dates: [],
        counts: [],
      });
    });
  });

  describe("countWeeklyWebDownloaders", () => {
    it("multiple downloaders on multiple weeks and days", () => {
      let db = AnalyticsDB.newInMemory();

      let url = "https://example.com/";

      // January 6, 2020 was a Monday. Monday is our start of the week.

      // First week, first day:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 6, 8, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua1",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 6, 9, 0, 0),
        url: url,
        downloaderIP: "192.168.1.2",
        downloaderUserAgent: "ua2",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 6, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.3",
        downloaderUserAgent: "ua3",
      });

      // First week, second day:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 7, 8, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua1",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 7, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.3",
        downloaderUserAgent: "ua3",
      });

      // Second week:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 13, 8, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua1",
      });

      expect(db.countWeeklyWebDownloaders([url])).toEqual({
        dates: [Date.UTC(2020, 0, 6, 0, 0, 0), Date.UTC(2020, 0, 13, 0, 0, 0)],
        counts: [3, 1],
      });
    });
  });

  describe("countDailyWebDownloads", () => {
    it("multiple downloads on multiple days", () => {
      let db = AnalyticsDB.newInMemory();

      let url = "https://example.com/";

      // Second day:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 2, 8, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua1",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 2, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.3",
        downloaderUserAgent: "ua3",
      });

      // First day:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 8, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua1",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 9, 0, 0),
        url: url,
        downloaderIP: "192.168.1.2",
        downloaderUserAgent: "ua2",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.3",
        downloaderUserAgent: "ua3",
      });

      expect(db.countDailyWebDownloads([url])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0), Date.UTC(2020, 0, 2, 0, 0, 0)],
        counts: [3, 2],
      });
    });

    it("ignores other URLs", () => {
      let db = AnalyticsDB.newInMemory();

      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 8, 0, 0),
        url: "https://example.com/a/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 9, 0, 0),
        url: "https://example.com/b/",
        downloaderIP: "192.168.1.2",
        downloaderUserAgent: "",
      });

      expect(db.countDailyWebDownloads(["https://example.com/a/"])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0)],
        counts: [1],
      });
    });

    it("identical downloaders are not deduplicated", () => {
      let db = AnalyticsDB.newInMemory();

      let url = "https://example.com/";

      // First day:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 8, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua",
      });
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 1, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua",
      });

      // Second day:
      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 2, 10, 0, 0),
        url: url,
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua",
      });

      expect(db.countDailyWebDownloads([url])).toEqual({
        dates: [Date.UTC(2020, 0, 1, 0, 0, 0), Date.UTC(2020, 0, 2, 0, 0, 0)],
        counts: [2, 1],
      });
    });

    it("no URLs gives no results", () => {
      let db = AnalyticsDB.newInMemory();

      db.addWebDownload({
        timestamp: Date.UTC(2020, 0, 2, 8, 0, 0),
        url: "https://example.com/",
        downloaderIP: "192.168.1.1",
        downloaderUserAgent: "ua1",
      });

      expect(db.countDailyWebDownloads([])).toEqual({
        dates: [],
        counts: [],
      });
    });
  });

  describe("addVSCodeDownloadStats", () => {
    it("downloads includes installs and web downloads", () => {
      let db = AnalyticsDB.newInMemory();
      db.addVSCodeDownloadStats([
        {
          version: "2.16.0",
          statisticDate: "2023-09-08T00:00:00Z",
          counts: {
            webPageViews: 33,
            installCount: 6,
            webDownloadCount: 2,
            averageRating: 5,
          },
        },
      ]);
      expect(db.countDailyVSCodeDownloads()).toEqual({
        dates: [Date.UTC(2023, 8, 8, 0, 0, 0)],
        counts: [6 + 2],
      });
    });

    it("no downloads for day counts as 0", () => {
      let db = AnalyticsDB.newInMemory();
      db.addVSCodeDownloadStats([
        {
          version: "2.16.0",
          statisticDate: "2023-09-08T00:00:00Z",
          counts: {
            webPageViews: 33,
            averageRating: 5,
          },
        },
      ]);
      expect(db.countDailyVSCodeDownloads()).toEqual({
        dates: [Date.UTC(2023, 8, 8, 0, 0, 0)],
        counts: [0],
      });
    });

    it("downloads includes all versions", () => {
      let db = AnalyticsDB.newInMemory();
      db.addVSCodeDownloadStats([
        {
          version: "2.16.0",
          statisticDate: "2023-09-08T00:00:00Z",
          counts: {
            installCount: 2,
          },
        },
        {
          version: "2.15.0",
          statisticDate: "2023-09-08T00:00:00Z",
          counts: {
            installCount: 1,
          },
        },
      ]);
      expect(db.countDailyVSCodeDownloads()).toEqual({
        dates: [Date.UTC(2023, 8, 8, 0, 0, 0)],
        counts: [2 + 1],
      });
    });

    it("weekly downloads groups by week", () => {
      let db = AnalyticsDB.newInMemory();
      db.addVSCodeDownloadStats([
        // First week:
        {
          version: "2.16.0",
          statisticDate: "2020-01-06T00:00:00Z",
          counts: {
            installCount: 3,
          },
        },
        {
          version: "2.16.0",
          statisticDate: "2020-01-07T00:00:00Z",
          counts: {
            installCount: 5,
          },
        },

        // Second week:
        {
          version: "2.16.0",
          statisticDate: "2020-01-14T00:00:00Z",
          counts: {
            installCount: 2,
          },
        },
      ]);
      expect(db.countWeeklyVSCodeDownloads()).toEqual({
        dates: [Date.UTC(2020, 0, 6, 0, 0, 0), Date.UTC(2020, 0, 13, 0, 0, 0)],
        counts: [3 + 5, 2],
      });
    });

    it("updating stats archives old stats", () => {
      let db = AnalyticsDB.newInMemory();
      db.addVSCodeDownloadStats([
        {
          version: "2.16.0",
          statisticDate: "2023-09-08T00:00:00Z",
          counts: {
            installCount: 2,
          },
        },
      ]);

      let beforeArchiveTimestamp = Date.now() / 1000;
      db.addVSCodeDownloadStats([
        {
          version: "2.16.0",
          statisticDate: "2023-09-08T00:00:00Z",
          counts: {
            installCount: 1,
          },
        },
      ]);
      let afterArchiveTimestamp = Date.now() / 1000;

      expect(db.countDailyVSCodeDownloads()).toEqual({
        dates: [Date.UTC(2023, 8, 8, 0, 0, 0)],
        counts: [1],
      });

      let archived = db._querySQLForTesting(
        `
          SELECT
            UNIXEPOCH(archive_timestamp) AS archive_timestamp,
            timestamp,
            install_count,
            version
          FROM vscode_stats_archive
        `
      );
      expect(archived.length).toEqual(1);
      // TODO(strager): Don't store string timestamp numbers in the database.
      expect(Number(archived[0].timestamp)).toEqual(
        Date.UTC(2023, 8, 8, 0, 0, 0) / 1000
      );
      expect(archived[0].version).toEqual("2.16.0");
      expect(archived[0].install_count).toEqual(2);
      // NOTE(strager): SQLite timestamp has second resolution.
      expect(archived[0].archive_timestamp).toBeGreaterThanOrEqual(
        Math.floor(beforeArchiveTimestamp)
      );
      expect(archived[0].archive_timestamp).toBeLessThanOrEqual(
        Math.ceil(afterArchiveTimestamp)
      );
    });

    it("updating stats does not archive identical stats", () => {
      let db = AnalyticsDB.newInMemory();
      db.addVSCodeDownloadStats([
        {
          version: "2.16.0",
          statisticDate: "2023-09-08T00:00:00Z",
          counts: {
            webDownloadCount: 3,
          },
        },
      ]);
      db.addVSCodeDownloadStats([
        {
          version: "2.16.0",
          statisticDate: "2023-09-08T00:00:00Z",
          counts: {
            installCount: 0,
            webDownloadCount: 3,
          },
        },
      ]);

      expect(db.countDailyVSCodeDownloads()).toEqual({
        dates: [Date.UTC(2023, 8, 8, 0, 0, 0)],
        counts: [3],
      });

      expect(
        db._querySQLForTesting(
          `
            SELECT
              UNIXEPOCH(archive_timestamp) AS archive_timestamp,
              timestamp,
              install_count,
              version
            FROM vscode_stats_archive
          `
        )
      ).toEqual([]);
    });
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
