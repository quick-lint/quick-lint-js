// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import BetterSQLite3 from "better-sqlite3";

let sql = String.raw;

export class AnalyticsDB {
  #sqlite3DB;

  constructor(sqlite3DB) {
    this.#sqlite3DB = sqlite3DB;

    this.#sqlite3DB.prepare(sql`PRAGMA journal_mode = WAL`).run();
    this.#sqlite3DB.prepare(sql`PRAGMA synchronous = NORMAL`).run();

    this.#sqlite3DB
      .prepare(
        sql`
          CREATE TABLE IF NOT EXISTS downloadable (
            id INTEGER PRIMARY KEY NOT NULL,
            url NOT NULL UNIQUE
          )
        `
      )
      .run();
    this.#sqlite3DB
      .prepare(
        sql`
          CREATE TABLE IF NOT EXISTS download (
            id INTEGER PRIMARY KEY NOT NULL,

            -- UNIX timestamp in seconds.
            timestamp NOT NULL,
            -- Cache: Start of the day containing 'timestamp'.
            timestamp_day NOT NULL,
            -- Cache: Start of the week containing 'timestamp'.
            timestamp_week NOT NULL,

            downloader_ip,
            downloader_user_agent,
            downloadable_id NOT NULL
          )
        `
      )
      .run();
    this.#sqlite3DB
      .prepare(
        sql`
          -- Invariant not encoded in SQL:
          -- If downloader_user_agent is NULL, then that row must be the only
          -- row with an equal (timestamp, downloader_ip, downloadable_id).
          CREATE UNIQUE INDEX IF NOT EXISTS download_uniqueness ON download (
            timestamp,
            downloader_ip,
            downloadable_id,
            downloader_user_agent
          )
        `
      )
      .run();
  }

  static fromFile(path) {
    return new AnalyticsDB(new BetterSQLite3(path));
  }

  static newInMemory() {
    return new AnalyticsDB(new BetterSQLite3(":memory:"));
  }

  close() {
    this.#sqlite3DB.close();
  }

  async batchAsync(callback) {
    try {
      this.#sqlite3DB.prepare(sql`BEGIN TRANSACTION`).run();
      return await callback();
    } finally {
      this.#sqlite3DB.prepare(sql`END TRANSACTION`).run();
    }
  }

  addWebDownload({ timestamp, url, downloaderIP, downloaderUserAgent }) {
    let insertResult = this.#sqlite3DB
      .prepare(
        sql`
          INSERT INTO downloadable (url)
          VALUES (@url)
          ON CONFLICT (url) DO
          UPDATE SET url = url
          RETURNING id
        `
      )
      .get({
        url: url,
      });
    let downloadableID = insertResult.id;

    let parameters = {
      timestamp: timestampMSToS(timestamp),
      downloader_ip: downloaderIP,
      downloader_user_agent: downloaderUserAgent,
      downloadable_id: downloadableID,
    };
    // TODO(strager): Put this query and the following query into a transation.
    let conflictResult = this.#sqlite3DB
      .prepare(
        sql`
          SELECT
            download.id AS download_id,
            downloader_user_agent IS NULL AS downloader_user_agent_is_null
          FROM download
          WHERE
            timestamp = @timestamp
            AND downloader_ip = @downloader_ip
            AND downloadable_id = @downloadable_id
            AND (
              downloader_user_agent IS NULL
              OR @downloader_user_agent IS NULL
              OR downloader_user_agent = @downloader_user_agent
            )
          LIMIT 1
        `
      )
      .get(parameters);
    if (conflictResult === undefined) {
      this.#sqlite3DB
        .prepare(
          sql`
            INSERT INTO download (
              timestamp,
              timestamp_day,
              timestamp_week,
              downloader_ip,
              downloader_user_agent,
              downloadable_id
            )
            VALUES (
              @timestamp,
              STRFTIME('%s', DATETIME(@timestamp, 'unixepoch'), 'start of day'),
              STRFTIME('%s', DATETIME(@timestamp, 'unixepoch'), 'start of day', '-6 days', 'weekday 1'),
              @downloader_ip,
              @downloader_user_agent,
              @downloadable_id
            )
          `
        )
        .run(parameters);
    } else {
      // A matching download is already in the database.
      //
      // One of the following is true:
      //
      // * the database has a single row with a null downloader_user_agent
      //   (conflictResult.downloader_user_agent_is_null === true)
      // * the database has one or more rows each with non-null
      //   downloader_user_agent
      //   (conflictResult.downloader_user_agent_is_null === false)
      if (downloaderUserAgent === null) {
        if (conflictResult.downloader_user_agent_is_null) {
          // The database has a row which is identical to the row we want to
          // insert.
        } else {
          // The database has a row with a user agent already. Don't insert a
          // new row with a null user agent.
        }
      } else {
        if (conflictResult.downloader_user_agent_is_null) {
          // The database has a single row with a null downloader_user_agent.
          this.#sqlite3DB
            .prepare(
              sql`
                UPDATE download
                SET downloader_user_agent = @downloader_user_agent
                WHERE download.id = @download_id
              `
            )
            .run({
              downloader_user_agent: downloaderUserAgent,
              download_id: conflictResult.download_id,
            });
        } else {
          // The database has a row which is identical to the row we want to
          // insert.
        }
      }
    }
  }

  getWebDownloadedURLs() {
    let rows = this.#sqlite3DB
      .prepare(
        sql`
          SELECT url
          FROM downloadable
        `
      )
      .all();
    return rows.map((row) => row.url);
  }

  // @return {dates: Array<UNIXTimestampMS>, counts: Array<Integer>}
  countDailyWebDownloaders(urls) {
    return this.#countWebDownloadersWithTimestampColumn(urls, "timestamp_day");
  }

  // @return {dates: Array<UNIXTimestampMS>, counts: Array<Integer>}
  countWeeklyWebDownloaders(urls) {
    return this.#countWebDownloadersWithTimestampColumn(urls, "timestamp_week");
  }

  #countWebDownloadersWithTimestampColumn(urls, timestampColumn) {
    let urlPlaceholders = Array(urls.length).fill("?").join(", ");
    let rows = this.#sqlite3DB
      .prepare(
        sql`
          SELECT timestamp, COUNT(*) AS count
          FROM (
            SELECT download.${timestampColumn} AS timestamp
            FROM download
            LEFT JOIN downloadable ON downloadable.id = download.downloadable_id
            WHERE downloadable.url IN (${urlPlaceholders})
            GROUP BY
              download.${timestampColumn},
              -- Downloads from the same IP with the same user agent are from
              -- the same downloader.
              download.downloader_ip,
              download.downloader_user_agent
          )
          GROUP BY timestamp
        `
      )
      .raw()
      .all(urls);
    let dates = [];
    let counts = [];
    for (let [timestamp, count] of rows) {
      dates.push(timestamp * 1000);
      counts.push(count);
    }
    return { dates, counts };
  }

  // @return {dates: Array<UNIXTimestampMS>, counts: Array<Integer>}
  countDailyWebDownloads(urls) {
    let urlPlaceholders = Array(urls.length).fill("?").join(", ");
    let rows = this.#sqlite3DB
      .prepare(
        sql`
          SELECT download.timestamp_day AS timestamp_day, COUNT(*) AS count
          FROM download
          LEFT JOIN downloadable ON downloadable.id = download.downloadable_id
          WHERE downloadable.url IN (${urlPlaceholders})
          GROUP BY download.timestamp_day
        `
      )
      .raw()
      .all(urls);
    let dates = [];
    let counts = [];
    for (let [timestamp, count] of rows) {
      dates.push(timestamp * 1000);
      counts.push(count);
    }
    return { dates, counts };
  }
}

function timestampMSToS(timestamp) {
  return Math.floor(timestamp / 1000);
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
