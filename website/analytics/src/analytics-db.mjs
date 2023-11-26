// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import BetterSQLite3 from "better-sqlite3";
import assert from "node:assert/strict";

let sql = String.raw;

export class AnalyticsDB {
  #sqlite3DB;
  #checkDownloadConflictQuery;

  // Map<number, BetterSQLite3.Statement>
  #insertDownloadablesQueryCache = new Map();

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
    try {
      this.#sqlite3DB
        .prepare(
          sql`
            ALTER TABLE download
            ADD COLUMN referrer_downloadable_id
          `
        )
        .run();
    } catch (e) {
      // "duplicate column name"
      if (
        e.toString() ===
        "SqliteError: duplicate column name: referrer_downloadable_id"
      ) {
        // Migration was already performed.
      } else {
        throw e;
      }
    }
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

    this.#sqlite3DB
      .prepare(
        sql`
          CREATE TABLE IF NOT EXISTS vscode_stats (
            id INTEGER PRIMARY KEY NOT NULL,

            -- UNIX timestamp in seconds.
            timestamp NOT NULL,
            -- Cache: Start of the day containing 'timestamp'.
            timestamp_day NOT NULL,
            -- Cache: Start of the week containing 'timestamp'.
            timestamp_week NOT NULL,

            -- Version of quick-lint-js, e.g. "2.16.0".
            version NOT NULL,

            average_rating NOT NULL,
            install_count NOT NULL,
            uninstall_count NOT NULL,
            web_download_count NOT NULL,
            web_page_views NOT NULL
          )
        `
      )
      .run();
    this.#sqlite3DB
      .prepare(
        sql`
          CREATE UNIQUE INDEX IF NOT EXISTS vscode_stats_uniqueness ON vscode_stats (
            timestamp,
            version
          )
        `
      )
      .run();

    this.#sqlite3DB
      .prepare(
        sql`
          CREATE TABLE IF NOT EXISTS vscode_stats_archive (
            id INTEGER PRIMARY KEY NOT NULL,

            archive_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,

            -- See vscode_stats.
            timestamp,
            timestamp_day,
            timestamp_week,
            version,
            average_rating,
            install_count,
            uninstall_count,
            web_download_count,
            web_page_views
          )
        `
      )
      .run();
    this.#sqlite3DB
      .prepare(
        sql`
          CREATE TRIGGER IF NOT EXISTS vscode_stats_archive_trigger
          BEFORE UPDATE OF
            average_rating,
            install_count,
            uninstall_count,
            web_download_count,
            web_page_views
          ON vscode_stats
          WHEN
            old.average_rating != new.average_rating
            OR old.install_count != new.install_count
            OR old.uninstall_count != new.uninstall_count
            OR old.web_download_count != new.web_download_count
            OR old.web_page_views != new.web_page_views
          BEGIN
          INSERT INTO vscode_stats_archive (
            timestamp,
            timestamp_day,
            timestamp_week,
            version,

            average_rating,
            install_count,
            uninstall_count,
            web_download_count,
            web_page_views
          )
          VALUES (
            old.timestamp,
            old.timestamp_day,
            old.timestamp_week,
            old.version,

            old.average_rating,
            old.install_count,
            old.uninstall_count,
            old.web_download_count,
            old.web_page_views
          );
          END
        `
      )
      .run();

    this.#checkDownloadConflictQuery = this.#sqlite3DB.prepare(
      sql`
        SELECT
          download.id AS download_id,
          downloader_user_agent IS NULL AS downloader_user_agent_is_null,
          referrer_downloadable_id AS referrer_downloadable_id
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
    );
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

  addWebDownloadBatch(downloads) {
    if (downloads.length === 0) {
      return;
    }

    let downloadableIDs = this.#addDownloadablesBatch(
      downloads.map((download) => download.url)
    );
    let referrerDownloadableIDs = this.#addDownloadablesBatch(
      downloads
        .map((download) => download.referrerURL)
        .filter((referrerURL) => referrerURL != null)
    );

    let referrerIndex = 0;
    for (let i = 0; i < downloads.length; ++i) {
      let { timestamp, url, referrerURL, downloaderIP, downloaderUserAgent } =
        downloads[i];
      let referrerDownloadableID = null;
      if (referrerURL != null) {
        referrerDownloadableID = referrerDownloadableIDs[referrerIndex];
        assert.ok(referrerDownloadableID !== undefined);
        assert.ok(referrerDownloadableID !== null);
        referrerIndex += 1;
      }

      let parameters = {
        timestamp: timestampMSToS(timestamp),
        downloader_ip: downloaderIP,
        downloader_user_agent: downloaderUserAgent,
        downloadable_id: downloadableIDs[i],
        referrer_downloadable_id: referrerDownloadableID,
      };
      // TODO(strager): Put this query and the following query into a transation.
      let conflictResult = this.#checkDownloadConflictQuery.get(parameters);
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
                downloadable_id,
                referrer_downloadable_id
              )
              VALUES (
                @timestamp,
                -- TODO(strager): Deduplicate.
                STRFTIME('%s', DATETIME(@timestamp, 'unixepoch'), 'start of day'),
                STRFTIME('%s', DATETIME(@timestamp, 'unixepoch'), 'start of day', '-6 days', 'weekday 1'),
                @downloader_ip,
                @downloader_user_agent,
                @downloadable_id,
                @referrer_downloadable_id
              )
            `
          )
          .run(parameters);
      } else {
        // A matching download is already in the database.

        // One of the following is true:
        //
        // * the database has a single row with a null downloader_user_agent
        //   (conflictResult.downloader_user_agent_is_null === true)
        // * the database has one or more rows each with non-null
        //   downloader_user_agent
        //   (conflictResult.downloader_user_agent_is_null === false)
        let needToUpdateUserAgent;
        if (downloaderUserAgent === null) {
          if (conflictResult.downloader_user_agent_is_null) {
            // downloader_user_agent is up to date.
            needToUpdateUserAgent = false;
          } else {
            // The database has a row with a user agent already. Don't insert a
            // new row with a null user agent.
            needToUpdateUserAgent = false;
          }
        } else {
          if (conflictResult.downloader_user_agent_is_null) {
            // The database has a single row with a null downloader_user_agent.
            needToUpdateUserAgent = true;
          } else {
            // downloader_user_agent is up to date.
            needToUpdateUserAgent = false;
          }
        }

        let needToUpdateReferrer;
        if (referrerDownloadableID === null) {
          // Don't lose information.
          needToUpdateReferrer = false;
        } else if (
          conflictResult.referrer_downloadable_id === referrerDownloadableID
        ) {
          // referrer_downloadable_id is up to date.
          needToUpdateReferrer = false;
        } else {
          // referrer_downloadable_id is possibly out of date.
          needToUpdateReferrer = true;
        }

        let sets = [];
        if (needToUpdateUserAgent) {
          sets.push(sql`downloader_user_agent = @downloader_user_agent`);
        }
        if (needToUpdateReferrer) {
          sets.push(sql`referrer_downloadable_id = @referrer_downloadable_id`);
        }
        if (sets.length > 0) {
          this.#sqlite3DB
            .prepare(
              sql`
                  UPDATE download
                  SET ${sets.join(",")}
                  WHERE download.id = @download_id
                `
            )
            .run({
              downloader_user_agent: downloaderUserAgent,
              referrer_downloadable_id: referrerDownloadableID,
              download_id: conflictResult.download_id,
            });
        }
      }
    }
  }

  // @param urls Array<String>
  // @return Array<Number> Array of downloadable.id.
  #addDownloadablesBatch(urls) {
    if (urls.length === 0) {
      return;
    }
    let insertDownloadablesQuery = this.#insertDownloadablesQueryCache.get(
      urls.length
    );
    if (insertDownloadablesQuery === undefined) {
      let urlPlaceholders = Array(urls.length).fill("(?)").join(", ");
      insertDownloadablesQuery = this.#sqlite3DB
        .prepare(
          sql`
            INSERT INTO downloadable (url)
            VALUES ${urlPlaceholders}
            ON CONFLICT (url) DO
            UPDATE SET url = url
            RETURNING id
          `
        )
        .pluck();
      this.#insertDownloadablesQueryCache.set(
        urls.length,
        insertDownloadablesQuery
      );
    }
    return insertDownloadablesQuery.all(urls);
  }

  // @param download { timestamp, url, downloaderIP, downloaderUserAgent }
  addWebDownload(download) {
    this.addWebDownloadBatch([download]);
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

  addVSCodeDownloadStats(vscodeDownloadStats) {
    for (let statsForOneDay of vscodeDownloadStats) {
      this.#sqlite3DB
        .prepare(
          sql`
            INSERT INTO vscode_stats (
              timestamp,
              timestamp_day,
              timestamp_week,
              version,
              average_rating,
              install_count,
              uninstall_count,
              web_download_count,
              web_page_views
            )
            VALUES (
              STRFTIME('%s', @timestamp),
              -- TODO(strager): Deduplicate.
              STRFTIME('%s', @timestamp, 'start of day'),
              STRFTIME('%s', @timestamp, 'start of day', '-6 days', 'weekday 1'),
              @version,
              @average_rating,
              @install_count,
              @uninstall_count,
              @web_download_count,
              @web_page_views
            )
            ON CONFLICT (timestamp, version)
            DO UPDATE SET
              average_rating = @average_rating,
              install_count = @install_count,
              uninstall_count = @uninstall_count,
              web_download_count = @web_download_count,
              web_page_views = @web_page_views
          `
        )
        .run({
          timestamp: statsForOneDay.statisticDate,
          version: statsForOneDay.version,
          average_rating: statsForOneDay.counts.averageRating ?? -1,
          install_count: statsForOneDay.counts.installCount ?? 0,
          uninstall_count: statsForOneDay.counts.uninstallCount ?? 0,
          web_download_count: statsForOneDay.counts.webDownloadCount ?? 0,
          web_page_views: statsForOneDay.counts.webPageViews ?? 0,
        });
    }
  }

  countDailyVSCodeDownloads() {
    let rows = this.#sqlite3DB
      .prepare(
        sql`
          SELECT
            vscode_stats.timestamp_day AS timestamp_day,
            SUM(install_count) + SUM(web_download_count) AS count
          FROM vscode_stats
          GROUP BY vscode_stats.timestamp_day
        `
      )
      .raw()
      .all();
    let dates = [];
    let counts = [];
    for (let [timestamp, count] of rows) {
      dates.push(timestamp * 1000);
      counts.push(count);
    }
    return { dates, counts };
  }

  countWeeklyVSCodeDownloads() {
    let rows = this.#sqlite3DB
      .prepare(
        sql`
          SELECT
            vscode_stats.timestamp_week AS timestamp_week,
            SUM(install_count) + SUM(web_download_count) AS count
          FROM vscode_stats
          GROUP BY vscode_stats.timestamp_week
        `
      )
      .raw()
      .all();
    let dates = [];
    let counts = [];
    for (let [timestamp, count] of rows) {
      dates.push(timestamp * 1000);
      counts.push(count);
    }
    return { dates, counts };
  }

  // Find what pages users clicked on when visiting the page at sourceURL.
  //
  // beginTimestamp is inclusive.
  // endTimestamp is exclusive.
  countWebDownloadsComingFromURL(sourceURL, beginTimestamp, endTimestamp) {
    let rows = this.#sqlite3DB
      .prepare(
        sql`
          SELECT
            dest_downloadable.url AS url,
            COUNT(*) AS count
          FROM downloadable AS source_downloadable

          INNER JOIN download AS dest_download
          ON dest_download.referrer_downloadable_id = source_downloadable.id

          INNER JOIN downloadable AS dest_downloadable
          ON dest_downloadable.id = dest_download.downloadable_id

          WHERE
            source_downloadable.url = @source_url
            AND @begin_timestamp <= dest_download.timestamp
            AND dest_download.timestamp < @end_timestamp

          GROUP BY dest_downloadable.id
        `
      )
      .raw()
      .all({
        source_url: sourceURL,
        begin_timestamp: timestampMSToS(beginTimestamp),
        end_timestamp: timestampMSToS(endTimestamp),
      });
    let urls = [];
    let counts = [];
    for (let [url, count] of rows) {
      urls.push(url);
      counts.push(count);
    }
    return { urls, counts };
  }

  _querySQLForTesting(sqlQuery) {
    return this.#sqlite3DB.prepare(sqlQuery).all();
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
