// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

import mariadb from "mariadb";

let sql = String.raw;

export class MatomoAnalyticsDB {
  #mariaDBConnection;

  constructor(mariaDBConnection) {
    this.#mariaDBConnection = mariaDBConnection;
  }

  static async fromConfigAsync(config) {
    let connection = await mariadb.createConnection({
      host: config["matomo_analytics.db_host"],
      user: config["matomo_analytics.db_user"],
      password: config["matomo_analytics.db_password"],
      database: config["matomo_analytics.db_database"],
      socketPath: config["matomo_analytics.db_socket"],
      bigIntAsNumber: true,
    });
    return new MatomoAnalyticsDB(connection);
  }

  close() {
    this.#mariaDBConnection.end();
  }

  async enumerateDownloadsAsync(onDownload) {
    let rows = await this.#mariaDBConnection.query(sql`
      SELECT
        UNIX_TIMESTAMP(matomo_log_link_visit_action.server_time) * 1000 AS timestamp,
        CASE 1
          WHEN matomo_log_action.name LIKE 'http://%'
            THEN CONCAT('https://', SUBSTRING(matomo_log_action.name FROM 8))
          WHEN matomo_log_action.name LIKE 'https://%'
            THEN matomo_log_action.name
          ELSE CONCAT('https://', matomo_log_action.name)
        END AS url,
        INET6_NTOA(matomo_log_visit.location_ip) AS downloaderIP,
        NULL AS downloaderUserAgent
      FROM matomo_log_link_visit_action
      LEFT JOIN matomo_log_action
        ON matomo_log_action.idaction = matomo_log_link_visit_action.idaction_url
      LEFT JOIN matomo_log_visit
        ON matomo_log_visit.idvisit = matomo_log_link_visit_action.idvisit
      WHERE
        matomo_log_action.name IS NOT NULL
    `);
    for (let row of rows) {
      onDownload(row);
    }
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
