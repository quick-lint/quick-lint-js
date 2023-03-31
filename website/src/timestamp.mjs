// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// See: https://www.w3.org/Protocols/rfc822/#z28
let rfc822Months = [
  "Jan",
  "Feb",
  "Mar",
  "Apr",
  "May",
  "Jun",
  "Jul",
  "Aug",
  "Sep",
  "Oct",
  "Nov",
  "Dec",
];

// Parses a subset of ISO 8601 timestamps.
//
// Example: "2022-05-25T21:04:02-07:00"
export function parseTimestamp(timestamp) {
  let match = timestamp.match(
    /^(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})T(?<hour>\d{2}):(?<minute>\d{2}):(?<second>\d{2})(?<zoneoffset>[-+]\d{2}:\d{2})$/
  );
  if (!match) {
    throw new Error(`failed to parse timestamp: ${timestamp}`);
  }
  let { year, month, day, hour, minute, second, zoneoffset } = match.groups;
  let monthNumber = parseInt(month, 10);
  return {
    date: `${year}-${month}-${day}`,
    // See: https://www.w3.org/Protocols/rfc822/#z28
    rfc822: `${day} ${
      rfc822Months[monthNumber - 1]
    } ${year} ${hour}:${minute}:${second} ${zoneoffset.replace(":", "")}`,
  };
}

// Parses a subset of ISO 8601 datestamps.
//
// Example: "2022-05-25"
export function datestampToRFC822(datestamp) {
  let match = datestamp.match(/^(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})$/);
  if (!match) {
    throw new Error(`failed to parse timestamp: ${datestamp}`);
  }
  let { year, month, day } = match.groups;
  let monthNumber = parseInt(month, 10);
  // See: https://www.w3.org/Protocols/rfc822/#z28
  return `${day} ${rfc822Months[monthNumber - 1]} ${year} 00:00:00 Z`;
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
