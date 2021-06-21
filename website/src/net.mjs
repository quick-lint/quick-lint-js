// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

export function listenAsync(server, ...args) {
  return new Promise((resolve, reject) => {
    server.listen(...args);
    function onError(error) {
      removeListeners();
      reject(error);
    }
    function onListening() {
      removeListeners();
      resolve();
    }
    function removeListeners() {
      server.removeListener("error", onError);
      server.removeListener("listening", onListening);
    }
    server.on("error", onError);
    server.on("listening", onListening);
  });
}

export function urlFromServerAddress(address) {
  if (typeof address !== "object") {
    throw new Error(
      `Expected an AF_INET or AF_INET6 address, but got ${address}`
    );
  }
  let url = new URL("http://localhost/");
  switch (address.family) {
    case "IPv4":
      url.hostname = address.address;
      break;
    case "IPv6":
      url.hostname = `[${address.address}]`;
      break;
    default:
      throw new Error(
        `Expected an AF_INET or AF_INET6 address, but got an ${address.family} address`
      );
  }
  url.port = String(address.port);
  return url;
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
