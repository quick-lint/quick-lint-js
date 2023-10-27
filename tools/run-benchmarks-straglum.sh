#!/usr/bin/env bash

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

# This script runs LSP benchmarks on strager's straglum laptop for the
# website: https://quick-lint-js.com/benchmarks/

set -e
set -u

sudo cpupower frequency-set --governor performance

. ~/tmp/Projects/nvm/nvm.sh
nvm install node
nvm use node
npm install -g yarn

CXX=g++-10 CXXFLAGS=-fcoroutines cmake -S . -B build -G Ninja -DQUICK_LINT_JS_ENABLE_BENCHMARKS=1 -DCMAKE_BUILD_TYPE=Release
ninja -C build quick-lint-js-benchmark-lsp-servers

cd benchmark/benchmark-lsp/

(
    cd eslint/
    yarn
    cd node_modules/vscode-eslint/
    npm ci && npm run compile:server
)

(
    cd flow/
    yarn
)

(
    cd typescript/
    yarn
)

sudo apt-get update
sudo apt-get install --yes quick-lint-js

(
    cd biome/
    yarn
)

(
    cd ~/tmp/Projects/RSLint/
    cargo build --release
)

PATH="${HOME}/tmp/Projects/RSLint/target/release/:${PATH}"
PATH="${HOME}/bin/:${PATH}"  # For Deno.

cool_down_cpu() {
    sleep 10
}

cool_down_cpu
../../build/benchmark/benchmark-lsp/quick-lint-js-benchmark-lsp-servers \
    --iterations 10 \
    --samples 10 \
    --output-json ../../website/public/benchmarks/full-change-wait-express-router-js.json \
    full-change-wait/express-router.js

cool_down_cpu
../../build/benchmark/benchmark-lsp/quick-lint-js-benchmark-lsp-servers \
    --iterations 10 \
    --samples 10 \
    --output-json ../../website/public/benchmarks/incremental-change-wait-express-router-js.json \
    incremental-change-wait/express-router.js

cool_down_cpu
../../build/benchmark/benchmark-lsp/quick-lint-js-benchmark-lsp-servers \
    --iterations 10 \
    --samples 10 \
    --output-json ../../website/public/benchmarks/incremental-change-wait-react-quickly-ch10-jsx.json \
    incremental-change-wait/react-quickly-ch10.jsx

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew "strager" Glazar
#
# This file is part of quick-lint-js.
#
# quick-lint-js is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# quick-lint-js is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
