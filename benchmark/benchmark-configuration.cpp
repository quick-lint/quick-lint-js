// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/diag/diag-reporter.h>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
void benchmark_parse_config_json(::benchmark::State& state,
                                 String8_View config_json) {
  Padded_String config_json_string(config_json);
  Null_Diag_Reporter diag_reporter;

  Configuration config;
  Monotonic_Allocator allocator("benchmark");
  for (auto _ : state) {
    Monotonic_Allocator::Rewind_Guard allocator_rewind =
        allocator.make_rewind_guard();

    config.reset();
    Diag_List diags(&allocator);
    config.load_from_json(&config_json_string, &diags);
    ::benchmark::ClobberMemory();
  }
}
BENCHMARK_CAPTURE(benchmark_parse_config_json, empty, u8"{}"sv);
BENCHMARK_CAPTURE(benchmark_parse_config_json, no_globals,
                  u8R"({
  "global-groups": false
}
)"sv);
BENCHMARK_CAPTURE(benchmark_parse_config_json, medium_sized,
                  u8R"({
  "global-groups": [
    "browser",
    "ecmascript",
    "jquery"
  ],
  "globals": {
    "google": true,
    "GOOGLE": true,
    "React": {
      "shadowable": true,
      "writable": false
    }
  }
}
)"sv);

void benchmark_config_globals_default(::benchmark::State& state) {
  Configuration config;
  for (auto _ : state) {
    config.reset();
    ::benchmark::DoNotOptimize(config.globals());
  }
}
BENCHMARK(benchmark_config_globals_default);

void benchmark_config_globals_cleared(::benchmark::State& state) {
  Configuration config;
  for (auto _ : state) {
    config.reset();
    config.reset_global_groups();
    ::benchmark::DoNotOptimize(config.globals());
  }
}
BENCHMARK(benchmark_config_globals_cleared);
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
