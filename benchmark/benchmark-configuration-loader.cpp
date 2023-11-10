// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <quick-lint-js/cli/options.h>
#include <quick-lint-js/configuration/basic-configuration-filesystem.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/util/cast.h>
#include <string>

namespace quick_lint_js {
namespace {
#if !defined(__EMSCRIPTEN__)  // TODO(#800): Support Emscripten.
void benchmark_no_config_file(::benchmark::State& state) {
  int extra_depth = narrow_cast<int>(state.range(0));
  std::string temp_dir = make_temporary_directory();

  std::string path = temp_dir;
  for (int i = 0; i < extra_depth; ++i) {
    path += "/subdir" + std::to_string(i);
    create_directory_or_exit(path);
  }
  path += "/hello.js";
  write_file_or_exit(path, u8"");

  for (auto _ : state) {
    Configuration_Loader loader(Basic_Configuration_Filesystem::instance());
    auto config = loader.load_for_file(path);
    ::benchmark::DoNotOptimize(config);
  }
}
BENCHMARK(benchmark_no_config_file)
    ->Arg(0)
    ->Arg(8)
    ->Arg(16)
    ->Arg(24)
    ->Arg(32)
    ->Arg(48)
    ->Arg(64);
#endif
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
