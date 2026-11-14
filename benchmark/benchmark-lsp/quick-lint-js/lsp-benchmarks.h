// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <memory>
#include <quick-lint-js/lsp-server-process.h>
#include <quick-lint-js/port/warning.h>
#include <string>

QLJS_WARNING_PUSH
#if __GNUC__ == 12
// HACK(strager): GCC 12 reports -Wzero-as-null-pointer-constant for the closing
// brace of every coroutine.
QLJS_WARNING_IGNORE_GCC("-Wzero-as-null-pointer-constant")
// HACK(strager): GCC 12 incorrectly reports these diagnostics for
// Benchmark::is_supported().
QLJS_WARNING_IGNORE_GCC("-Wsuggest-final-methods")
QLJS_WARNING_IGNORE_GCC("-Wsuggest-final-types")
#endif

namespace quick_lint_js {
class Benchmark {
 public:
  virtual ~Benchmark() {}

  virtual std::string name() const = 0;
  // Must be called by derived classes.
  virtual bool is_supported(const Benchmark_Config_Server&) const;

  virtual LSP_Task<void> set_up_async(LSP_Server_Process&,
                                      const Benchmark_Config_Server&,
                                      int iteration_count) = 0;

  virtual LSP_Task<void> run_iteration_async(LSP_Server_Process&,
                                             int iteration_index) = 0;

  LSP_Task<void> run_iterations_async(LSP_Server_Process& server,
                                      int start_iteration, int end_iteration) {
    for (int i = start_iteration; i < end_iteration; ++i) {
      co_await this->run_iteration_async(server, i);
    }
  }
};

using Benchmark_Factory = std::unique_ptr<Benchmark> (*)();

std::vector<Benchmark_Factory> get_benchmark_factories();
}

QLJS_WARNING_POP

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
