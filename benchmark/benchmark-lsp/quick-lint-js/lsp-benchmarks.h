// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_LSP_BENCHMARKS_H
#define QUICK_LINT_JS_LSP_BENCHMARKS_H

#include <memory>
#include <quick-lint-js/lsp-server-process.h>
#include <string>

namespace quick_lint_js {
class benchmark {
 public:
  virtual ~benchmark() {}

  virtual std::string name() const = 0;
  // Must be called by derived classes.
  virtual bool is_supported(const benchmark_config_server&) const;

  virtual lsp_task<void> set_up_async(lsp_server_process&,
                                      const benchmark_config_server&,
                                      int iteration_count) = 0;

  virtual lsp_task<void> run_iteration_async(lsp_server_process&,
                                             int iteration_index) = 0;

  lsp_task<void> run_iterations_async(lsp_server_process& server,
                                      int start_iteration, int end_iteration) {
    for (int i = start_iteration; i < end_iteration; ++i) {
      co_await this->run_iteration_async(server, i);
    }
  }
};

using benchmark_factory = std::unique_ptr<benchmark> (*)();

std::vector<benchmark_factory> get_benchmark_factories();
}

#endif

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
