// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_PORT_CHILD_PROCESS_H
#define QUICK_LINT_JS_PORT_CHILD_PROCESS_H

#include <initializer_list>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/span.h>
#include <string>
#include <vector>

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace quick_lint_js {
struct run_program_options {
  const char* current_directory = nullptr;
  string8_view input;
};

struct run_program_result {
  padded_string output;
  std::uint32_t exit_status;
};

run_program_result run_program(std::initializer_list<std::string> command);
run_program_result run_program(std::initializer_list<const char*> command);
run_program_result run_program(std::initializer_list<std::string> command,
                               run_program_options);
run_program_result run_program(std::initializer_list<const char*> command,
                               run_program_options);
run_program_result run_program(span<const std::string> command);
run_program_result run_program(span<const char* const> command);
run_program_result run_program(span<const std::string> command,
                               run_program_options);
run_program_result run_program(span<const char* const> command,
                               run_program_options);

#if QLJS_HAVE_SYS_WAIT_H
std::uint32_t wait_for_process_exit(::pid_t);
#endif
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
