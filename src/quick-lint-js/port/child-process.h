// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

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
struct Run_Program_Options {
  const char* current_directory = nullptr;
  String8_View input;
};

struct Run_Program_Result {
  Padded_String output;
  std::uint32_t exit_status;
};

Run_Program_Result run_program(std::initializer_list<std::string> command);
Run_Program_Result run_program(std::initializer_list<const char*> command);
Run_Program_Result run_program(std::initializer_list<std::string> command,
                               Run_Program_Options);
Run_Program_Result run_program(std::initializer_list<const char*> command,
                               Run_Program_Options);
Run_Program_Result run_program(Span<const std::string> command);
Run_Program_Result run_program(Span<const char* const> command);
Run_Program_Result run_program(Span<const std::string> command,
                               Run_Program_Options);
Run_Program_Result run_program(Span<const char* const> command,
                               Run_Program_Options);

#if QLJS_HAVE_SYS_WAIT_H
std::uint32_t wait_for_process_exit(::pid_t);
#endif
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
