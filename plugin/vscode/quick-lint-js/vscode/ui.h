// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/container/result.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/vscode/vscode.h>

namespace quick_lint_js {
class QLJS_Workspace;
struct Canonicalize_Path_IO_Error;
struct Loaded_Config_File;
struct Watch_IO_Error;
struct Read_File_IO_Error;

class VSCode_UI {
 public:
  // NOTE[VSCode_UI-lifetime]: workspace must live at least as long as this
  // VSCode_UI.
  explicit VSCode_UI(QLJS_Workspace* workspace);

  void show_watch_io_errors(::Napi::Env env, Span<const Watch_IO_Error> errors);

  void show_associated_config_file_errors(::Napi::Env env,
                                          std::string_view document_path,
                                          std::string_view config_file_path);

  void show_config_file_load_errors(
      ::Napi::Env env, std::string_view document_path,
      const Result<Loaded_Config_File*, Canonicalize_Path_IO_Error,
                   Read_File_IO_Error>& error);

 private:
  VSCode_Module& vscode();
  QLJS_Workspace* workspace_;
};
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
