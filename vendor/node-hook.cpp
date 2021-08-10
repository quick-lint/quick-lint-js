// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(_WIN32)
#include <Windows.h>
#include <delayimp.h>
#include <string_view>

using namespace std::literals::string_view_literals;

namespace {
// Instead of searching for and loading node.exe as a DLL, use the main
// executable (code.exe).
FARPROC WINAPI delay_load_notify_hook(unsigned dliNotify,
                                      ::DelayLoadInfo* pdli) {
  switch (dliNotify) {
  case dliNotePreLoadLibrary:
    if (pdli->szDll == "NODE.EXE"sv) {
      ::HMODULE exeModule = ::GetModuleHandleW(nullptr);
      return reinterpret_cast<::FARPROC>(exeModule);
    }
    return nullptr;

  case dliStartProcessing:
  case dliNotePreGetProcAddress:
  case dliNoteEndProcessing:
  default:
    return nullptr;

  case dliFailLoadLib:
  case dliFailGetProc:
    // Shouldn't happen.
    ::DebugBreak();
    return nullptr;
  }
}
}

extern "C" const ::PfnDliHook __pfnDliNotifyHook2 = delay_load_notify_hook;
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
