# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

if (NOT QUICK_LINT_JS_NODEJS_OS_PLATFORM)
  if (CMAKE_SYSTEM_NAME STREQUAL Darwin)
    set(NODEJS_OS_PLATFORM darwin)
  elseif (CMAKE_SYSTEM_NAME STREQUAL Linux)
    set(NODEJS_OS_PLATFORM linux)
  elseif (CMAKE_SYSTEM_NAME STREQUAL Windows)
    set(NODEJS_OS_PLATFORM win32)
  else ()
    set(NODEJS_OS_PLATFORM)
  endif ()
  set(
    QUICK_LINT_JS_NODEJS_OS_PLATFORM
    "${NODEJS_OS_PLATFORM}"
    CACHE STRING
    "Node.js value of os.platform() for the target system"
    FORCE
  )
endif ()
if (NOT QUICK_LINT_JS_NODEJS_OS_ARCH)
  set(NODEJS_OS_ARCH)
  if (CMAKE_VS_PLATFORM_NAME STREQUAL ARM)
    set(NODEJS_OS_ARCH arm)
  elseif (CMAKE_VS_PLATFORM_NAME STREQUAL ARM64)
    set(NODEJS_OS_ARCH arm64)
  elseif (CMAKE_VS_PLATFORM_NAME STREQUAL Win32)
    set(NODEJS_OS_ARCH ia32)
  elseif (CMAKE_VS_PLATFORM_NAME STREQUAL x64)
    set(NODEJS_OS_ARCH x64)
  elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL AMD64 OR CMAKE_SYSTEM_PROCESSOR STREQUAL x86_64)
    set(NODEJS_OS_ARCH x64)
  elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL aarch64)
    set(NODEJS_OS_ARCH arm64)
  elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL arm)
    set(NODEJS_OS_ARCH arm)
  endif ()
  set(
    QUICK_LINT_JS_NODEJS_OS_ARCH
    "${NODEJS_OS_ARCH}"
    CACHE STRING
    "Node.js value of os.arch() for the target system"
    FORCE
  )
endif ()

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
