# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

# This is a CMake toolchain file used on CI to cross-compile to Linux AArch64.

set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR aarch64)

set(CMAKE_C_COMPILER aarch64-linux-gnu-gcc-10)
set(CMAKE_CXX_COMPILER aarch64-linux-gnu-g++-10)

set(CMAKE_SYSTEM_INCLUDE_PATH /usr/aarch64-linux-gnu/include)
set(CMAKE_SYSTEM_LIBRARY_PATH /usr/aarch64-linux-gnu/lib)
set(CMAKE_SYSTEM_PREFIX_PATH /usr/aarch64-linux-gnu)

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew Glazar
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
