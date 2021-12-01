# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

set(SIMDJSON_DEVELOPER_MODE OFF CACHE INTERNAL "")
set(BUILD_SHARED_LIBS OFF)

add_subdirectory(simdjson EXCLUDE_FROM_ALL)

target_compile_definitions(simdjson PUBLIC SIMDJSON_EXCEPTIONS=0)

# HACK(strager): Avoid various warnings when including <simdjson.h>.
get_property(
  SIMDJSON_INCLUDE_DIRECTORIES
  TARGET simdjson
  PROPERTY INTERFACE_INCLUDE_DIRECTORIES
)
set_property(
  TARGET simdjson
  APPEND PROPERTY
  INTERFACE_SYSTEM_INCLUDE_DIRECTORIES
  "${SIMDJSON_INCLUDE_DIRECTORIES}"
)

quick_lint_js_add_warning_options_if_supported(
  simdjson
  PRIVATE
  -Wno-array-bounds
)

# HACK(strager): libc++ versions 6 through 9 (and maybe other versions) marks
# std::signbit as [[gnu::always_inline]]. For reasons I don't understand, this
# causes problems when compiling simdjson:
#
# > vendor/simdjson/src/to_chars.cpp:918:7: error: always_inline function
# > 'signbit' requires target feature 'avx2', but would be inlined into function
# > 'to_chars' that is compiled without support for 'avx2'
#
# Work around this error by using [[gnu::internal_linkage]] instead of
# [[gnu::always_inline]] for std::signbit (and, as collateral damage, a bunch of
# other functions).
target_compile_definitions(
  simdjson
  PRIVATE
  # libc++ version 7 and newer:
  _LIBCPP_HIDE_FROM_ABI_PER_TU_BY_DEFAULT=1
  # libc++ version 6 (also affects version 7 and newer):
  _LIBCPP_INLINE_VISIBILITY=__attribute__\(\(internal_linkage\)\)
)

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
