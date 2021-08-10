# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

add_library(node-napi INTERFACE)
target_compile_definitions(node-napi INTERFACE BUILDING_NODE_EXTENSION)
target_include_directories(node-napi SYSTEM INTERFACE node/src)
# Users should also set the NAPI_VERSION compile definition.

if (APPLE)
  # Allow the linker to succeed by effectively ignoring undefined-symbol errors.
  target_link_libraries(node-napi INTERFACE "-undefined dynamic_lookup")
endif ()
if (WIN32)
  # Create a .lib file for linking based on the symbol list in node.def.
  if (CMAKE_SYSTEM_PROCESSOR STREQUAL AMD64)
    set(LIB_MACHINE /MACHINE:X64)
  else ()
    set(LIB_MACHINE)
  endif ()
  add_custom_command(
    OUTPUT node-napi.lib node-napi.exp
    COMMAND
      lib
      "/DEF:${CMAKE_CURRENT_SOURCE_DIR}/node.def"
      /OUT:node-napi.lib
      /WX
      ${LIB_MACHINE}
    DEPENDS node.def
    COMMENT "Generating node-napi implib"
  )
  add_custom_target(
    node-napi-implib
    DEPENDS node-napi.lib
    SOURCES node.def
  )
  add_dependencies(node-napi node-napi-implib)
  target_link_libraries(
    node-napi
    INTERFACE
    "${CMAKE_CURRENT_BINARY_DIR}/node-napi.lib"
  )

  # Ensure symbols are found in the extension host (node.exe or code.exe or
  # electron.exe or whatever), not in a separately-loaded DLL called "NODE.EXE".
  add_library(node-hook STATIC node-hook.cpp)
  target_link_libraries(
    node-napi
    INTERFACE
    -DELAY:nobind  # Reduce binary size.
    -DELAYLOAD:NODE.EXE
    -WHOLEARCHIVE:$<TARGET_FILE:node-hook>
    delayimp
  )
  add_dependencies(node-napi node-hook)
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
