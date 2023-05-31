# Install script for directory: C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/install/x64-Debug")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Debug")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/tools/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/vendor/googletest/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/dist/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/docs/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/fuzz/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/plugin/vim/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/plugin/emacs/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/plugin/vscode/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/src/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/website/wasm/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/completions/cmake_install.cmake")
  include("C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/test/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
