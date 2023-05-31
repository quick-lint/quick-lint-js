# Install script for directory: C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/plugin/vim

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

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/vim/vimfiles/ale_linters/javascript" TYPE FILE FILES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/plugin/vim/quick-lint-js.vim/ale_linters/javascript/quick_lint_js.vim")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/vim/vimfiles/autoload" TYPE FILE FILES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/plugin/vim/quick-lint-js.vim/autoload/quick_lint_js_ale.vim")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/vim/vimfiles/doc" TYPE FILE FILES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/plugin/vim/quick-lint-js.vim/doc/quick-lint-js.txt")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/vim/vimfiles/plugin" TYPE FILE FILES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/plugin/vim/quick-lint-js.vim/plugin/quick-lint-js.vim")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/nvim/site/ale_linters/javascript" TYPE FILE FILES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/plugin/vim/quick-lint-js.vim/ale_linters/javascript/quick_lint_js.vim")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/nvim/site/autoload" TYPE FILE FILES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/plugin/vim/quick-lint-js.vim/autoload/quick_lint_js_ale.vim")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/nvim/site/doc" TYPE FILE FILES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/plugin/vim/quick-lint-js.vim/doc/quick-lint-js.txt")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/nvim/site/plugin" TYPE FILE FILES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/plugin/vim/quick-lint-js.vim/plugin/quick-lint-js.vim")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/nvim/site/lua/lspconfig" TYPE FILE FILES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/plugin/vim/quick-lint-js.vim/lua/lspconfig/quick_lint_js.lua")
endif()

