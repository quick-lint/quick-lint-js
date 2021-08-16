# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

cmake_minimum_required(VERSION 3.10)
include(GNUInstallDirs)

find_package(QuickLintJSEmacs)

macro(emacs_pkg_target NAME FILE)
  cmake_parse_arguments(
    ""
    ""
    "OUTPUT"
    ""
    ${ARGN})

  add_custom_command(
    OUTPUT ${_OUTPUT}
    COMMAND
      ${QUICK_LINT_JS_EMACS}
      -Q -batch
      -l package --eval "(package-initialize)"
      --eval "(add-to-list 'package-directory-list \"${CMAKE_CURRENT_BINARY_DIR}\")"
      -l ${CMAKE_CURRENT_LIST_DIR}/quicklintjs-make-package.el
      -f quicklintjs-batch-make-pkg
      ${FILE}
      WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    DEPENDS
      ${QUICK_LINT_JS_EMACS}
      ${CMAKE_CURRENT_LIST_DIR}/quicklintjs-make-package.el
      ${FILE}
    VERBATIM)

  add_custom_target(${NAME} ALL DEPENDS ${_OUTPUT})
endmacro ()

if (QUICK_LINT_JS_EMACS_FOUND)
  emacs_pkg_target(emacs-plugin-quicklintjs
    ${CMAKE_CURRENT_LIST_DIR}/quicklintjs.el 0.0.1
    OUTPUT quicklintjs-0.0.1)

  emacs_pkg_target(emacs-plugin-flycheck-quicklintjs
    ${CMAKE_CURRENT_LIST_DIR}/flycheck-quicklintjs.el 0.0.1
    OUTPUT flycheck-quicklintjs-0.0.1)

  emacs_pkg_target(emacs-plugin-flymake-quicklintjs
    ${CMAKE_CURRENT_LIST_DIR}/flymake-quicklintjs.el 0.0.1
    OUTPUT flymake-quicklintjs-0.0.1)

  emacs_pkg_target(emacs-plugin-eglot-quicklintjs
    ${CMAKE_CURRENT_LIST_DIR}/eglot-quicklintjs.el 0.0.1
    OUTPUT eglot-quicklintjs-0.0.1)

  emacs_pkg_target(emacs-plugin-lsp-quicklintjs
    ${CMAKE_CURRENT_LIST_DIR}/lsp-quicklintjs.el 0.0.1
    OUTPUT lsp-quicklintjs-0.0.1)

  add_dependencies(emacs-plugin-flycheck-quicklintjs
    emacs-plugin-quicklintjs)
  add_dependencies(emacs-plugin-flymake-quicklintjs
    emacs-plugin-quicklintjs)
  add_dependencies(emacs-plugin-eglot-quicklintjs
    emacs-plugin-quicklintjs)
  add_dependencies(emacs-plugin-lsp-quicklintjs
    emacs-plugin-quicklintjs)

  install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/quicklintjs-0.0.1
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/emacs/site-lisp/elpa)
  install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/flycheck-quicklintjs-0.0.1
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/emacs/site-lisp/elpa)
  install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/flymake-quicklintjs-0.0.1
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/emacs/site-lisp/elpa)
  install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/eglot-quicklintjs-0.0.1
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/emacs/site-lisp/elpa)
  install(
    DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lsp-quicklintjs-0.0.1
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/emacs/site-lisp/elpa)
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
