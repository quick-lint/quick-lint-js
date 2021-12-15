# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)
include(CheckCXXSourceCompiles)

function (quick_lint_js_check_designated_initializers OUT)
  check_cxx_source_compiles(
    "struct s { int m; }; int main() { s x{.m = 0}; return x.m; }"
    "${OUT}"
  )
endfunction ()

function (quick_lint_js_check_designated_initializers_with_defaults OUT)
  check_cxx_source_compiles(
    "struct s { int a = 1; int b; int c = 3; }; int main() { s x{.b = 0}; return x.c; }"
    "${OUT}"
  )
endfunction ()

function (quick_lint_js_enable_char8_t_if_supported)
  check_cxx_compiler_flag(-fchar8_t QUICK_LINT_JS_HAVE_FCHAR8_T_FLAG)
  if (QUICK_LINT_JS_HAVE_FCHAR8_T_FLAG)
    check_cxx_source_compiles(
      "#include <cstdio>
      #include <typeinfo>
      int main() {
        std::puts(typeid(char).name());
        return 0;
      }" QUICK_LINT_JS_HAVE_TYPEID)
    if (QUICK_LINT_JS_HAVE_TYPEID)
      set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS} -fchar8_t")
      # On some compilers, -fchar8_t appears to work, unless typeid is used.
      # Avoid -fchar8_t on these broken compilers.
      check_cxx_source_compiles(
        "#include <cstdio>
        #include <typeinfo>
        int main() {
          std::puts(typeid(char8_t).name());
          return 0;
        }" QUICK_LINT_JS_HAVE_WORKING_FCHAR8_T)
    else ()
      set(QUICK_LINT_JS_HAVE_WORKING_FCHAR8_T TRUE)
    endif ()

    if (QUICK_LINT_JS_HAVE_WORKING_FCHAR8_T)
      add_compile_options($<$<COMPILE_LANGUAGE:CXX>:-fchar8_t>)
    else ()
      check_cxx_compiler_flag(-fno-char8_t QUICK_LINT_JS_HAVE_FNO_CHAR8_T_FLAG)
      if (QUICK_LINT_JS_HAVE_FNO_CHAR8_T_FLAG)
        add_compile_options($<$<COMPILE_LANGUAGE:CXX>:-fno-char8_t>)
      endif ()
    endif ()
  endif ()
endfunction ()

function (quick_lint_js_enable_bigobj_if_supported TARGET)
  check_cxx_compiler_flag(/bigobj QUICK_LINT_JS_HAVE_BIGOBJ)
  if (QUICK_LINT_JS_HAVE_BIGOBJ)
    target_compile_options("${TARGET}" PRIVATE $<$<COMPILE_LANGUAGE:CXX>:/bigobj>)
  endif ()
endfunction ()

function (quick_lint_js_configure_exception_handling)
  if (MSVC)
    add_compile_options(/EHc-s-)
    add_definitions(-D_HAS_EXCEPTIONS=0)
  endif ()
  quick_lint_js_add_cxx_flag_if_supported(-fno-exceptions QUICK_LINT_JS_HAVE_FNO_EXCEPTIONS)
endfunction ()

# RTTI stands for Run-Time Type Information.
function (quick_lint_js_configure_rtti)
  quick_lint_js_add_cxx_flag_if_supported(-fno-rtti QUICK_LINT_JS_HAVE_FNO_RTTI)
  quick_lint_js_add_cxx_flag_if_supported(/GR- QUICK_LINT_JS_HAVE_GR_)
endfunction ()

function (quick_lint_js_set_cxx_standard)
  set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
  if (cxx_std_20 IN_LIST CMAKE_CXX_COMPILE_FEATURES)
    set(CMAKE_CXX_STANDARD 20)
  else ()
    set(CMAKE_CXX_STANDARD 17)
    quick_lint_js_check_designated_initializers(
      QUICK_LINT_JS_COMPILER_SUPPORTS_DESIGNATED_INITIALIZERS
    )
    if (NOT "${QUICK_LINT_JS_COMPILER_SUPPORTS_DESIGNATED_INITIALIZERS}")
      message(
        FATAL_ERROR
        "C++ compiler does not support designated initializers (either GNU extension or C++20)"
      )
    endif ()
    quick_lint_js_check_designated_initializers_with_defaults(
      QUICK_LINT_JS_COMPILER_SUPPORTS_DESIGNATED_INITIALIZERS_WITH_DEFAULTS
    )
    if (NOT "${QUICK_LINT_JS_COMPILER_SUPPORTS_DESIGNATED_INITIALIZERS_WITH_DEFAULTS}")
      message(
        FATAL_ERROR
        "C++ compiler does not support designated initializers with default values (either GNU extension or C++20)"
      )
    endif ()
  endif ()

  set(CMAKE_CXX_STANDARD "${CMAKE_CXX_STANDARD}" PARENT_SCOPE)
  set(CMAKE_CXX_STANDARD_REQUIRED "${CMAKE_CXX_STANDARD_REQUIRED}" PARENT_SCOPE)

  quick_lint_js_use_new_msvc_preprocessor()
endfunction ()

function (quick_lint_js_use_new_msvc_preprocessor)
  # https://docs.microsoft.com/en-us/cpp/preprocessor/preprocessor-experimental-overview?view=msvc-160
  quick_lint_js_add_c_cxx_flag_if_supported(/Zc:preprocessor QUICK_LINT_JS_HAVE_ZC_PREPROCESSOR)
  if (NOT QUICK_LINT_JS_HAVE_ZC_PREPROCESSOR_C OR NOT QUICK_LINT_JS_HAVE_ZC_PREPROCESSOR_CXX)
    quick_lint_js_add_c_cxx_flag_if_supported(/experimental:preprocessor QUICK_LINT_JS_HAVE_EXPERIMENTAL_PREPROCESSOR)
  endif ()

  # <Windows.h>, at least in SDK version 10.0.17763.0, has some bogus code when
  # /Zc:preprocessor is enabled. Work around it.
  quick_lint_js_get_supported_warning_options(/wd5105 WARNING_OPTIONS_TO_ADD)
  add_compile_options(${WARNING_OPTIONS_TO_ADD})
  add_definitions(-DWIN32_LEAN_AND_MEAN)
endfunction ()

function (quick_lint_js_add_warning_options_if_supported)
  cmake_parse_arguments("" "" "" "INTERFACE;PRIVATE;PUBLIC" ${ARGN})
  set(TARGETS "${_UNPARSED_ARGUMENTS}")

  foreach (STYLE INTERFACE PRIVATE PUBLIC)
    quick_lint_js_get_supported_warning_options(
      "${_${STYLE}}"
      WARNING_OPTIONS_TO_ADD
    )
    if (WARNING_OPTIONS_TO_ADD)
      target_compile_options("${TARGETS}" "${STYLE}" "${WARNING_OPTIONS_TO_ADD}")
    endif ()
  endforeach ()
endfunction ()

function (quick_lint_js_get_supported_warning_options WARNING_OPTIONS OUT_SUPPORTED_WARNING_OPTIONS)
  set(SUPPORTED_WARNING_OPTIONS)
  foreach (WARNING_OPTION IN LISTS WARNING_OPTIONS)
    quick_lint_js_warning_option_var_name("${WARNING_OPTION}" WARNING_OPTION_VAR_NAME)
    check_cxx_compiler_flag("${WARNING_OPTION}" "${WARNING_OPTION_VAR_NAME}")
    if ("${${WARNING_OPTION_VAR_NAME}}")
      list(APPEND SUPPORTED_WARNING_OPTIONS "${WARNING_OPTION}")
    endif ()
  endforeach ()
  set("${OUT_SUPPORTED_WARNING_OPTIONS}" "${SUPPORTED_WARNING_OPTIONS}" PARENT_SCOPE)
endfunction ()

function (quick_lint_js_warning_option_var_name WARNING_OPTION OUT_VAR_NAME)
  set(VAR_NAME "${WARNING_OPTION}")
  string(TOUPPER "${VAR_NAME}" VAR_NAME)
  string(REGEX REPLACE "[-/]" _ VAR_NAME "${VAR_NAME}")
  set(VAR_NAME "QUICK_LINT_JS_HAVE_WARNING_OPTION_${VAR_NAME}")
  set("${OUT_VAR_NAME}" "${VAR_NAME}" PARENT_SCOPE)
endfunction ()

function (quick_lint_js_work_around_implicit_link_directories)
  # HACK(strager): Work around weird CMake behaviors when mixing multiple
  # languages (e.g. C and C++).
  #
  # Let's say the user asks CMake to compile C code with compiler X and C++ code
  # with compiler Y. Let's say a CMake file says we need to link an executable
  # which mixes C code and C++ code. CMake does the following:
  #
  # 1. Ask compiler X for its implicit linker flags.
  # 2. Use compiler Y to link the executable. Use some of compiler X's implicit
  #    linker flags.
  #
  # This is a problem when using compiler X's flags break compiler Y in some
  # way. In the wild, we observed that, if compiler X is GCC 7 and compiler Y is
  # GCC 8, CMake links the executable with GCC 8 but uses GCC 7's path to
  # libstdc++. This means that sources are compiled with GCC 8's libstdc++ but
  # link with GCC 7's libstdc++.
  #
  # Work around this by having compiler Y's flags take priority over compiler
  # X's flags. We need to clear CMAKE_<LANG>_IMPLICIT_LINK_DIRECTORIES;
  # otherwise, CMake ignores the link_directories command because it thinks the
  # compiler will already include those directories.
  #
  # See this issue for more details:
  # https://github.com/quick-lint/quick-lint-js/issues/9
  link_directories(${CMAKE_CXX_IMPLICIT_LINK_DIRECTORIES})
  set(CMAKE_CXX_IMPLICIT_LINK_DIRECTORIES "" PARENT_SCOPE)
endfunction ()

function (quick_lint_js_have_charconv OUT_VAR)
  check_cxx_source_compiles(
    "#include <charconv>
    int main() {
      char out[10];
      int value = 42;
      (void)std::to_chars(out, out + 10, value);
      return 0;
    }" QUICK_LINT_JS_HAVE_CHARCONV_AND_STD_TO_CHARS)
  set(
    "${OUT_VAR}"
    "${QUICK_LINT_JS_HAVE_CHARCONV_AND_STD_TO_CHARS}"
    PARENT_SCOPE
  )
endfunction ()

function (quick_lint_js_add_c_cxx_flag_if_supported FLAG VAR)
  quick_lint_js_add_c_flag_if_supported("${FLAG}" "${VAR}_C")
  quick_lint_js_add_cxx_flag_if_supported("${FLAG}" "${VAR}_CXX")
endfunction ()

function (quick_lint_js_add_c_flag_if_supported FLAG VAR)
  check_c_compiler_flag("${FLAG}" "${VAR}")
  if ("${${VAR}}")
    add_compile_options($<$<COMPILE_LANGUAGE:C>:${FLAG}>)
  endif ()
endfunction ()

function (quick_lint_js_add_cxx_flag_if_supported FLAG VAR)
  check_cxx_compiler_flag("${FLAG}" "${VAR}")
  if ("${${VAR}}")
    add_compile_options($<$<COMPILE_LANGUAGE:CXX>:${FLAG}>)
  endif ()
endfunction ()

function (quick_lint_js_add_cxx_linker_flag_if_supported FLAG VAR)
  list(APPEND CMAKE_REQUIRED_LIBRARIES "${FLAG}")
  check_cxx_source_compiles("int main() { return 0; }" "${VAR}")
  if ("${${VAR}}")
    if (CMAKE_VERSION VERSION_GREATER_EQUAL 3.18)
      link_libraries("$<$<LINK_LANGUAGE:CXX>:${FLAG}>")
    else ()
      link_libraries("${FLAG}")
    endif ()
  endif ()
endfunction ()

function (quick_lint_js_enable_dead_code_stripping)
  quick_lint_js_add_c_cxx_flag_if_supported(-fdata-sections QUICK_LINT_JS_HAVE_FDATA_SECTIONS)
  quick_lint_js_add_c_cxx_flag_if_supported(-ffunction-sections QUICK_LINT_JS_HAVE_FFUNCTION_SECTIONS)
  quick_lint_js_add_cxx_linker_flag_if_supported(-Wl,--gc-sections QUICK_LINT_JS_HAVE_GC_SECTIONS)
endfunction ()

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
