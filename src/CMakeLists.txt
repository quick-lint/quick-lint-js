cmake_minimum_required(VERSION 3.10)

add_executable(
  quicklint-js
  main.cpp
)
target_link_libraries(quicklint-js PRIVATE quicklint-js-lib)

add_library(
  quicklint-js-lib
  lex.cpp
)
target_include_directories(quicklint-js-lib PUBLIC .)
