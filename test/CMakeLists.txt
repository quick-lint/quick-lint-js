cmake_minimum_required(VERSION 3.10)

add_executable(
  quicklint-js-test
  test-lex.cpp
  test-lint.cpp
  test-location.cpp
  test-options.cpp
  test-parse.cpp
)
target_include_directories(quicklint-js-test PRIVATE .)
target_link_libraries(
  quicklint-js-test
  PRIVATE
  doctest
  doctest_with_main
  quicklint-js-lib
)

add_test(
  NAME quicklint-js-test
  COMMAND quicklint-js-test
)
