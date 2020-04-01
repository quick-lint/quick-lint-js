cmake_minimum_required(VERSION 3.10)
project(quicklint-js)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)

add_subdirectory(vendor)

add_subdirectory(src)
include(CTest)
if (BUILD_TESTING)
  add_subdirectory(test)
endif ()
