include(CMakeFindDependencyMacro)
if("ON")
  find_dependency(Threads)
endif()

include("${CMAKE_CURRENT_LIST_DIR}/simdjsonTargets.cmake")
