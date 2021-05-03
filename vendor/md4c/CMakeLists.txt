
cmake_minimum_required(VERSION 3.4)
project(MD4C C)

set(MD_VERSION_MAJOR 0)
set(MD_VERSION_MINOR 4)
set(MD_VERSION_RELEASE 7)
set(MD_VERSION "${MD_VERSION_MAJOR}.${MD_VERSION_MINOR}.${MD_VERSION_RELEASE}")

set(PROJECT_VERSION "${MD_VERSION}")
set(PROJECT_URL "https://github.com/mity/md4c")

if(WIN32)
    # On Windows, given there is no standard lib install dir etc., we rather
    # by default build static lib.
    option(BUILD_SHARED_LIBS "help string describing option" OFF)
else()
    # On Linux, MD4C is slowly being adding into some distros which prefer
    # shared lib.
    option(BUILD_SHARED_LIBS "help string describing option" ON)
endif()

add_definitions(
    -DMD_VERSION_MAJOR=${MD_VERSION_MAJOR}
    -DMD_VERSION_MINOR=${MD_VERSION_MINOR}
    -DMD_VERSION_RELEASE=${MD_VERSION_RELEASE}
)

set(CMAKE_CONFIGURATION_TYPES Debug Release RelWithDebInfo MinSizeRel)
if("${CMAKE_BUILD_TYPE}" STREQUAL "")
    set(CMAKE_BUILD_TYPE $ENV{CMAKE_BUILD_TYPE})

    if("${CMAKE_BUILD_TYPE}" STREQUAL "")
        set(CMAKE_BUILD_TYPE "Release")
    endif()
endif()


if(${CMAKE_C_COMPILER_ID} MATCHES GNU|Clang)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall")
elseif(MSVC)
    # Disable warnings about the so-called unsecured functions:
    add_definitions(/D_CRT_SECURE_NO_WARNINGS)

    # Specify proper C runtime library:
    string(REGEX REPLACE "/M[DT]d?" "" CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
    string(REGEX REPLACE "/M[DT]d?" "" CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE}")
    string(REGEX REPLACE "/M[DT]d?" "" CMAKE_C_FLAGS_RELWITHDEBINFO "{$CMAKE_C_FLAGS_RELWITHDEBINFO}")
    string(REGEX REPLACE "/M[DT]d?" "" CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL}")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /MTd")
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /MT")
    set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELEASE} /MT")
    set(CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_RELEASE} /MT")
endif()

include(GNUInstallDirs)

add_subdirectory(src)
add_subdirectory(md2html)
