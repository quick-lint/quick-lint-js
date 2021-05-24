# Vendor

This vendor directory contains third-party software.

See ADR003-Vendor-sources.md for usage of and rational for this directory.

## benchmark

The benchmark directory contains Google Benchmark, a performance measurement
framework for C++ code.

Copyright: Copyright 2015 Google Inc. All rights reserved.; Copyright 2016 Ismael Jimenez Martinez. All rights reserved.
Download URL: https://codeload.github.com/google/benchmark/tar.gz/v1.5.1
Download date: August 20, 2020
Git commit: 8039b4030795b1c9b8cedb78e3a2a6fb89574b6e
License file: LICENSE
License type: Apache-2.0
Location: benchmark
Project URL: https://github.com/google/benchmark
Release URL: https://github.com/google/benchmark/releases/tag/v1.5.1
Release date: June 9, 2020
Version: v1.5.1

## boost

The boost directory contains Boost, free peer-reviewed portable C++ source
libraries.

Many directories and files have been deleted to reduce storage consumption.

The following patches have been manually applied:

* boost-static-var.patch

Copyright: various
Download URL: https://dl.bintray.com/boostorg/release/1.74.0/source/boost_1_74_0.tar.bz2
Download date: August 25, 2020
Git commit: a7090e8ce184501cfc9e80afa6cafb5bfd3b371c (et al)
License file: boost/LICENSE_1_0.txt
License type: BSL-1.0
Location: boost
Project URL: https://www.boost.org/
Release URL: https://www.boost.org/users/history/version_1_74_0.html
Release date: August 14th, 2020
Version: 1.74.0

## googletest

The googletest directory contains Google Test, a testing framework for C++
applications, and Google Mock, its companion mocking framework.

The following patches have been manually applied:

* googletest-result-of.patch
* googletest-werror.patch

Copyright: Copyright 2008, Google Inc.
Download URL: https://github.com/google/googletest/archive/release-1.10.0.tar.gz
Download date: July 27, 2020
Git commit: 703bd9caab50b139428cea1aaff9974ebee5742e + patches
License file: googletest/LICENSE, googletest/googlemock/LICENSE, googletest/googlemock/scripts/generator/LICENSE, googletest/googletest/LICENSE
License type: Apache-2.0, BSD-3-Clause
Location: googletest
Project URL: https://github.com/google/googletest
Release URL: https://github.com/google/googletest/releases/tag/release-1.10.0
Release date: October 3, 2019
Version: 1.10.0

## jsoncpp

The jsoncpp directory contains JsonCpp, a C++ library that allows manipulating
JSON values.

The following directories have been deleted to reduce storage consumption:

* jsoncpp/src/test_lib_json/
* jsoncpp/test/

Copyright: Public Domain; Copyright (c) 2007-2010 Baptiste Lepilleur and The JsonCpp Authors
Download URL: https://github.com/open-source-parsers/jsoncpp/archive/1.9.4.tar.gz
Download date: October 18, 2020
Git commit: 9059f5cad030ba11d37818847443a53918c327b1
License file: jsoncpp/LICENSE
License type: Public Domain; MIT
Location: jsoncpp
Project URL: https://github.com/open-source-parsers/jsoncpp
Release URL: https://github.com/open-source-parsers/jsoncpp/releases/tag/1.9.4
Release date: September 25, 2020
Version: 1.9.4

## simdjson

The simdjson directory contains simdjson, a C++ library for parsing JSON.

The following directories and files have been deleted to reduce storage
consumption:

* simdjson/Doxyfile
* simdjson/benchmark/
* simdjson/dependencies/
* simdjson/doc/
* simdjson/fuzz/
* simdjson/images/
* simdjson/jsonchecker/
* simdjson/jsonexamples/
* simdjson/singleheader/
* simdjson/tests/

The following patches have been manually applied:

* simdjson-cmake-version.patch

Copyright: Copyright 2018-2019 The simdjson authors; various
Download URL: https://github.com/simdjson/simdjson/archive/v0.9.4.tar.gz
Download date: May 24, 2020
Git commit: 076f41ae4b7c3a3eeed8a043dfb1143abe87ddf0
License file: simdjson/LICENSE; simdjson/src/to_chars.cpp; simdjson/windows/toni_ronnko_dirent.h; simdjson/windows/getopt.h; simdjson/include/simdjson/internal/isadetection.h
License type: Apache-2.0; MIT; MIT; MIT(Old Style with legal disclaimer 2)/BSD-2-Clause; BSD-3-Clause
Location: simdjson
Project URL: https://github.com/simdjson/simdjson
Release URL: https://github.com/simdjson/simdjson/releases/tag/v0.9.4
Release date: May 20, 2021
Version: 0.9.4
