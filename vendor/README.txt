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

The boost directory was generated using Boost's [bcp
tool](https://www.boost.org/doc/libs/1_76_0/tools/bcp/doc/html/index.html). To
regenerate the boost directory, run the following commands inside a Boost
release archive:

    $ qljs_vendor=/PATH/TO/quick-lint-js/vendor/
    $ cd /PATH/TO/BOOST/
    $ ./bootstrap.sh
    $ cd tools/bcp/
    $ ../../b2
    $ cd ../../
    $ rm -r "${qljs_vendor}/boost/"
    $ mkdir "${qljs_vendor}/boost/"
    $ boost_libs="boost/container/pmr/global_resource.hpp boost/container/pmr/memory_resource.hpp boost/container/pmr/monotonic_buffer_resource.hpp boost/container/pmr/polymorphic_allocator.hpp boost/container/pmr/unsynchronized_pool_resource.hpp boost/container/small_vector.hpp boost/json/parse.hpp boost/json.hpp boost/json/src.hpp boost/throw_exception.hpp"
    $ eval ./dist/bin/bcp ${boost_libs} "${qljs_vendor}/boost/"
    $ eval ./dist/bin/bcp --report ${boost_libs} "${qljs_vendor}/boost/report.html"
    $ cp LICENSE_1_0.txt "${qljs_vendor}/boost/"
    $ rm -r "${qljs_vendor}/boost/usr/"

Additionally, the following patches have been manually applied:

* boost-json-pmr.patch
* boost-static-var.patch

Additionally, the following directories and files have been deleted to reduce
storage consumption:

* boost/boost/align/
* boost/boost/io/
* boost/boost/predef/
* boost/boost/shared_ptr.hpp
* boost/boost/smart_ptr/
* boost/boost/system/
* boost/boost/type_traits/
* boost/boost/utility/
* boost/boost/winapi/

Copyright: various
Download URL: https://boostorg.jfrog.io/artifactory/main/release/1.76.0/source/boost_1_76_0.tar.bz2
Download date: June 27, 2021
Git commit: ccb2ab3b4384a16deafaa849de509bce2b2cbada (et al)
License file: boost/LICENSE_1_0.txt
License type: BSL-1.0
Location: boost
Project URL: https://www.boost.org/
Release URL: https://www.boost.org/users/history/version_1_76_0.html
Release date: April 16th, 2021
Version: 1.76.0

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

## node

The node directory contains Node.js, a JavaScript runtime environment.

Only the following files have been kept to reduce storage consumption:

* node/AUTHORS
* node/LICENSE
* node/README.md
* node/src/js_native_api.h
* node/src/js_native_api_types.h
* node/src/node_api.h
* node/src/node_api_types.h

Copyright: Copyright Node.js contributors. All rights reserved.; various
Download URL: https://nodejs.org/download/release/v14.17.3/node-v14.17.3.tar.xz
Download date: July 25, 2021
Git commit: 9fb7b48c5ed7707fbc4007a2e3fc99aec7587d8b
License file: node/LICENSE
License type: MIT; various
Location: node
Project URL: https://nodejs.org/
Release URL: https://nodejs.org/download/release/v14.17.3/
Release date: July 5, 2021
Version: v14.17.3

## node-addon-api

The node-addon-api directory contains node-addon-api, header-only C++ wrapper
classes which simplify the use of the C based Node-API provided by Node.js when
using C++.

The following directories have been deleted to reduce storage consumption:

* node-addon-api/.github/
* node-addon-api/benchmark/
* node-addon-api/doc/
* node-addon-api/test/
* node-addon-api/tools/

Copyright: Copyright (c) 2017 Node.js API collaborators
Download URL: https://github.com/nodejs/node-addon-api/archive/refs/tags/4.0.0.tar.gz
Download date: July 25, 2021
Git commit: ad76ad07f914fab02be5778ec67485916c4626d9
License file: node-addon-api/LICENSE.md
License type: MIT
Location: node-addon-api
Project URL: https://github.com/nodejs/node-addon-api
Release URL: https://github.com/nodejs/node-addon-api/releases/tag/4.0.0
Release date: June 15, 2021
Version: 4.0.0

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
* simdjson-iostream.patch
* simdjson-reenter-child-ub.patch

Copyright: Copyright 2018-2019 The simdjson authors; various
Download URL: https://github.com/simdjson/simdjson/archive/refs/tags/v0.9.6.tar.gz
Download date: July 17, 2021
Git commit: e9b893ff1b13c6a70135827c62b3f3d65938d135
License file: simdjson/LICENSE; simdjson/src/to_chars.cpp; simdjson/windows/toni_ronnko_dirent.h; simdjson/windows/getopt.h; simdjson/include/simdjson/internal/isadetection.h
License type: Apache-2.0; MIT; MIT; MIT(Old Style with legal disclaimer 2)/BSD-2-Clause; BSD-3-Clause
Location: simdjson
Project URL: https://github.com/simdjson/simdjson
Release URL: https://github.com/simdjson/simdjson/releases/tag/v0.9.6
Release date: June 6, 2021
Version: 0.9.6
