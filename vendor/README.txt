# Vendor

This vendor directory contains third-party software.

See ADR003-Vendor-sources.md for usage of and rational for this directory.

## benchmark

The benchmark directory contains Google Benchmark, a performance measurement
framework for C++ code.

Copyright: Copyright 2015 Google Inc. All rights reserved.; Copyright 2016 Ismael Jimenez Martinez. All rights reserved.
Download URL: https://github.com/google/benchmark/archive/refs/tags/v1.6.1.tar.gz
Download date: April 18, 2022
Git commit: 0d98dba29d66e93259db7daa53a9327df767a415
License file: LICENSE
License type: Apache-2.0
Location: benchmark
Project URL: https://github.com/google/benchmark
Release URL: https://github.com/google/benchmark/releases/tag/v1.6.1
Release date: January 10, 2022
Version: v1.6.1

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
    $ boost_libs="boost/container/pmr/memory_resource.hpp boost/container/pmr/polymorphic_allocator.hpp boost/container/pmr/unsynchronized_pool_resource.hpp boost/json/parse.hpp boost/json.hpp boost/json/src.hpp boost/throw_exception.hpp"
    $ eval ./dist/bin/bcp ${boost_libs} "${qljs_vendor}/boost/"
    $ eval ./dist/bin/bcp --report ${boost_libs} "${qljs_vendor}/boost/report.html"
    $ cp LICENSE_1_0.txt "${qljs_vendor}/boost/"
    $ rm -r "${qljs_vendor}/boost/usr/"

Additionally, the following patches have been manually applied:

* boost-static-var.patch

Additionally, the following directories and files have been deleted to reduce
storage consumption:

* boost/boost/shared_ptr.hpp
* boost/boost/smart_ptr/

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

* googletest-werror.patch

Copyright: Copyright 2008, Google Inc.
Download URL: https://codeload.github.com/google/googletest/tar.gz/refs/tags/release-1.11.0
Download date: September 20, 2021
Git commit: e2239ee6043f73722e7aa812a459f54a28552929
License file: googletest/LICENSE, googletest/googlemock/scripts/generator/LICENSE
License type: Apache-2.0, BSD-3-Clause
Location: googletest
Project URL: https://github.com/google/googletest
Release URL: https://github.com/google/googletest/releases/tag/release-1.11.0
Release date: June 11, 2021
Version: 1.11.0

## mongoose

The benchmark mongoose contains Mongoose, an HTTP server library.

Only the following files and directories have been kept to reduce storage
consumption:

* mongoose/LICENSE
* mongoose/README.md
* mongoose/docs/
* mongoose/src/

Copyright: Copyright (c) 2004-2013 Sergey Lyubka; Copyright (c) 2013-2023 Cesanta Software Limited; All rights reserved
Download URL: https://github.com/cesanta/mongoose/archive/refs/tags/7.9.tar.gz
Download date: March 16, 2023
Git commit: 4236405b90e051310aadda818e21c811e404b4d8
License file: mongoose/LICENSE
License type: GPL-2.0-only
Location: mongoose
Project URL: https://mongoose.ws/
Release URL: https://github.com/cesanta/mongoose/releases/tag/7.9
Release date: January 20, 2023
Version: 7.9

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

* simdjson-asan-build-perf.patch
* simdjson-cmake-version.patch
* simdjson-smaller-number-parsing.patch
* simdjson-warnings.patch

Copyright: Copyright 2018-2019 The simdjson authors; various
Download URL: https://codeload.github.com/simdjson/simdjson/tar.gz/refs/tags/v1.0.0
Download date: September 28, 2021
Git commit: 3bd8b0b575f43403705dcce57d427944c11421f8
License file: simdjson/LICENSE; simdjson/src/to_chars.cpp; simdjson/windows/toni_ronnko_dirent.h; simdjson/windows/getopt.h; simdjson/include/simdjson/internal/isadetection.h; simdjson/include/simdjson/nonstd/string_view.hpp
License type: Apache-2.0; MIT; MIT; MIT(Old Style with legal disclaimer 2)/BSD-2-Clause; BSD-3-Clause; BSL-1.0
Location: simdjson
Project URL: https://github.com/simdjson/simdjson
Release URL: https://github.com/simdjson/simdjson/releases/tag/v1.0.0
Release date: September 7, 2021
Version: 1.0.0
