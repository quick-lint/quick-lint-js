# Vendor

This vendor directory contains third-party software.

## gnulib

The gnulib directory contains Gnulib, the GNU portability library.

The following Gnulib modules have been imported:

* getopt-gnu (and transitive dependencies)

Run the following commands to import:

    $ gnulib-tool --lgpl --import getopt-gnu

For details on importing or upgrading Gnulib, see Gnulib's documentation:
https://www.gnu.org/software/gnulib/manual/html_node/Invoking-gnulib_002dtool.html

Copyright: Copyright (C) 2002-2020 Free Software Foundation, Inc.
Download URL: git://git.savannah.gnu.org/gnulib.git
Download date: July 28, 2020
Git commit: 5e50baa16ef90204d9048a9e2f23c5a538955121
License file: N/A
License type: FSFULLR, GPL-3.0-or-later, LGPL-2.0-or-later, LGPL-2.1-or-later
Location: gnulib
Project URL: https://www.gnu.org/software/gnulib/
Release URL: N/A
Release date: N/A
Version: N/A

## googletest

The googletest directory contains Google Test, a testing framework for C++
applications, and Google Mock, its companion mocking framework.

The following patches have been manually applied:

* googletest-result-of.patch

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
