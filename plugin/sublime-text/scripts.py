# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

# TODO: use version from toplevel to create package name

from os import path


def build(version, type="release"):
    buildpath = "build-sublime-text-" + version
    cache_entries = {"CMAKE_BUILD_TYPE": type, "QLJS_SUBLIME_TEXT_VERSION": version}
    if path.exists(buildpath):
        cmake("../..", buildpath, "ninja", cache_entries)
    cmake_build(buildpath, "quick-lint-js-sublime-text")


def start():
    pass


def clean():
    pass


def install():
    pass


def lint():
    pass


def format():
    pass


# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew Glazar
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
