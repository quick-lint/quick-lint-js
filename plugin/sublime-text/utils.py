# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

################################################################################
## utils
################################################################################

import contextlib
import os

import sublime


## ctypes ######################################################################


def is_pointer_null(ptr):
    return not bool(ptr)


## str #########################################################################


def get_first_character(str):
    return str[0]


def remove_prefix(str, prefix):
    if str.startswith(prefix):
        return str[len(prefix) :]
    return str


## os ##########################################################################


def get_module_path():
    return os.path.realpath(__file__)


@contextlib.contextmanager
def changed_directory(path):
    previous_path = os.getcwd()
    try:
        yield os.chdir(path)
    finally:
        os.chdir(previous_path)


## sublime #####################################################################


def get_major_version():
    return get_first_character(sublime.version())


def is_version_three():
    return get_major_version() == "3"


def is_version_four():
    return get_major_version() == "4"


def plugin_error_message(msg):
    sublime.error_message("quick-lint-js: " + msg)


def view_entire_content(view):
    region = sublime.Region(0, view.size())
    return view.substr(region)


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
