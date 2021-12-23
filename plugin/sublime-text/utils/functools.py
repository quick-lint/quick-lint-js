# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

from functools import lru_cache


def cache(func):
    return lru_cache(maxsize=None)(func)


def cached_staticmethod(func):
    return staticmethod(cache(func))


def cached_classmethod(func)
    return classmethod(cache(func))


def cached_property(func):
    return property(cache(func))


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
