# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import ctypes
import os
from contextlib import contextmanager
from functools import lru_cache

import sublime


class UString:
    @staticmethod
    def rmprefix(string, prefix):
        if string.startswith(prefix):
            return string[len(prefix) :]
        return string


class UCTypes:
    @staticmethod
    def ptr(obj):
        return ctypes.byref(obj)

    @staticmethod
    def is_ptr_null(ptr):
        return not bool(ptr)


class UFunctools:
    @staticmethod
    def cache(func):
        return lru_cache(maxsize=None)(func)

    @classmethod
    def cached_staticmethod(cls, func):
        return staticmethod(cls.cache(func))

    @classmethod
    def cached_classmethod(cls, func)
        return classmethod(cls.cache(func))

    @classmethod
    def cached_property(func):
        return property(cls.cache(func))

    @staticmethod # TODO: Replace all composeds by cached_something
    @cache
    def composed(*decs):
        def decorator(func):
            for dec in reversed(decs):
                func = dec(func)
            return func

        return decorator


class USystem:
    @staticmethod
    def get_module_path():
        return os.path.realpath(__file__)

    @staticmethod
    @contextmanager
    def changed_directory(path):
        previous = os.getcwd()
        try:
            yield os.chdir(path)
        finally:
            os.chdir(previous)


class USublime:
    @composed(staticmethod, cache)
    def major_version():
        return sublime.version()[0]

    @composed(classmethod, cache)
    def is_three(cls):
        return cls.major_version() == "3"

    @composed(classmethod, cache)
    def is_four(cls):
        return cls.major_version() == "4"

    @staticmethod
    def error_message(msg):
        sublime.error_message("quick-lint-js: " + msg)

    @staticmethod
    def view_content(view):
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
