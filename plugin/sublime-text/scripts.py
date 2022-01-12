# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

################################################################################
## scripts
################################################################################


# TODO: use version from toplevel to create package name


class Scripts:
    def build(version):
        sversion = str(version)
        buildpath = "build-sublime-text-" + sversion
        cache_entries = {
            "CMAKE_BUILD_TYPE": "Release",
            "QUICK_LINT_JS_SUBLIME_TEXT_VERSION": sversion,
        }
        cmake("../..", buildpath, "ninja", cache_entries)

    def start(self):
        pass

    def clean(self):
        pass

    def install(self):
        pass

    def lint(self):
        pass

    def format(self):
        pass

    def _dependencies():
        pass

    def __call__(self):
        pass

    @staticmethod
    def main():
        pass


################################################################################
## internals
################################################################################


def main():
    pass


if __name__ == "__main__":
    main()


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
