# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import subprocess


def cmd(args):
    subprocess.run(args)


def cmake(sourcepath, buildpath, generator, cache_entries):
    cache_entries = [f"-D {key}={val}" for key, val in cache_entries]
    cmd(["cmake", "-S", sourcepath, "-B", buildpath, "-G", generator, *cache_entries])


def cmake_build(directory, target):
    cmd(["cmake", "--build", directory, "--target", target])


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
