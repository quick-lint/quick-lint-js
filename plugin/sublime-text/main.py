# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

import sublime
import sublime_plugin

from . import c_api


class QuickLintJsListener(sublime_plugin.ViewEventListener):
    @classmethod
    def is_applicable(cls, settings):
        syntax = settings.get("syntax")
        if "JavaScript" in syntax:
            return True
        return False

    def __init__(self, view):
        self.view = view
        self.parser = c_api.Parser()
        self.parser.init()

    def on_load(self):
        viewsize = self.view.size()
        allregion = sublime.Region(0, viewsize)
        allcontent = self.view.substr(allregion).encode("utf-8")
        diagnostics = self.parser.set_text_and_lint(allcontent, viewsize)
        regions = [
            sublime.Region(d.begin_offset, d.end_offset)
            for d in c_api.iterdiags(diagnostics)
        ]
        self.add_squiggly_underlines(regions)

    def on_modified(self):
        self.on_load()

    def on_close(self):
        self.parser.dealloc()

    def add_squiggly_underlines(self, regions):
        flags = (
            sublime.DRAW_SQUIGGLY_UNDERLINE
            | sublime.DRAW_NO_FILL
            | sublime.DRAW_NO_OUTLINE
        )
        self.view.add_regions("0", regions, "invalid.illegal", "", flags)


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
