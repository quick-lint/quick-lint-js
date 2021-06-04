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
        self.on_modified()

    def on_load(self):
        self.on_modified()

    def on_modified(self):
        viewsize = self.view.size()
        allregion = sublime.Region(0, viewsize)
        allcontent = self.view.substr(allregion).encode("utf-8")
        self.parser.set_text(allcontent, viewsize)
        self.set_diagnostics(self.parser.lint())
        self._add_squiggly_underlines(self.get_diagnostics())

    def on_hover(self, point, hover_zone):
        if hover_zone == sublime.HOVER_TEXT:
            diagnostics = self.safe_get_diagnostics()
            self._add_popup(diagnostics, point)

    def get_diagnostics(self):
        return self.diagnostics

    def safe_get_diagnostics(self):
        diagnostics = getattr(self, "diagnostics", None)
        if diagnostics is None:
            self.on_modified()
            diagnostics = self.diagnostics
        return diagnostics

    def set_diagnostics(self, diagnostics):
        self.diagnostics = []
        for d in c_api.iterdiags(diagnostics):
            self.diagnostics.append(d)

    def _add_squiggly_underlines(self, diagnostics):
        error_regions = []
        warning_regions = []
        for d in diagnostics:
            region = sublime.Region(d.begin_offset, d.end_offset)
            if d.severity == c_api.SeverityEnumeration.ERROR:
                error_regions.append(region)
            elif d.severity == c_api.SeverityEnumeration.WARNING:
                warning_regions.append(region)

        flags = (
            sublime.DRAW_SQUIGGLY_UNDERLINE
            | sublime.DRAW_NO_FILL
            | sublime.DRAW_NO_OUTLINE
        )
        self.view.add_regions("1", error_regions, "invalid.illegal", "", flags)
        self.view.add_regions("2", warning_regions, "invalid.deprecated", "", flags)

    def _add_popup(self, diagnostics, point):
        for d in diagnostics:
            if d.begin_offset <= point and point <= d.end_offset:
                content = "%s [%s]" % (
                    d.message.decode("utf-8"),
                    d.code.decode("utf-8"),
                )
                flags = sublime.HIDE_ON_MOUSE_MOVE_AWAY
                location = d.begin_offset
                max_width, max_height = self.view.viewport_extent()
                self.view.show_popup(content, flags, location, max_width, max_height)


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
