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
