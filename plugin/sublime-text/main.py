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

    def on_load(self):
        allregion = sublime.Region(0, self.view.size())
        allcontent = self.view.substr(allregion)
        slices = self.parser.lint(allcontent)
        regions = [sublime.Region(s.start, s.stop) for s in slices]
        self.add_squiggly_underlines(regions)

    def on_modified(self):
        self.on_load()

    def add_squiggly_underlines(self, regions):
        flags = (
            sublime.DRAW_SQUIGGLY_UNDERLINE
            | sublime.DRAW_NO_FILL
            | sublime.DRAW_NO_OUTLINE
        )
        self.view.add_regions("0", regions, "invalid.illegal", "", flags)
