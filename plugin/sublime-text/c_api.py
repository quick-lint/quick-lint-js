class Parser:
    def lint(self, content):
        slices = []
        substr = "ipsum"
        for start in self._findall(content, substr):
            end = start + len(substr)
            slices.append(slice(start, end))
        return slices

    def _findall(self, str, sub):
        index = str.find(sub)
        while index != -1:
            yield index
            index = str.find(sub, index + 1)
