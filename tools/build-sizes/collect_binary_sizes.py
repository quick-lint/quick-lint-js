#!/usr/bin/env python3

# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import argparse
import json
import logging
import pathlib
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import typing
import unittest
import zipfile

logger = logging.getLogger(__name__)


def main() -> None:
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument("paths", nargs="+", metavar="PATH")
    args = parser.parse_args()

    sizes = Sizes()
    for user_path in args.paths:
        recorder = FileOrDirectoryRecorder(
            name=(user_path,), path=pathlib.Path(user_path)
        )
        recorder.record(sizes=sizes)

    out = {
        "readme": parse_readme_files(args.paths),
        "sizes": sizes.to_json_like(),
    }
    json.dump(out, sys.stdout, indent=2)


def parse_readme_files(paths: typing.List[str]) -> typing.Dict[str, str]:
    result = {}
    for path in paths:
        path = pathlib.Path(path)
        try:
            readme_text = (path / "README").read_text()
        except (FileNotFoundError, NotADirectoryError):
            continue
        for line in readme_text.splitlines():
            match = re.match(r"^(?P<key>\S+): (?P<value>.*)$", line)
            if match is not None:
                result[match.group("key")] = match.group("value")
    return result


def classify_path(path: typing.Union[pathlib.Path, pathlib.PureWindowsPath]):
    file_type_by_extension = {
        ".deb": DebianPackageRecorder,
        ".exe": LLVMSizeRecorder,
        ".js": RegularFileRecorder,
        ".node": LLVMSizeRecorder,
        ".tgz": TarFileRecorder,
        ".vsix": ZIPFileRecorder,
        ".wasm": LLVMSizeRecorder,
        ".zip": ZIPFileRecorder,
    }
    if path.suffix == "":
        if path.name == "quick-lint-js":
            # ELF and Mach-O binaries.
            return LLVMSizeRecorder
    if path.suffixes[-2:] in (".tar.gz", ".tar.xz"):
        return TarFileRecorder
    return file_type_by_extension.get(path.suffix)


class Sizes:
    def __init__(self) -> None:
        self.__sizes = []

    def record_size(
        self,
        name: typing.Tuple[str, ...],
        type: str,
        size: int,
    ) -> None:
        self.__sizes.append(
            {
                "name": name,
                "type": type,
                "size": size,
            }
        )

    def to_json_like(self) -> typing.Any:
        return self.__sizes


class Recorder:
    def __init__(self, name: typing.Tuple[str, ...], path: pathlib.Path) -> None:
        self._name = name
        self._path = path


class RegularFileRecorder(Recorder):
    def __init__(
        self, name: typing.Tuple[str, ...], path: pathlib.Path, type: str = "file"
    ) -> None:
        super().__init__(name=name, path=path)
        self.__type = type

    def record(self, sizes: Sizes) -> None:
        sizes.record_size(
            name=self._name, size=self._path.stat().st_size, type=self.__type
        )


class FileOrDirectoryRecorder(Recorder):
    def record(self, sizes: Sizes) -> None:
        if self._path.is_file():
            recorder_factory = classify_path(self._path)
            if recorder_factory is None:
                recorder_factory = RegularFileRecorder
            recorder = recorder_factory(name=self._name, path=self._path)
            recorder.record(sizes=sizes)
        else:
            for p in self._path.glob("**/*"):
                if not p.is_file():
                    continue
                recorder_factory = classify_path(p)
                if recorder_factory is None:
                    continue
                recorder = recorder_factory(
                    name=self._name + (str(p.relative_to(self._path)),), path=p
                )
                recorder.record(sizes=sizes)


class DebianPackageRecorder(Recorder):
    def record(self, sizes: Sizes) -> None:
        RegularFileRecorder(name=self._name, path=self._path, type="archive").record(
            sizes=sizes
        )
        with tempfile.TemporaryDirectory() as temp_dir:
            try:
                subprocess.check_call(["dpkg-deb", "--extract", self._path, temp_dir])
            except FileNotFoundError:
                logger.warn(
                    "skipping recording of %s because dpkg-deb is not installed",
                    self._name,
                )
                return
            FileOrDirectoryRecorder(
                name=self._name, path=pathlib.Path(temp_dir)
            ).record(sizes=sizes)


class LLVMSizeRecorder(Recorder):
    def record(self, sizes: Sizes) -> None:
        if self.is_npm_package_dummy_file():
            return

        RegularFileRecorder(name=self._name, path=self._path, type="exe").record(
            sizes=sizes
        )
        try:
            process = subprocess.run(
                ["llvm-size", "--format=sysv", "--", self._path],
                stdout=subprocess.PIPE,
                encoding="utf-8",
            )
        except FileNotFoundError:
            logger.warn(
                "skipping recording of %s because llvm-size is not installed",
                self._name,
            )
            return
        try:
            process.check_returncode()
        except subprocess.CalledProcessError as e:
            print(
                f"error: failed to parse: {self._name} {self._path} {e}",
                file=sys.stderr,
            )
            raise e
        for (section, section_size) in self.parse_llvm_size_output(process.stdout):
            sizes.record_size(
                name=self._name + (section,), size=section_size, type="section"
            )

    @staticmethod
    def parse_llvm_size_output(output: str) -> typing.List[typing.Tuple[str, int]]:
        sizes = []
        for line in output.splitlines():
            match = re.match(
                r"^(?P<section>\S+)\s+(?P<size>\d+)\s+(?P<address>\d+)$", line
            )
            if match is not None:
                sizes.append((match.group("section"), int(match.group("size"))))
        return sizes

    def is_npm_package_dummy_file(self):
        # HACK(strager): Our npm package contains a dummy file named
        # quick-lint-js.exe. It's not really a Windows executable. Don't give it
        # to llvm-size, and don't record its size at all.
        magic = b"This file gets overwritten by preinstall.js.\n"
        with open(self._path, "rb") as file:
            header = file.read(len(magic))
            return header == magic


class StandardArchiveFileRecorder(Recorder):
    def record(self, sizes: Sizes) -> None:
        RegularFileRecorder(name=self._name, path=self._path, type="archive").record(
            sizes=sizes
        )
        with self.open_archive(self._path) as archive:
            for entry in self.get_entries(archive):
                self._record_archive_entry(archive=archive, entry=entry, sizes=sizes)

    def _record_archive_entry(self, archive, entry, sizes: Sizes) -> None:
        if self.is_entry_directory(entry):
            return
        entry_name = self.get_entry_name(entry)
        entry_path = pathlib.PureWindowsPath(entry_name)
        recorder_factory = classify_path(entry_path)
        if recorder_factory is None:
            return
        name = self._name + (entry_name,)
        if recorder_factory is RegularFileRecorder:
            # Optimization: Don't extract the file just to get its size.
            sizes.record_size(
                name=name,
                size=self.get_entry_size(entry),
                type="file",
            )
        else:
            with tempfile.NamedTemporaryFile(suffix=entry_path.suffix) as temp_file:
                with self.open_entry(archive=archive, entry=entry) as entry_file:
                    shutil.copyfileobj(entry_file, temp_file)
                temp_file.flush()
                recorder = recorder_factory(
                    name=name, path=pathlib.Path(temp_file.name)
                )
                recorder.record(sizes=sizes)


class ZIPFileRecorder(StandardArchiveFileRecorder):
    def open_archive(self, path: pathlib.Path):
        return zipfile.ZipFile(path)

    def get_entries(self, archive) -> typing.Iterable:
        return archive.infolist()

    def is_entry_directory(self, entry) -> bool:
        return entry.is_dir()

    def get_entry_name(self, entry) -> str:
        return entry.filename

    def get_entry_size(self, entry) -> int:
        return entry.file_size

    def open_entry(self, archive, entry):
        return archive.open(entry)


class TarFileRecorder(StandardArchiveFileRecorder):
    def open_archive(self, path: pathlib.Path):
        return tarfile.open(path, mode="r:*")

    def get_entries(self, archive) -> typing.Iterable:
        return archive.getmembers()

    def is_entry_directory(self, entry) -> bool:
        return entry.isdir()

    def get_entry_name(self, entry) -> str:
        return entry.name

    def get_entry_size(self, entry) -> int:
        return entry.size

    def open_entry(self, archive, entry):
        return archive.extractfile(entry)


class TestLLVMSizeRecorder(unittest.TestCase):
    def test_exe(self) -> None:
        output = """\
bin/quick-lint-js.exe  :
section       size         addr
.text       526964   5368713216
.rdata      163018   5369241600
.data         8192   5369405440
.pdata       22152   5369421824
_RDATA         148   5369446400
.rsrc          480   5369450496
.reloc        4648   5369454592
Total       725602


"""
        sizes = LLVMSizeRecorder.parse_llvm_size_output(output)
        self.assertEqual(
            sizes,
            [
                (".text", 526964),
                (".rdata", 163018),
                (".data", 8192),
                (".pdata", 22152),
                ("_RDATA", 148),
                (".rsrc", 480),
                (".reloc", 4648),
            ],
        )


if __name__ == "__main__":
    main()

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew "strager" Glazar
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
