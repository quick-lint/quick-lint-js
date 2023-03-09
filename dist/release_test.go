// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "io/fs"
import "os"
import "testing"
import "time"

func AssertStringsEqual(t *testing.T, actual string, expected string) {
	if actual != expected {
		t.Errorf("expected %#v, but got %#v", expected, actual)
	}
}

func Check(t *testing.T, err error) {
	if err != nil {
		t.Errorf("%#v", err)
	}
}

func TestReadVersionFileData(t *testing.T) {
	t.Run("1.0.0", func(t *testing.T) {
		version := ReadVersionFileData([]byte("1.0.0\n2021-12-13\n"))
		AssertStringsEqual(t, version.VersionNumber, "1.0.0")
		AssertStringsEqual(t, version.ReleaseDate, "2021-12-13")
	})
}

func TestWriteVersionFile(t *testing.T) {
	t.Run("replace existing version file", func(t *testing.T) {
		dir := t.TempDir()
		Check(t, os.Chdir(dir))
		Check(t, os.WriteFile("version", []byte{}, fs.FileMode(0644)))

		releaseDate := time.Date(2022, 2, 8, 16, 56, 37, 0, time.Local)
		Check(t, WriteVersionFile("2.0.0", releaseDate))

		versionData, err := os.ReadFile("version")
		Check(t, err)
		AssertStringsEqual(t, string(versionData), "2.0.0\n2022-02-08\n")
	})
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
