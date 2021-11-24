// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "bufio"
import "bytes"
import "fmt"
import "io/ioutil"
import "log"
import "os"
import "os/exec"
import "path/filepath"
import "sort"
import "strings"

func main() {
	poFiles, err := ListPOFiles()
	if err != nil {
		log.Fatal(err)
	}

	outputFile, err := os.Create("src/translation-data.cpp")
	if err != nil {
		log.Fatal(err)
	}
	defer outputFile.Close()
	writer := bufio.NewWriter(outputFile)

	writer.WriteString(
		`// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// This file is **GENERATED** by tools/compile-translations.go.

#include <cstdint>
#include <quick-lint-js/locale.h>
#include <quick-lint-js/translation-data.h>

namespace quick_lint_js {
`)

	writer.WriteString(
		`namespace {
// clang-format off
`)

	for i, poFilePath := range poFiles {
		_ = poFilePath
		fmt.Fprintf(writer, "const std::uint8_t language_%d[] = {\n", i)

		gmo, err := POFileToGMO(poFilePath)
		if err != nil {
			log.Fatal(err)
		}

		HexDump(gmo, writer)
		writer.WriteString("};\n")
	}

	writer.WriteString(
		`// clang-format on
}

`)

	writer.WriteString("const locale_entry<const std::uint8_t *> gmo_files[] = {\n")
	for i, poFilePath := range poFiles {
		locale := strings.TrimSuffix(filepath.Base(poFilePath), filepath.Ext(poFilePath))
		fmt.Fprintf(writer, "    {\"%s\", language_%d},\n", locale, i)
	}

	writer.WriteString(
		`    {},
};
`)

	writer.WriteString("}\n")

	writer.WriteString(
		`
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
`)

	if err := writer.Flush(); err != nil {
		log.Fatal(err)
	}
}

func ListPOFiles() ([]string, error) {
	poDirectory := "po"
	filesInPODirectory, err := ioutil.ReadDir(poDirectory)
	if err != nil {
		return nil, err
	}
	var poFiles []string
	for _, fileInfo := range filesInPODirectory {
		if filepath.Ext(fileInfo.Name()) == ".po" {
			poFiles = append(poFiles, filepath.Join(poDirectory, fileInfo.Name()))
		}
	}
	// Sort locales to make builds reproducible.
	sort.Strings(poFiles)
	return poFiles, nil
}

func POFileToGMO(poFilePath string) ([]byte, error) {
	process := exec.Command(
		"msgfmt",
		"--output-file=-",
		"--",
		poFilePath,
	)
	var gmo bytes.Buffer
	process.Stdout = &gmo
	process.Stderr = os.Stderr
	if err := process.Start(); err != nil {
		return nil, err
	}
	if err := process.Wait(); err != nil {
		return nil, err
	}
	return gmo.Bytes(), nil
}

func HexDump(data []byte, writer *bufio.Writer) {
	const width int = 12
	lastByteIndex := len(data) - 1
	writer.WriteRune(' ')
	for i, b := range data {
		fmt.Fprintf(writer, " 0x%02x", b)
		if i == lastByteIndex {
			writer.WriteRune('\n')
			break
		}
		writer.WriteRune(',')
		if i%width == width-1 {
			writer.WriteString("\n ")
		}
	}
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
