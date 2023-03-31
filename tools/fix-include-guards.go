// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "bufio"
import "bytes"
import "fmt"
import "log"
import "os"
import "path/filepath"
import "regexp"
import "strings"
import "unicode"

func main() {
	pathsToFix := os.Args[1:]
	if len(pathsToFix) == 0 {
		log.Fatal("missing arguments; usage: go run tools/fix-include-guards.go file.h [file.h ...]")
	}

	for _, pathToFix := range pathsToFix {
		content, err := os.ReadFile(pathToFix)
		if err != nil {
			log.Fatal(err)
		}
		includeGuardMacroName := IncludeGuardMacroName(pathToFix)
		fixedContent := FixExistingIncludeGuard(AddIncludeGuardIfMissing(content, includeGuardMacroName), includeGuardMacroName)
		if !bytes.Equal(content, fixedContent) {
			if err := os.WriteFile(pathToFix, fixedContent, 0644); err != nil {
				log.Fatal(err)
			}
		}
	}
}

var ifndefRegexp = regexp.MustCompile(`^#ifndef ([A-Za-z0-9_]+)(.*)`)
var defineRegexp = regexp.MustCompile(`^#define ([A-Za-z0-9_]+)(.*)`)

func AddIncludeGuardIfMissing(source []byte, macroName string) []byte {
	// TODO(strager)
	return source
}

func FixExistingIncludeGuard(source []byte, macroName string) []byte {
	var out bytes.Buffer
	scanner := bufio.NewScanner(bytes.NewReader(source))
	fixedIfndefLine := fmt.Sprintf("#ifndef %s", macroName)
	fixedDefineLine := fmt.Sprintf("#define %s", macroName)
	fixedIfndef := false
	fixedDefine := false
	for scanner.Scan() {
		line := scanner.Text()
		ifndefMatch := ifndefRegexp.FindStringSubmatch(line)
		if ifndefMatch != nil {
			line = fixedIfndefLine
			fixedIfndef = true
		} else if fixedIfndef && !fixedDefine {
			defineMatch := defineRegexp.FindStringSubmatch(line)
			if defineMatch == nil {
				fmt.Fprintf(&out, "%s\n", fixedDefineLine)
			} else {
				line = fixedDefineLine
				fixedDefine = true
			}
		}
		fmt.Fprintf(&out, "%s\n", line)
	}
	return out.Bytes()
}

func IncludeGuardMacroName(path string) string {
	components := SplitPathComponents(path)
	rootIndex := -1
	for i, component := range components {
		if component == "quick-lint-js" {
			rootIndex = i
			// Keep going. We want the last index of
			// 'quick-lint-js'.
		}
	}
	if rootIndex < 0 {
		return ""
	}
	return strings.Map(sanitizeRuneForMacroName, strings.Join(components[rootIndex:], "_"))
}

func sanitizeRuneForMacroName(r rune) rune {
	if r <= unicode.MaxASCII && (unicode.IsLetter(r) || unicode.IsDigit(r)) {
		return unicode.ToUpper(r)
	} else {
		return '_'
	}
}

func SplitPathComponents(path string) []string {
	var components []string
	for path != "" {
		parent, file := filepath.Split(path)
		components = append(components, file)
		path = strings.TrimRight(parent, "\\/")
	}
	ReverseStringSlice(components)
	return components
}

func ReverseStringSlice(items []string) {
	middle := len(items) / 2
	for i := 0; i < middle; i++ {
		j := len(items) - i - 1
		items[i], items[j] = items[j], items[i]
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
