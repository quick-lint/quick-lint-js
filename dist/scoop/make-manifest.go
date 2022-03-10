// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "bytes"
import "crypto/sha256"
import "encoding/json"
import "flag"
import "fmt"
import "io"
import "log"
import "os"
import "regexp"
import _ "embed"

//go:embed quick-lint-js.json
var TemplateManifestJSON []byte

var BaseURI string
var BaseURIRegexp *regexp.Regexp = regexp.MustCompile(`^https://c\.quick-lint-js\.com/(builds|releases)/[^/]+/`)

var x64ZIP string
var x86ZIP string

var OutPath string

func main() {
	flag.StringVar(&BaseURI, "BaseURI", "", "")
	flag.StringVar(&x86ZIP, "x86-ZIP", "", "")
	flag.StringVar(&x64ZIP, "x64-ZIP", "", "")
	flag.StringVar(&OutPath, "Out", "", "")
	flag.Parse()
	if BaseURI == "" {
		fmt.Fprintf(os.Stderr, "error: missing -BaseURI\n")
		os.Exit(2)
	}
	if !BaseURIRegexp.MatchString(BaseURI) {
		fmt.Fprintf(os.Stderr, "error: invalid -BaseURI; must match regular expression: %v\n", BaseURIRegexp.String())
		os.Exit(2)
	}
	if x86ZIP == "" {
		fmt.Fprintf(os.Stderr, "error: missing -x86-ZIP\n")
		os.Exit(2)
	}
	if x64ZIP == "" {
		fmt.Fprintf(os.Stderr, "error: missing -x64-ZIP\n")
		os.Exit(2)
	}
	if OutPath == "" {
		fmt.Fprintf(os.Stderr, "error: missing -Out\n")
		os.Exit(2)
	}

	if err := Main(); err != nil {
		log.Fatal(err)
	}
}

func Main() error {
	decoder := json.NewDecoder(bytes.NewReader(TemplateManifestJSON))
	var manifest interface{}
	if err := decoder.Decode(&manifest); err != nil {
		return err
	}

	if err := ModifyManifest(manifest); err != nil {
		return err
	}

	outFile, err := os.Create(OutPath)
	if err != nil {
		return err
	}
	defer outFile.Close()
	encoder := json.NewEncoder(outFile)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(&manifest); err != nil {
		return err
	}
	return nil
}

func ModifyManifest(manifest interface{}) error {
	x86Hash, err := HashFile(x86ZIP)
	if err != nil {
		return err
	}
	x64Hash, err := HashFile(x64ZIP)
	if err != nil {
		return err
	}

	architecture := manifest.(map[string]interface{})["architecture"].(map[string]interface{})
	architecture32 := architecture["32bit"].(map[string]interface{})
	architecture32["hash"] = x86Hash
	architecture32["url"] = BaseURIRegexp.ReplaceAllLiteralString(architecture32["url"].(string), BaseURI)
	architecture64 := architecture["64bit"].(map[string]interface{})
	architecture64["hash"] = x64Hash
	architecture64["url"] = BaseURIRegexp.ReplaceAllLiteralString(architecture64["url"].(string), BaseURI)

	return nil
}

func HashFile(path string) (string, error) {
	file, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer file.Close()
	hasher := sha256.New()
	if _, err := io.Copy(hasher, file); err != nil {
		return "", err
	}
	return fmt.Sprintf("sha256:%x", hasher.Sum(nil)), nil
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
