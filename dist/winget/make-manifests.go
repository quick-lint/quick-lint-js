// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "crypto/sha256"
import "flag"
import "fmt"
import "io"
import "io/ioutil"
import "log"
import "os"
import "path/filepath"
import "regexp"
import "runtime"
import "text/template"

var BaseURI string
var MSIXPath string
var OutDirPath string

var WingetSourcePath string

var BaseURIRegexp *regexp.Regexp = regexp.MustCompile(`^https?://.*/$`)

type TemplateVariables struct {
	BaseURI    string
	MSIXSHA256 string
}

func main() {
	flag.StringVar(&BaseURI, "BaseURI", "", "")
	flag.StringVar(&MSIXPath, "MSIX", "", "")
	flag.StringVar(&OutDirPath, "OutDir", "", "")
	flag.Parse()
	if BaseURI == "" {
		fmt.Fprintf(os.Stderr, "error: missing -BaseURI\n")
		os.Exit(2)
	}
	if !BaseURIRegexp.MatchString(BaseURI) {
		fmt.Fprintf(os.Stderr, "error: invalid -BaseURI; must match regular expression: %v\n", BaseURIRegexp.String())
		os.Exit(2)
	}
	if MSIXPath == "" {
		fmt.Fprintf(os.Stderr, "error: missing -MSIX\n")
		os.Exit(2)
	}
	if OutDirPath == "" {
		fmt.Fprintf(os.Stderr, "error: missing -OutDir\n")
		os.Exit(2)
	}

	_, scriptPath, _, ok := runtime.Caller(0)
	if !ok {
		panic("could not determine path of .go file")
	}
	WingetSourcePath = filepath.Dir(scriptPath)

	if err := Main(); err != nil {
		log.Fatal(err)
	}
}

func Main() error {
	if err := os.MkdirAll(OutDirPath, 0755); err != nil {
		return err
	}

	msixSHA256, err := SHA256File(MSIXPath)
	if err != nil {
		return err
	}
	variables := TemplateVariables{
		BaseURI:    BaseURI,
		MSIXSHA256: msixSHA256,
	}

	filesToTransform := map[string]string{
		"quick-lint.quick-lint-js.installer.template.yaml":    "quick-lint.quick-lint-js.installer.yaml",
		"quick-lint.quick-lint-js.locale.en-US.template.yaml": "quick-lint.quick-lint-js.locale.en-US.yaml",
		"quick-lint.quick-lint-js.template.yaml":              "quick-lint.quick-lint-js.yaml",
	}
	for templatePath, outputPath := range filesToTransform {
		if err := TransformTemplateFile(
			filepath.Join(WingetSourcePath, templatePath),
			filepath.Join(OutDirPath, outputPath),
			variables,
		); err != nil {
			return err
		}
	}

	return nil
}

func TransformTemplateFile(templatePath string, outputPath string, variables TemplateVariables) error {
	templateSource, err := ioutil.ReadFile(templatePath)
	if err != nil {
		return err
	}
	tmpl, err := template.New(templatePath).Parse(string(templateSource))
	if err != nil {
		return err
	}

	outputFile, err := os.Create(outputPath)
	if err != nil {
		return err
	}
	defer outputFile.Close()
	if err := tmpl.Execute(outputFile, variables); err != nil {
		return err
	}

	return nil
}

func SHA256File(path string) (string, error) {
	file, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer file.Close()
	hasher := sha256.New()
	if _, err := io.Copy(hasher, file); err != nil {
		return "", err
	}
	return fmt.Sprintf("%x", hasher.Sum(nil)), nil
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
