// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// TODO(strager): For portability, rewrite this script to
// use our appx.go code instead of Microsoft's makeappx.exe.
package main

import "flag"
import "fmt"
import "io/ioutil"
import "log"
import "os"
import "os/exec"
import "path/filepath"
import "runtime"
import "text/template"

var EXEPath string
var LicensePath string
var OutPath string

func main() {
	flag.StringVar(&EXEPath, "EXE", "", "")
	flag.StringVar(&LicensePath, "License", "", "")
	flag.StringVar(&OutPath, "Out", "", "")
	flag.Parse()
	if EXEPath == "" {
		fmt.Fprintf(os.Stderr, "error: missing -EXE\n")
		os.Exit(2)
	}
	if LicensePath == "" {
		fmt.Fprintf(os.Stderr, "error: missing -License\n")
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
	var err error

	OutPath, err = filepath.Abs(OutPath)
	if err != nil {
		return err
	}

	tempDir, err := ioutil.TempDir("", "quick-lint-js-build-unsigned-msix")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tempDir)

	_, scriptPath, _, ok := runtime.Caller(0)
	if !ok {
		panic("could not determine path of .go file")
	}
	msixSourcePath := filepath.Dir(scriptPath)

	mappingFilePath := filepath.Join(tempDir, "mapping.txt")
	if err := MakeMapping(msixSourcePath, mappingFilePath); err != nil {
		return err
	}

	process := exec.Command(
		"makeappx",
		"pack",
		"/verbose",
		"/overwrite",
		"/hashAlgorithm", "SHA256",
		"/f", mappingFilePath,
		"/p", OutPath,
	)
	process.Stdout = os.Stdout
	process.Stderr = os.Stderr
	process.Dir = msixSourcePath
	if err := process.Start(); err != nil {
		return err
	}
	if err := process.Wait(); err != nil {
		return err
	}

	return nil
}

func MakeMapping(msixSourcePath string, mappingFilePath string) error {
	var err error

	var templateVariables struct {
		EXE     string
		License string
	}
	templateVariables.EXE, err = filepath.Abs(EXEPath)
	if err != nil {
		return err
	}
	templateVariables.License, err = filepath.Abs(LicensePath)
	if err != nil {
		return err
	}

	mappingTemplatePath := filepath.Join(msixSourcePath, "mapping.template.txt")
	mappingTemplateSource, err := ioutil.ReadFile(mappingTemplatePath)
	tmpl, err := template.New(mappingTemplatePath).Parse(string(mappingTemplateSource))
	if err != nil {
		return err
	}

	mappingFile, err := os.Create(mappingFilePath)
	if err != nil {
		return err
	}
	defer mappingFile.Close()
	if err := tmpl.Execute(mappingFile, templateVariables); err != nil {
		return err
	}

	return nil
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
