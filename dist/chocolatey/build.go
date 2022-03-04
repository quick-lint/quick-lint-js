// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "archive/zip"
import "embed"
import "encoding/xml"
import "flag"
import "fmt"
import "io"
import "io/fs"
import "log"
import "os"
import "strings"
import "text/template"

var QLJS_ZIP_x86 string
var QLJS_ZIP_x64 string
var OutputNuGetPath string

// See NOTE[contenttypes].
//go:embed Content_Types.xml
//go:embed _rels/.rels
//go:embed package/services/metadata/core-properties/0f2e3a46417c41e09f34ed948b00df06.template.psmdcp
//go:embed quick-lint-js.nuspec
//go:embed tools/VERIFICATION.txt
//go:embed tools/chocolateyinstall.template.ps1
var PackageSources embed.FS

type NuSpecMetadata struct {
	Authors     string `xml:"metadata>authors"`
	Description string `xml:"metadata>description"`
	ID          string `xml:"metadata>id"`
	Tags        string `xml:"metadata>tags"`
	Title       string `xml:"metadata>title"`
	Version     string `xml:"metadata>version"`
}

type ZIPFileLocations struct {
	X86_ZIPPath string
	X64_ZIPPath string
}

func main() {
	flag.StringVar(&QLJS_ZIP_x86, "x86-ZIP", "", "")
	flag.StringVar(&QLJS_ZIP_x64, "x64-ZIP", "", "")
	flag.StringVar(&OutputNuGetPath, "Out", "", "")
	flag.Parse()
	if flag.NArg() != 0 {
		fmt.Fprintf(os.Stderr, "error: unexpected arguments\n")
		os.Exit(2)
	}
	if QLJS_ZIP_x86 == "" {
		fmt.Fprintf(os.Stderr, "error: missing -x86-ZIP\n")
		os.Exit(2)
	}
	if QLJS_ZIP_x64 == "" {
		fmt.Fprintf(os.Stderr, "error: missing -x64-ZIP\n")
		os.Exit(2)
	}
	if OutputNuGetPath == "" {
		fmt.Fprintf(os.Stderr, "error: missing -Out\n")
		os.Exit(2)
	}

	if err := MakeNuGetPackage(); err != nil {
		log.Fatal(err)
	}
}

func MakeNuGetPackage() error {
	nugetFile, err := os.Create(OutputNuGetPath)
	if err != nil {
		return err
	}
	fileComplete := false
	defer (func() {
		nugetFile.Close()
		if !fileComplete {
			os.Remove(OutputNuGetPath)
		}
	})()

	nugetZIPFile := zip.NewWriter(nugetFile)
	defer nugetZIPFile.Close()

	if err := AddSourceFiles(nugetZIPFile, PackageSources); err != nil {
		return err
	}
	if err := AddZIPFiles(nugetZIPFile); err != nil {
		return err
	}
	if err := AddLicenseFile(nugetZIPFile); err != nil {
		return err
	}

	fileComplete = true
	return nil
}

func AddSourceFiles(nuget *zip.Writer, sourceFiles fs.ReadDirFS) error {
	var err error

	var templateVariables struct {
		NuSpecMetadata
		ZIPFileLocations
	}
	templateVariables.NuSpecMetadata, err = ParseNuSpecMetadata()
	if err != nil {
		return err
	}
	templateVariables.ZIPFileLocations = GetZIPFileLocations()

	archiveFile := func(path string) error {
		sourceFile, err := sourceFiles.Open(path)
		if err != nil {
			return err
		}
		defer sourceFile.Close()

		// NOTE[contenttypes]: go:embed dislikes [
		// and ] in file paths. Rename the embedded
		// Content_Types.xml as [Content_Types].xml
		// when archiving.
		nugetPath := strings.ReplaceAll(path, "Content_Types.xml", "[Content_Types].xml")
		if err := ArchiveFile(sourceFile, nuget, nugetPath); err != nil {
			return err
		}
		return nil
	}

	readTemplate := func(path string) (*template.Template, error) {
		templateFile, err := sourceFiles.Open(path)
		if err != nil {
			return nil, err
		}
		defer templateFile.Close()
		templateSource, err := io.ReadAll(templateFile)
		if err != nil {
			return nil, err
		}
		tmpl, err := template.New(path).Parse(string(templateSource))
		if err != nil {
			return nil, err
		}
		return tmpl, nil
	}

	archiveTemplate := func(path string) error {
		nugetPath := strings.ReplaceAll(path, ".template.", ".")
		nugetFile, err := nuget.Create(nugetPath)
		if err != nil {
			return err
		}

		tmpl, err := readTemplate(path)
		if err != nil {
			return err
		}
		if err := tmpl.Execute(nugetFile, templateVariables); err != nil {
			return err
		}
		return nil
	}

	paths, err := GetFilePathsRecursively(sourceFiles, ".")
	if err != nil {
		return err
	}
	for _, path := range paths {
		isTemplate := strings.Contains(path, ".template.")
		if isTemplate {
			if err := archiveTemplate(path); err != nil {
				return err
			}
		} else {
			if err := archiveFile(path); err != nil {
				return err
			}
		}
	}
	return nil
}

func ParseNuSpecMetadata() (NuSpecMetadata, error) {
	var metadata NuSpecMetadata

	nuspecFile, err := PackageSources.Open("quick-lint-js.nuspec")
	if err != nil {
		return metadata, err
	}
	defer nuspecFile.Close()

	nuspec, err := io.ReadAll(nuspecFile)
	if err != nil {
		return metadata, err
	}
	if err := xml.Unmarshal(nuspec, &metadata); err != nil {
		return metadata, err
	}
	return metadata, nil
}

func GetZIPFileLocations() ZIPFileLocations {
	return ZIPFileLocations{
		X86_ZIPPath: "windows-x86.zip",
		X64_ZIPPath: "windows-x64.zip",
	}
}

func AddZIPFiles(nuget *zip.Writer) error {
	type ZIPFile struct {
		SourcePath string
		NuGetPath  string
	}

	archiveZIP := func(zipFile ZIPFile) error {
		sourceFile, err := os.Open(zipFile.SourcePath)
		if err != nil {
			return err
		}
		defer sourceFile.Close()
		if err := ArchiveFile(sourceFile, nuget, zipFile.NuGetPath); err != nil {
			return err
		}
		return nil
	}

	zipLocations := GetZIPFileLocations()
	zipFiles := []ZIPFile{
		ZIPFile{QLJS_ZIP_x64, "tools/" + zipLocations.X64_ZIPPath},
		ZIPFile{QLJS_ZIP_x86, "tools/" + zipLocations.X86_ZIPPath},
	}
	for _, zipFile := range zipFiles {
		if err := archiveZIP(zipFile); err != nil {
			return err
		}
	}
	return nil
}

func AddLicenseFile(nuget *zip.Writer) error {
	// NOTE(strager): It shouldn't matter which ZIP we
	// open. All ZIPs should have the same license text.
	qljsZIPFile, err := os.Open(QLJS_ZIP_x64)
	if err != nil {
		return err
	}
	defer qljsZIPFile.Close()
	qljsZIPFileStat, err := qljsZIPFile.Stat()
	if err != nil {
		return err
	}
	qljsZIP, err := zip.NewReader(qljsZIPFile, qljsZIPFileStat.Size())
	if err != nil {
		return err
	}

	copyrightFile, err := qljsZIP.Open("share/doc/quick-lint-js/copyright")
	if err != nil {
		return err
	}
	if err := ArchiveFile(copyrightFile, nuget, "tools/LICENSE.txt"); err != nil {
		return err
	}

	return nil
}

func ArchiveFile(sourceFile io.Reader, nuget *zip.Writer, nugetPath string) error {
	nugetFile, err := nuget.Create(nugetPath)
	if err != nil {
		return err
	}
	if _, err := io.Copy(nugetFile, sourceFile); err != nil {
		return err
	}
	return nil
}

func GetFilePathsRecursively(filesystem fs.ReadDirFS, rootDir string) ([]string, error) {
	paths := []string{}
	if err := fs.WalkDir(filesystem, rootDir, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !entry.IsDir() {
			paths = append(paths, path)
		}
		return nil
	}); err != nil {
		return paths, err
	}
	return paths, nil
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
