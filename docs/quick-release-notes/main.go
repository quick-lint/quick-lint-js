// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
)

// Tag is for GitHub repo tags.
type Tag struct {
	Name string `json:"name"`
}

// ChangeLogInfo is for each releases info generated from CHANGELOG.md
type ChangeLogInfo struct {
	versionLineNumbers          []int
	changeLogText               []string
	counterForChangeLogLength   int
	versionTitlesForEachRelease []string
}

func main() {
	fmt.Println("Quick release notes running...")
	_, scriptPath, _, ok := runtime.Caller(0)
	if !ok {
		panic("could not determine path of .go file")
	}
	pathToChangeLog := filepath.Join(filepath.Dir(scriptPath), "../CHANGELOG.md")
	file, err := os.Open(pathToChangeLog)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	ChangeLogInfo := getChangeLogInfo(bufio.NewScanner(file))
	releaseNotesForEachVersion := createReleaseNotes(ChangeLogInfo)
	owner, repo := "quick-lint", "quick-lint-js"
	tagsForEachRelease := getTagsFromAPI(owner, repo)
	owner, repo = "LeeWannacott", "quick-lint-js"
	if len(releaseNotesForEachVersion) == len(tagsForEachRelease) && len(releaseNotesForEachVersion) == len(ChangeLogInfo.versionTitlesForEachRelease) {
		for i := range releaseNotesForEachVersion[:] {
			makeGitHubRelease(tagsForEachRelease[i], releaseNotesForEachVersion[i], ChangeLogInfo.versionTitlesForEachRelease[i], owner, repo)
		}
		fmt.Println("Quick release notes finished...")
	} else {
		fmt.Println("Error: Release Note versions in changelog.md and Tags from api are different lengths")
	}
}

func getTagsFromAPI(owner string, repo string) []Tag {
	// https://docs.github.com/en/rest/reference/repos#list-repository-tags
	pathToTags := fmt.Sprintf("https://api.github.com/repos/%v/%v/tags", url.QueryEscape(owner), url.QueryEscape(repo))
	resp, err := http.Get(pathToTags)
	if err != nil {
		log.Fatal(err)
	}
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	responseFromAPI := []byte(body)
	var tagsForEachRelease []Tag
	err = json.Unmarshal(responseFromAPI, &tagsForEachRelease)
	if err != nil {
		log.Fatal(err)
	}
	return tagsForEachRelease
}

func getChangeLogInfo(scanner *bufio.Scanner) ChangeLogInfo {
	// regexp for: ## 1.0.0 (2021-12-13)
	re, err := regexp.Compile(`## (?P<versionNumberAndDate>\d+\.\d+\.\d+.*)`)

	if err != nil {
		log.Fatal(err)
	}
	lineCount := 0
	counterForChangeLogLength := 0
	var versionLineNumbers []int
	var changeLogText []string
	var versionTitlesForEachRelease []string
	for scanner.Scan() {
		counterForChangeLogLength++
		changeLogText = append(changeLogText, scanner.Text())
		if re.MatchString(scanner.Text()) {
			hashVersionAndDate := re.FindStringSubmatch(scanner.Text())
			index := re.SubexpIndex("versionNumberAndDate")
			versionNumberAndDate := hashVersionAndDate[index]
			versionTitlesForEachRelease = append(versionTitlesForEachRelease, versionNumberAndDate)
			versionLineNumbers = append(versionLineNumbers, lineCount)
		}
		lineCount++
	}
	if scanner.Err() != nil {
		fmt.Println(scanner.Err())
	}
	return ChangeLogInfo{versionLineNumbers, changeLogText, counterForChangeLogLength, versionTitlesForEachRelease}
}

func createReleaseNotes(ChangeLogInfo ChangeLogInfo) []string {
	// Store contributors and errors from end of changelog.
	contributorsAndErrors := ""
	numberOfLinesForLastRelease := 5
	for i := numberOfLinesForLastRelease + ChangeLogInfo.versionLineNumbers[len(ChangeLogInfo.versionLineNumbers)-1]; i < ChangeLogInfo.counterForChangeLogLength; i++ {
		contributorsAndErrors += ChangeLogInfo.changeLogText[i] + "\n"
	}
	var releaseNotesForEachVersion []string
	// exclude Last version (## 0.2.0) with - 1
	lastVersion := len(ChangeLogInfo.versionLineNumbers) - 1
	for i, versionLineNumber := range ChangeLogInfo.versionLineNumbers[:] {
		releaseBodyLines := ""
		if !(i == (lastVersion)) {
			for j := ChangeLogInfo.versionLineNumbers[i] + 1; j < ChangeLogInfo.versionLineNumbers[i+1]; j++ {
				releaseBodyLines += ChangeLogInfo.changeLogText[j] + "\n"
			}
		} else {
			// Handle last version (## 0.2.0)
			if versionLineNumber == ChangeLogInfo.versionLineNumbers[lastVersion] {
				for j := 1; j < numberOfLinesForLastRelease; j++ {
					releaseBodyLines += ChangeLogInfo.changeLogText[versionLineNumber+j] + "\n"
				}
			}
		}
		//
		releaseNotesForEachVersion = append(releaseNotesForEachVersion, releaseBodyLines+contributorsAndErrors)
	}
	return releaseNotesForEachVersion
}

func makeGitHubRelease(tagForRelease Tag, releaseNote string, versionTitle string, owner string, repo string) {
	// https://docs.github.com/en/rest/reference/releases
	postBody, err := json.Marshal(map[string]string{
		"tag_name": tagForRelease.Name,
		"name":     versionTitle,
		"body":     releaseNote,
	})
	if err != nil {
		log.Fatal(err)
	}
	responseBody := bytes.NewBuffer(postBody)
	url := fmt.Sprintf("https://api.github.com/repos/%v/%v/releases", url.QueryEscape(owner), url.QueryEscape(repo))
	req, err := http.NewRequest("POST", url, responseBody)
	req.Header.Set("Accept", "application/vnd.github.v3+json")
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "token insert_token_here")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()
	fmt.Println("response Headers:", resp.Header)
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
