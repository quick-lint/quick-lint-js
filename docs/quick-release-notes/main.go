// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
)

// Tag is for GitHub repo tags.
type Tag struct {
	Name string `json:"name"`
}

type listOfReleasesForUpdate struct {
	Name    string `json:"name"`
	Body    string `json:"body"`
	URL     string `json:"url"`
	TagName string `json:"tag_name"`
}

// ChangeLogInfo for each releases info from CHANGELOG.md
type ChangeLogInfo struct {
	versionLineNumbers           []int
	changeLogText                []string
	changeLogLength              int
	versionTitlesForEachRelease  []string
	versionNumbersForEachRelease []string
}

func main() {
	repoPtr := flag.String("Repo", "quick-lint/quick-lint-js", "a string")
	tagsRepoPtr := flag.String("TagsRepo", "quick-lint/quick-lint-js", "a string")
	flag.Parse()
	fmt.Println(*repoPtr, *tagsRepoPtr)
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
	tagsRepoPath := *tagsRepoPtr
	tagsForEachRelease := getTagsFromAPI(tagsRepoPath)
	repoPath := *repoPtr
	releaseNotesMap := checkEachTagHasReleaseNote(tagsForEachRelease, ChangeLogInfo, releaseNotesForEachVersion, repoPath)

	getReleasesFromAPI(tagsRepoPath, releaseNotesMap)
}

func checkEachTagHasReleaseNote(tagsForEachRelease []Tag, ChangeLogInfo ChangeLogInfo, releaseNoteForEachVersion []string, repoPath string) map[string]string {

	var redColor = "\033[31m"
	var resetColor = "\033[0m"
	releaseVersionAndTag := make(map[string]string)
	changeLogReleaseNotesMap := make(map[string]string)
	for i, releaseNoteVersion := range ChangeLogInfo.versionNumbersForEachRelease[:] {
		tagVersionForMap := ""
		releaseVersionHasTag := false
		for _, tagVersion := range tagsForEachRelease[:] {
			if releaseNoteVersion == tagVersion.Name {
				releaseVersionHasTag = true
				tagVersionForMap = tagVersion.Name
			}
		}
		if releaseVersionHasTag == false {
			fmt.Println(redColor+"WARNING: release", releaseNoteVersion, "missing Tag"+resetColor)
		}
		if releaseVersionHasTag {
			releaseVersionAndTag[releaseNoteVersion] = tagVersionForMap
			changeLogReleaseNotesMap[releaseNoteVersion] = releaseNoteForEachVersion[i]
		}
	}

	tagAndReleaseVersion := make(map[string]string)
	for _, tagVersion := range tagsForEachRelease[:] {
		releaseNoteVersionForMap := ""
		tagHasVersionNumber := false
		for _, releaseNoteVersion := range ChangeLogInfo.versionNumbersForEachRelease[:] {
			if tagVersion.Name == releaseNoteVersion {
				tagHasVersionNumber = true
				releaseNoteVersionForMap = releaseNoteVersion
			}
		}
		if tagHasVersionNumber == false {
			fmt.Println(redColor+"WARNING: tag", tagVersion.Name, "missing changelog entry"+resetColor)
		}
		if tagHasVersionNumber {
			tagAndReleaseVersion[tagVersion.Name] = releaseNoteVersionForMap
		}
	}

	for releaseNoteVersion, tagVersion := range releaseVersionAndTag {
		if releaseVersionAndTag[releaseNoteVersion] == tagAndReleaseVersion[tagVersion] {
			makeOrUpdateGitHubRelease(tagAndReleaseVersion[tagVersion], changeLogReleaseNotesMap[releaseNoteVersion], releaseNoteVersion, repoPath, "POST", "")
		}
	}
	return changeLogReleaseNotesMap

}

func getReleasesFromAPI(tagsRepoPath string, releaseNotesForEachVersion map[string]string) {
	releasePath := fmt.Sprintf("https://api.github.com/repos/%v/releases", tagsRepoPath)
	req, err := http.NewRequest("GET", releasePath, nil)
	req.Header.Set("Accept", "application/vnd.github.v3+json")
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "token insert_token")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}

	var eachRelease []listOfReleasesForUpdate
	err = json.Unmarshal(body, &eachRelease)
	if err != nil {
		log.Fatal(err)
	}
	// fmt.Printf("%T", eachRelease)
	for _, release := range eachRelease[:] {
		fmt.Println(release.Body)
		if release.Body != releaseNotesForEachVersion[release.Name] {
			makeOrUpdateGitHubRelease(release.TagName, releaseNotesForEachVersion[release.Name], release.Name, "", "PATCH", release.URL)
		}
	}
}

func getTagsFromAPI(tagsRepoPath string) []Tag {
	// https://docs.github.com/en/rest/reference/repos#list-repository-tags
	pathToTags := fmt.Sprintf("https://api.github.com/repos/%v/tags", tagsRepoPath)
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
	re, err := regexp.Compile(`## (?P<versionNumberAndDate>(?P<versionNumber>\d+\.\d+\.\d+).*)`)
	if err != nil {
		log.Fatal(err)
	}
	lineCount := 0
	changeLogLength := 0
	var versionLineNumbers []int
	var changeLogText []string
	var versionTitlesForEachRelease []string
	var versionNumbersForEachRelease []string
	for scanner.Scan() {
		changeLogLength++
		changeLogText = append(changeLogText, scanner.Text())
		if re.MatchString(scanner.Text()) {
			hashVersionAndDate := re.FindStringSubmatch(scanner.Text())
			idx := re.SubexpIndex("versionNumberAndDate")
			idx2 := re.SubexpIndex("versionNumber")
			versionNumberAndDate := hashVersionAndDate[idx]
			versionNumber := hashVersionAndDate[idx2]
			versionTitlesForEachRelease = append(versionTitlesForEachRelease, versionNumberAndDate)
			versionNumbersForEachRelease = append(versionNumbersForEachRelease, versionNumber)
			versionLineNumbers = append(versionLineNumbers, lineCount)
		}
		lineCount++
	}
	if scanner.Err() != nil {
		fmt.Println(scanner.Err())
	}
	return ChangeLogInfo{versionLineNumbers, changeLogText, changeLogLength, versionTitlesForEachRelease, versionNumbersForEachRelease}
}

func createReleaseNotes(ChangeLogInfo ChangeLogInfo) []string {
	re, err := regexp.Compile(`^\[.+\]: .+`)
	if err != nil {
		log.Fatal(err)
	}
	lastVersion := len(ChangeLogInfo.versionLineNumbers) - 1
	contributorsAndErrors := ""
	for i := ChangeLogInfo.versionLineNumbers[lastVersion]; i < ChangeLogInfo.changeLogLength; i++ {
		if re.MatchString(ChangeLogInfo.changeLogText[i]) {
			contributorsAndErrors += ChangeLogInfo.changeLogText[i] + "\n"
		}
	}
	var releaseNotesForEachVersion []string
	// exclude Last version (## 0.2.0) with - 1
	for i, versionLineNumber := range ChangeLogInfo.versionLineNumbers[:] {
		releaseBodyLines := ""
		if !(i == (lastVersion)) {
			for j := ChangeLogInfo.versionLineNumbers[i] + 1; j < ChangeLogInfo.versionLineNumbers[i+1]; j++ {
				releaseBodyLines += ChangeLogInfo.changeLogText[j] + "\n"
			}
		} else {
			// Handle last version (## 0.2.0)
			if versionLineNumber == ChangeLogInfo.versionLineNumbers[lastVersion] {
				for j := 1; j < ChangeLogInfo.changeLogLength-ChangeLogInfo.versionLineNumbers[lastVersion]; j++ {
					currentLineOfText := ChangeLogInfo.changeLogText[versionLineNumber+j]
					if !re.MatchString(currentLineOfText) {
						releaseBodyLines += currentLineOfText + "\n"
					}
				}
			}
		}
		releaseNotesForEachVersion = append(releaseNotesForEachVersion, releaseBodyLines+contributorsAndErrors)
	}
	return releaseNotesForEachVersion
}

func makeOrUpdateGitHubRelease(tagForRelease string, releaseNote string, versionTitle string, repoPath string, requestType string, releaseURLWithID string) {
	// https://docs.github.com/en/rest/reference/releases
	postBody, err := json.Marshal(map[string]string{
		"tag_name": tagForRelease,
		"name":     versionTitle,
		"body":     releaseNote,
	})
	if err != nil {
		log.Fatal(err)
	}
	responseBody := bytes.NewBuffer(postBody)
	releasesURL := ""
	if requestType == "POST" {
		releasesURL = fmt.Sprintf("https://api.github.com/repos/%v/releases", repoPath)
	} else if requestType == "PATCH" {
		releasesURL = releaseURLWithID
	}
	fmt.Printf(releaseURLWithID)
	req, err := http.NewRequest(requestType, releasesURL, responseBody)
	req.Header.Set("Accept", "application/vnd.github.v3+json")
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "token insert_token")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()
	fmt.Println("Response Headers:", resp.Header)
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
