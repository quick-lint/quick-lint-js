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
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
)

type tagInfo struct {
	Name string `json:"name"`
}

type listOfReleasesForUpdate struct {
	Name    string `json:"name"`
	Body    string `json:"body"`
	URL     string `json:"url"`
	TagName string `json:"tag_name"`
}

type changeLogInfo struct {
	versionLineNumbers []int
	changeLogText      []string
	changeLogLength    int
	versionTitles      []string
	versionNumbers     []string
}

type releaseData struct {
	releaseVersionAndNote map[string]string
	releaseVersionAndTag  map[string]string
	tagAndReleaseVersion  map[string]string
}

type validationData struct {
	authToken     string
	tags          []tagInfo
	changeLogInfo changeLogInfo
	releaseNotes  []string
	repoPath      string
}

type dataForAPI struct {
	authToken     string
	repoPath      string
	requestType   string
	urlWithID     string
	tagForRelease string
	versionTitle  string
	releaseNote   string
}

func main() {
	authToken := os.Args[len(os.Args)-1]
	repoPtr := flag.String("Repo", "quick-lint/quick-lint-js", "a string")
	tagsRepoPtr := flag.String("TagsRepo", "quick-lint/quick-lint-js", "a string")
	flag.Parse()
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
	changeLogInfo := getChangeLogInfo(bufio.NewScanner(file))
	releaseNotes := createReleaseNotes(changeLogInfo)
	tagsRepoPath := *tagsRepoPtr
	tags := getTagsFromAPI(tagsRepoPath)
	repoPath := *repoPtr
	releaseData := validateTagsHaveReleases(validationData{authToken: authToken, tags: tags, changeLogInfo: changeLogInfo, releaseNotes: releaseNotes, repoPath: repoPath})
	ifReleaseNotExistMakeReleases(releaseData, authToken, repoPath)
	ifChangeLogChangedUpdateReleases(releaseData, authToken, repoPath)
}

func ifReleaseNotExistMakeReleases(releaseData releaseData, authToken string, repoPath string) {
	for releaseVersion, tagVersion := range releaseData.releaseVersionAndTag {
		if releaseData.releaseVersionAndTag[releaseVersion] == releaseData.tagAndReleaseVersion[tagVersion] {
			makeOrUpdateGitHubRelease(dataForAPI{authToken: authToken, repoPath: repoPath, requestType: "POST", urlWithID: "", tagForRelease: tagVersion, versionTitle: releaseData.tagAndReleaseVersion[releaseVersion], releaseNote: releaseData.releaseVersionAndNote[releaseVersion]})
		}
	}
}

func ifChangeLogChangedUpdateReleases(releaseData releaseData, authToken string, repoPath string) {
	releases := getReleases(authToken, repoPath)
	for _, release := range releases[:] {
		if release.Body != releaseData.releaseVersionAndNote[release.Name] {
			makeOrUpdateGitHubRelease(dataForAPI{authToken: authToken, repoPath: repoPath, requestType: "PATCH", urlWithID: release.URL, tagForRelease: release.TagName, versionTitle: release.Name, releaseNote: releaseData.releaseVersionAndNote[release.Name]})
		}
	}
}

func validateTagsHaveReleases(validationData validationData) releaseData {
	var redColor = "\033[31m"
	var resetColor = "\033[0m"
	releaseVersionAndTag := make(map[string]string)
	releaseVersionAndNote := make(map[string]string)
	for i, releaseVersion := range validationData.changeLogInfo.versionNumbers[:] {
		tagVersionForMap := ""
		releaseVersionHasTag := false
		for _, tagVersion := range validationData.tags[:] {
			if releaseVersion == tagVersion.Name {
				releaseVersionHasTag = true
				tagVersionForMap = tagVersion.Name
			}
		}
		if releaseVersionHasTag == false {
			fmt.Println(redColor+"WARNING: release", releaseVersion, "missing tagInfo"+resetColor)
		}
		if releaseVersionHasTag {
			releaseVersionAndTag[releaseVersion] = tagVersionForMap
			releaseVersionAndNote[releaseVersion] = validationData.releaseNotes[i]
		}
	}
	tagAndReleaseVersion := make(map[string]string)
	for _, tagVersion := range validationData.tags[:] {
		releaseVersionForMap := ""
		tagHasVersionNumber := false
		for _, releaseVersion := range validationData.changeLogInfo.versionNumbers[:] {
			if tagVersion.Name == releaseVersion {
				tagHasVersionNumber = true
				releaseVersionForMap = releaseVersion
			}
		}
		if tagHasVersionNumber == false {
			fmt.Println(redColor+"WARNING: tag", tagVersion.Name, "missing changelog entry"+resetColor)
		}
		if tagHasVersionNumber {
			tagAndReleaseVersion[tagVersion.Name] = releaseVersionForMap
		}
	}
	return releaseData{releaseVersionAndNote: releaseVersionAndNote, releaseVersionAndTag: releaseVersionAndTag, tagAndReleaseVersion: tagAndReleaseVersion}
}

func getReleases(authToken string, repoPath string) []listOfReleasesForUpdate {
	repoPathSplit := strings.Split(repoPath, "/")
	repoOwner, repoName := url.QueryEscape(repoPathSplit[0]), url.QueryEscape(repoPathSplit[1])
	releasePath := fmt.Sprintf("https://api.github.com/repos/%v/%v/releases", repoOwner, repoName)
	req, err := http.NewRequest("GET", releasePath, nil)
	req.Header.Set("Accept", "application/vnd.github.v3+json")
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "token "+authToken)
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
	return eachRelease
}

func getTagsFromAPI(tagsRepoPath string) []tagInfo {
	// https://docs.github.com/en/rest/reference/repos#list-repository-tags
	tagsRepoSplit := strings.Split(tagsRepoPath, "/")
	tagsRepoOwner, tagsRepoName := url.QueryEscape(tagsRepoSplit[0]), url.QueryEscape(tagsRepoSplit[1])
	pathToTags := fmt.Sprintf("https://api.github.com/repos/%v/%v/tags", tagsRepoOwner, tagsRepoName)
	resp, err := http.Get(pathToTags)
	if err != nil {
		log.Fatal(err)
	}
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	responseFromAPI := []byte(body)
	var tags []tagInfo
	err = json.Unmarshal(responseFromAPI, &tags)
	if err != nil {
		log.Fatal(err)
	}
	return tags
}

func getChangeLogInfo(scanner *bufio.Scanner) changeLogInfo {
	re, err := regexp.Compile(`## (?P<versionNumberAndDate>(?P<versionNumber>\d+\.\d+\.\d+).*)`)
	if err != nil {
		log.Fatal(err)
	}
	lineCount := 0
	changeLogLength := 0
	var versionLineNumbers []int
	var changeLogText []string
	var versionTitles []string
	var versionNumbers []string
	for scanner.Scan() {
		changeLogLength++
		changeLogText = append(changeLogText, scanner.Text())
		if re.MatchString(scanner.Text()) {
			hashVersionAndDate := re.FindStringSubmatch(scanner.Text())
			idxVersionNumberAndDate := re.SubexpIndex("versionNumberAndDate")
			idxVersionNumber := re.SubexpIndex("versionNumber")
			versionNumberAndDate := hashVersionAndDate[idxVersionNumberAndDate]
			versionNumber := hashVersionAndDate[idxVersionNumber]
			versionTitles = append(versionTitles, versionNumberAndDate)
			versionNumbers = append(versionNumbers, versionNumber)
			versionLineNumbers = append(versionLineNumbers, lineCount)
		}
		lineCount++
	}
	if scanner.Err() != nil {
		fmt.Println(scanner.Err())
	}
	return changeLogInfo{versionLineNumbers, changeLogText, changeLogLength, versionTitles, versionNumbers}
}

func createReleaseNotes(changeLogInfo changeLogInfo) []string {
	re, err := regexp.Compile(`^\[.+\]: .+`)
	if err != nil {
		log.Fatal(err)
	}
	lastVersionIdx := len(changeLogInfo.versionLineNumbers) - 1
	contributorsAndErrors := ""
	for i := changeLogInfo.versionLineNumbers[lastVersionIdx]; i < changeLogInfo.changeLogLength; i++ {
		if re.MatchString(changeLogInfo.changeLogText[i]) {
			contributorsAndErrors += changeLogInfo.changeLogText[i] + "\n"
		}
	}
	var releaseNotes []string
	// exclude Last version (## 0.2.0) with - 1
	for i, versionLineNumber := range changeLogInfo.versionLineNumbers[:] {
		releaseBodyLines := ""
		if !(i == (lastVersionIdx)) {
			for j := changeLogInfo.versionLineNumbers[i] + 1; j < changeLogInfo.versionLineNumbers[i+1]; j++ {
				releaseBodyLines += changeLogInfo.changeLogText[j] + "\n"
			}
		} else {
			// Handle last version (## 0.2.0)
			if versionLineNumber == changeLogInfo.versionLineNumbers[lastVersionIdx] {
				for j := 1; j < changeLogInfo.changeLogLength-changeLogInfo.versionLineNumbers[lastVersionIdx]; j++ {
					currentLineOfText := changeLogInfo.changeLogText[versionLineNumber+j]
					if !re.MatchString(currentLineOfText) {
						releaseBodyLines += currentLineOfText + "\n"
					}
				}
			}
		}
		releaseNotes = append(releaseNotes, releaseBodyLines+contributorsAndErrors)
	}
	return releaseNotes
}

func makeOrUpdateGitHubRelease(dataForAPI dataForAPI) {
	// https://docs.github.com/en/rest/reference/releases
	postBody, err := json.Marshal(map[string]string{
		"tag_name": dataForAPI.tagForRelease,
		"name":     dataForAPI.versionTitle,
		"body":     dataForAPI.releaseNote,
	})
	if err != nil {
		log.Fatal(err)
	}
	responseBody := bytes.NewBuffer(postBody)
	releasesURL := ""
	if dataForAPI.requestType == "POST" {
		repoPathSplit := strings.Split(dataForAPI.repoPath, "/")
		repoOwner, repoName := url.QueryEscape(repoPathSplit[0]), url.QueryEscape(repoPathSplit[1])
		releasesURL = fmt.Sprintf("https://api.github.com/repos/%v/%v/releases", repoOwner, repoName)
	} else if dataForAPI.requestType == "PATCH" {
		releasesURL = dataForAPI.urlWithID
	}
	req, err := http.NewRequest(dataForAPI.requestType, releasesURL, responseBody)
	req.Header.Set("Accept", "application/vnd.github.v3+json")
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "token "+dataForAPI.authToken)
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
