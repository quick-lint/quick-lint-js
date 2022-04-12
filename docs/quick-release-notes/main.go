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

type tag struct {
	Name string `json:"name"`
}

type releaseForUpdate struct {
	Name    string `json:"name"`
	Body    string `json:"body"`
	URL     string `json:"url"`
	TagName string `json:"tag_name"`
}

type changeLog struct {
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
	authToken    string
	tags         []tag
	changeLog    changeLog
	releaseNotes []string
	repoPath     string
}

type newReleaseRequest struct {
	authToken     string
	repoPath      string
	requestType   string
	urlWithID     string
	tagForRelease string
	versionTitle  string
	releaseNote   string
}

func main() {
	// authToken := os.Args[len(os.Args)-1]
	authTokenPtr := flag.String("AuthToken", "", "a string")
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
	changeLog := getChangeLogInfo(bufio.NewScanner(file))
	releaseNotes := createReleaseNotes(changeLog)
	tagsRepoPath := *tagsRepoPtr
	tags := getTagsFromGitHub(tagsRepoPath)
	repoPath := *repoPtr
	releaseData := validateTagsHaveReleases(validationData{authToken: *authTokenPtr, tags: tags, changeLog: changeLog, releaseNotes: releaseNotes, repoPath: repoPath})
	ifReleaseNotExistMakeReleases(releaseData, *authTokenPtr, repoPath)
	ifChangeLogChangedUpdateReleases(releaseData, *authTokenPtr, repoPath)
}

func ifReleaseNotExistMakeReleases(releaseData releaseData, authToken string, repoPath string) {
	for releaseVersion, tagVersion := range releaseData.releaseVersionAndTag {
		if releaseData.releaseVersionAndTag[releaseVersion] == releaseData.tagAndReleaseVersion[tagVersion] {
			makeOrUpdateGitHubRelease(newReleaseRequest{authToken: authToken, repoPath: repoPath, requestType: "POST", urlWithID: "", tagForRelease: tagVersion, versionTitle: releaseData.tagAndReleaseVersion[releaseVersion], releaseNote: releaseData.releaseVersionAndNote[releaseVersion]})
		}
	}
}

func ifChangeLogChangedUpdateReleases(releaseData releaseData, authToken string, repoPath string) {
	releases := getReleases(authToken, repoPath)
	for _, release := range releases[:] {
		if release.Body != releaseData.releaseVersionAndNote[release.Name] {
			makeOrUpdateGitHubRelease(newReleaseRequest{authToken: authToken, repoPath: repoPath, requestType: "PATCH", urlWithID: release.URL, tagForRelease: release.TagName, versionTitle: release.Name, releaseNote: releaseData.releaseVersionAndNote[release.Name]})
		}
	}
}

func validateTagsHaveReleases(validationData validationData) releaseData {
	var redColor = "\033[31m"
	var resetColor = "\033[0m"
	releaseVersionAndTag := make(map[string]string)
	releaseVersionAndNote := make(map[string]string)
	for i, releaseVersion := range validationData.changeLog.versionNumbers[:] {
		tagVersionForMap := ""
		releaseVersionHasTag := false
		for _, tagVersion := range validationData.tags[:] {
			if releaseVersion == tagVersion.Name {
				releaseVersionHasTag = true
				tagVersionForMap = tagVersion.Name
			}
		}
		if releaseVersionHasTag == false {
			fmt.Println(redColor+"WARNING: release", releaseVersion, "missing tag"+resetColor)
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
		for _, releaseVersion := range validationData.changeLog.versionNumbers[:] {
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

func splitAndEncodeURLPath(urlPath string) (string, string) {
	urlPathSplit := strings.Split(urlPath, "/")
	owner, name := url.QueryEscape(urlPathSplit[0]), url.QueryEscape(urlPathSplit[1])
	return owner, name
}

func getReleases(authToken string, repoPath string) []releaseForUpdate {
	repoOwner, repoName := splitAndEncodeURLPath(repoPath)
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
	var eachRelease []releaseForUpdate
	err = json.Unmarshal(body, &eachRelease)
	if err != nil {
		log.Fatal(err)
	}
	return eachRelease
}

func getTagsFromGitHub(tagsRepoPath string) []tag {
	// https://docs.github.com/en/rest/reference/repos#list-repository-tags
	tagsRepoOwner, tagsRepoName := splitAndEncodeURLPath(tagsRepoPath)
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
	var tags []tag
	err = json.Unmarshal(responseFromAPI, &tags)
	if err != nil {
		log.Fatal(err)
	}
	return tags
}

func getChangeLogInfo(scanner *bufio.Scanner) changeLog {
	re := regexp.MustCompile(`## (?P<versionNumberAndDate>(?P<versionNumber>\d+\.\d+\.\d+).*)`)
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
	return changeLog{versionLineNumbers, changeLogText, changeLogLength, versionTitles, versionNumbers}
}

func createReleaseNotes(changeLog changeLog) []string {
	linkReferenceDefinitionRE, err := regexp.Compile(`^\[.+\]: .+`)
	if err != nil {
		log.Fatal(err)
	}
	lastVersionIdx := len(changeLog.versionLineNumbers) - 1
	contributorsAndErrors := ""
	for i := changeLog.versionLineNumbers[lastVersionIdx]; i < changeLog.changeLogLength; i++ {
		if linkReferenceDefinitionRE.MatchString(changeLog.changeLogText[i]) {
			contributorsAndErrors += changeLog.changeLogText[i] + "\n"
		}
	}
	var releaseNotes []string
	for i, versionLineNumber := range changeLog.versionLineNumbers[:] {
		releaseBodyLines := ""
		if i != lastVersionIdx {
			for j := changeLog.versionLineNumbers[i] + 1; j < changeLog.versionLineNumbers[i+1]; j++ {
				releaseBodyLines += changeLog.changeLogText[j] + "\n"
			}
		} else {
			// Handle last version (Currently: ## 0.2.0).
			if versionLineNumber == changeLog.versionLineNumbers[lastVersionIdx] {
				for j := 1; j < changeLog.changeLogLength-changeLog.versionLineNumbers[lastVersionIdx]; j++ {
					currentLineOfText := changeLog.changeLogText[versionLineNumber+j]
					if !linkReferenceDefinitionRE.MatchString(currentLineOfText) {
						releaseBodyLines += currentLineOfText + "\n"
					}
				}
			}
		}
		releaseNotes = append(releaseNotes, releaseBodyLines+contributorsAndErrors)
	}
	return releaseNotes
}

func makeOrUpdateGitHubRelease(newReleaseRequest newReleaseRequest) {
	// https://docs.github.com/en/rest/reference/releases
	postBody, err := json.Marshal(map[string]string{
		"tag_name": newReleaseRequest.tagForRelease,
		"name":     newReleaseRequest.versionTitle,
		"body":     newReleaseRequest.releaseNote,
	})
	if err != nil {
		log.Fatal(err)
	}
	requestBody := bytes.NewBuffer(postBody)
	releasesURL := ""
	if newReleaseRequest.requestType == "POST" {
		repoOwner, repoName := splitAndEncodeURLPath(newReleaseRequest.repoPath)
		releasesURL = fmt.Sprintf("https://api.github.com/repos/%v/%v/releases", repoOwner, repoName)
	} else if newReleaseRequest.requestType == "PATCH" {
		releasesURL = newReleaseRequest.urlWithID
	}
	req, err := http.NewRequest(newReleaseRequest.requestType, releasesURL, requestBody)
	req.Header.Set("Accept", "application/vnd.github.v3+json")
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "token "+newReleaseRequest.authToken)
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
