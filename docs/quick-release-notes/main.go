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
	ID      int    `json:"id"`
	Name    string `json:"name"`
	Body    string `json:"body"`
	URL     string `json:"url"`
	Draft   bool   `json:"draft"`
	TagName string `json:"tag_name"`
}

type changeLog struct {
	changeLogText   []string
	changeLogLength int
	versions        []changeLogVersion
}

type changeLogVersion struct {
	lineNumber int
	title      string
	number     string
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

// Syntax highlighting for CLI warning messages.
var redColor = "\033[31m"
var resetColor = "\033[0m"

func main() {
	authTokenPtr := flag.String("AuthToken", "", "a string")
	repoPtr := flag.String("Repo", "quick-lint/quick-lint-js", "a string")
	tagsRepoPtr := flag.String("TagsRepo", "quick-lint/quick-lint-js", "a string")
	isDraftReleasePtr := flag.Bool("isDraft", false, "This is a bool argument")
	flag.Parse()
	if *authTokenPtr == "" {
		fmt.Println(redColor + "WARNING: No GitHub personal access token given for flag -AuthToken. Refer to INSTRUCTIONS.md" + resetColor)
	}
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
	tags := getTagsFromGitHub(*tagsRepoPtr)
	repoPath := *repoPtr
	releaseData := validateTagsHaveReleases(validationData{authToken: *authTokenPtr, tags: tags, changeLog: changeLog, releaseNotes: releaseNotes, repoPath: repoPath})
	ifReleaseNotExistMakeReleases(releaseData, *authTokenPtr, repoPath, *isDraftReleasePtr)
	ifChangeLogChangedUpdateReleases(releaseData, *authTokenPtr, repoPath)
}

func ifReleaseNotExistMakeReleases(releaseData releaseData, authToken string, repoPath string, isDraftRelease bool) {

	releases := getReleases(authToken, repoPath)
	for _, release := range releases[:] {
		if release.Draft == true {
			repoOwner, repoName := splitAndEncodeURLPath(repoPath)
			releasePath := fmt.Sprintf("https://api.github.com/repos/%v/%v/releases/%v", repoOwner, repoName, release.ID)
			req, err := http.NewRequest("DELETE", releasePath, nil)
			req.Header.Set("Accept", "application/vnd.github.v3+json")
			req.Header.Set("Content-Type", "application/json")
			req.Header.Set("Authorization", "token "+authToken)
			if err != nil {
				log.Fatal(err)
			}
			http.DefaultClient.Do(req)
		}
	}
	for releaseVersion, tagVersion := range releaseData.releaseVersionAndTag {
		if releaseData.releaseVersionAndTag[releaseVersion] == releaseData.tagAndReleaseVersion[tagVersion] {
			repoOwner, repoName := splitAndEncodeURLPath(repoPath)
			requestURL := fmt.Sprintf("https://api.github.com/repos/%v/%v/releases", repoOwner, repoName)
			makeOrUpdateGitHubRelease(newReleaseRequest{authToken: authToken, repoPath: repoPath, requestType: "POST", tagForRelease: tagVersion, versionTitle: releaseData.tagAndReleaseVersion[releaseVersion], releaseNote: releaseData.releaseVersionAndNote[releaseVersion]}, isDraftRelease, requestURL)
		}
	}
}

func ifChangeLogChangedUpdateReleases(releaseData releaseData, authToken string, repoPath string) {
	repoOwner, repoName := splitAndEncodeURLPath(repoPath)
	releases := getReleases(authToken, repoPath)
	for _, release := range releases[:] {
		if release.Body != releaseData.releaseVersionAndNote[release.Name] && release.Draft != true {
			requestURL := fmt.Sprintf("https://api.github.com/repos/%v/%v/releases/%v", repoOwner, repoName, release.ID)
			makeOrUpdateGitHubRelease(newReleaseRequest{authToken: authToken, repoPath: repoPath, requestType: "PATCH", tagForRelease: release.TagName, versionTitle: release.Name, releaseNote: releaseData.releaseVersionAndNote[release.Name]}, release.Draft, requestURL)
		}
	}
}

func validateTagsHaveReleases(validationData validationData) releaseData {
	releaseVersionAndTag := make(map[string]string)
	releaseVersionAndNote := make(map[string]string)
	for i, releaseVersion := range validationData.changeLog.versions[:] {
		tagVersionForMap := ""
		releaseVersionHasTag := false
		for _, tagVersion := range validationData.tags[:] {
			if releaseVersion.number == tagVersion.Name {
				releaseVersionHasTag = true
				tagVersionForMap = tagVersion.Name
			}
		}
		if releaseVersionHasTag == false {
			fmt.Println(redColor+"WARNING: release", releaseVersion, "missing tag"+resetColor)
		}
		if releaseVersionHasTag {
			releaseVersionAndTag[releaseVersion.number] = tagVersionForMap
			releaseVersionAndNote[releaseVersion.number] = validationData.releaseNotes[i]
		}
	}
	tagAndReleaseVersion := make(map[string]string)
	for _, tagVersion := range validationData.tags[:] {
		releaseVersionForMap := ""
		tagHasVersionNumber := false
		for _, releaseVersion := range validationData.changeLog.versions[:] {
			if tagVersion.Name == releaseVersion.number {
				tagHasVersionNumber = true
				releaseVersionForMap = releaseVersion.number
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
		fmt.Println(redColor + string(body) + resetColor)
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
	versionNumberAndDateRE := regexp.MustCompile(`## (?P<versionNumberAndDate>(?P<versionNumber>\d+\.\d+\.\d+).*)`)
	unreleasedRE := regexp.MustCompile(`## Unreleased`)
	var changeLogText []string
	var versions []changeLogVersion

	for scanner.Scan() {
		changeLogText = append(changeLogText, scanner.Text())
		unreleased := unreleasedRE.FindStringSubmatch(scanner.Text())

		hashVersionAndDate := versionNumberAndDateRE.FindStringSubmatch(scanner.Text())
		if unreleased != nil {
			fmt.Println(redColor+"WARNING: Line:", len(changeLogText)-1, "## Unreleased section won't be synced to GitHub"+resetColor)
		}
		if hashVersionAndDate != nil {
			idxVersionNumberAndDate := versionNumberAndDateRE.SubexpIndex("versionNumberAndDate")
			idxVersionNumber := versionNumberAndDateRE.SubexpIndex("versionNumber")
			versionNumberAndDate := hashVersionAndDate[idxVersionNumberAndDate]
			versionNumber := hashVersionAndDate[idxVersionNumber]
			versions = append(versions, changeLogVersion{title: versionNumberAndDate, number: versionNumber, lineNumber: len(changeLogText) - 1})
		}
	}
	if scanner.Err() != nil {
		fmt.Println(scanner.Err())
	}
	return changeLog{changeLogText: changeLogText, changeLogLength: len(changeLogText), versions: versions}
}

func createReleaseNotes(changeLog changeLog) []string {
	linkReferenceDefinitionRE, err := regexp.Compile(`^\[.+\]: .+`)
	if err != nil {
		log.Fatal(err)
	}
	lastVersionIdx := len(changeLog.versions) - 1
	contributorsAndErrors := ""
	for i, line := range changeLog.changeLogText {
		if linkReferenceDefinitionRE.MatchString(line) {
			contributorsAndErrors += changeLog.changeLogText[i] + "\n"
		}
	}
	var releaseNotes []string
	for i, version := range changeLog.versions[:] {
		releaseBodyLines := ""
		var nextVersionLineNumber int
		if !(i == (lastVersionIdx)) {
			nextVersionLineNumber = changeLog.versions[i+1].lineNumber
		} else {
			nextVersionLineNumber = changeLog.changeLogLength
		}
		for j := version.lineNumber + 1; j < nextVersionLineNumber; j++ {
			if !linkReferenceDefinitionRE.MatchString(changeLog.changeLogText[j]) {
				releaseBodyLines += changeLog.changeLogText[j] + "\n"
			}
		}

		releaseNotes = append(releaseNotes, releaseBodyLines+contributorsAndErrors)
	}
	return releaseNotes
}

func makeOrUpdateGitHubRelease(newReleaseRequest newReleaseRequest, isDraftRelease bool, requestURL string) {
	postBody, err := json.Marshal(map[string]interface{}{
		"name":     newReleaseRequest.versionTitle,
		"tag_name": newReleaseRequest.tagForRelease,
		"body":     newReleaseRequest.releaseNote,
		"draft":    isDraftRelease,
	})
	if err != nil {
		log.Fatal(err)
	}
	requestBody := bytes.NewBuffer(postBody)
	req, err := http.NewRequest(newReleaseRequest.requestType, requestURL, requestBody)
	req.Header.Set("Accept", "application/vnd.github.v3+json")
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "token "+newReleaseRequest.authToken)
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()
	// https://docs.github.com/en/developers/apps/building-oauth-apps/scopes-for-oauth-apps
	if len(resp.Header["X-Oauth-Scopes"]) > 0 {
		for _, permission := range resp.Header["X-Oauth-Scopes"] {
			// public_repo is the least permission; use repo for a private repo.
			if permission == "public_repo" || permission == "repo" {
			} else {
				fmt.Println(redColor + "WARNING: token doesn't include X-Oauth-Scope: public_repo or repo access. " + resetColor)
			}
		}
	} else {
		fmt.Println(redColor + "WARNING: GitHub access Token has no permissions for X-Oauth-Scopes (select public_repo or repo scopes)" + resetColor)
	}
	fmt.Print("Token Rate-limit:", resp.Header["X-Ratelimit-Remaining"])
	fmt.Println(" Token Expiration:", resp.Header["Github-Authentication-Token-Expiration"])
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
