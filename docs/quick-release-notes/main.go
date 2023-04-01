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
	"time"
)

type tag struct {
	Name string `json:"name"`
}

type releaseForUpdate struct {
	ID      int    `json:"id"`
	Name    string `json:"name"`
	Body    string `json:"body"`
	URL     string `json:"url"`
	TagName string `json:"tag_name"`
}

type changeLogVersion struct {
	lineNumber int
	title      string
	number     string
}

type releaseMetaData struct {
	ReleaseVersionNoteMap map[string]string
	ReleaseVersionTagMap  map[string]string
	TagReleaseVersionMap  map[string]string
}

type changeLog struct {
	changeLogText   []string
	changeLogLength int
	versions        []changeLogVersion
}

type releaseTagValidationInput struct {
	authToken    string
	tags         []tag
	changeLog    changeLog
	releaseNotes []string
	repoPath     string
}

type releaseRequest struct {
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
	start := time.Now()
	authTokenPtr := flag.String("AuthToken", "", "a string")
	repoPtr := flag.String("Repo", "quick-lint/quick-lint-js", "a string")
	tagsRepoPtr := flag.String("TagsRepo", "quick-lint/quick-lint-js", "a string")
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
	releaseTagValidationInput := releaseTagValidationInput{
		authToken:    *authTokenPtr,
		tags:         tags,
		changeLog:    changeLog,
		releaseNotes: releaseNotes,
		repoPath:     repoPath,
	}
	releaseMetaData := validateTagsHaveReleases(releaseTagValidationInput)
	createMissingReleases(releaseMetaData, *authTokenPtr, repoPath)
	updateReleasesIfChanged(releaseMetaData, *authTokenPtr, repoPath)
	elapsed := time.Since(start)
	fmt.Println("Program finished in: ", elapsed)
}

func createMissingReleases(releaseMetaData releaseMetaData, authToken string, repoPath string) {
	for releaseVersion, tagVersion := range releaseMetaData.ReleaseVersionTagMap {
		if releaseMetaData.ReleaseVersionTagMap[releaseVersion] == releaseMetaData.TagReleaseVersionMap[tagVersion] {
			repoOwner, repoName := splitAndEncodeURLPath(repoPath)
			requestURL := fmt.Sprintf("https://api.github.com/repos/%v/%v/releases", repoOwner, repoName)
			postRequest := releaseRequest{
				authToken:     authToken,
				repoPath:      repoPath,
				requestType:   "POST",
				tagForRelease: tagVersion,
				versionTitle:  releaseMetaData.TagReleaseVersionMap[releaseVersion],
				releaseNote:   releaseMetaData.ReleaseVersionNoteMap[releaseVersion],
			}
			updateOrCreateGitHubRelease(postRequest, requestURL)
		}
	}
}

func updateReleasesIfChanged(releaseMetaData releaseMetaData, authToken string, repoPath string) {
	repoOwner, repoName := splitAndEncodeURLPath(repoPath)
	releases := getReleases(authToken, repoPath)
	for _, release := range releases[:] {
		if release.Body != releaseMetaData.ReleaseVersionNoteMap[release.Name] {
			requestURL := fmt.Sprintf("https://api.github.com/repos/%v/%v/releases/%v", repoOwner, repoName, release.ID)
			patchRequest := releaseRequest{
				authToken:     authToken,
				repoPath:      repoPath,
				requestType:   "PATCH",
				tagForRelease: release.TagName,
				versionTitle:  release.Name,
				releaseNote:   releaseMetaData.ReleaseVersionNoteMap[release.Name],
			}
			updateOrCreateGitHubRelease(patchRequest, requestURL)
		}
	}
}

func validateTagsHaveReleases(releaseTagValidationInput releaseTagValidationInput) releaseMetaData {
	releaseMetaData := releaseMetaData{
		ReleaseVersionNoteMap: make(map[string]string),
		ReleaseVersionTagMap:  make(map[string]string),
		TagReleaseVersionMap:  make(map[string]string),
	}
	for i, releaseVersion := range releaseTagValidationInput.changeLog.versions[:] {
		tagVersionForMap := ""
		releaseVersionHasTag := false
		for _, tagVersion := range releaseTagValidationInput.tags[:] {
			if releaseVersion.number == tagVersion.Name {
				releaseVersionHasTag = true
				tagVersionForMap = tagVersion.Name
			}
		}
		if releaseVersionHasTag == false {
			fmt.Println(redColor+"WARNING: release", releaseVersion, "missing tag"+resetColor)
		}
		if releaseVersionHasTag {
			releaseMetaData.ReleaseVersionTagMap[releaseVersion.number] = tagVersionForMap
			releaseMetaData.ReleaseVersionNoteMap[releaseVersion.number] = releaseTagValidationInput.releaseNotes[i]
		}
	}
	for _, tagVersion := range releaseTagValidationInput.tags[:] {
		releaseVersionForMap := ""
		tagHasVersionNumber := false
		for _, releaseVersion := range releaseTagValidationInput.changeLog.versions[:] {
			if tagVersion.Name == releaseVersion.number {
				tagHasVersionNumber = true
				releaseVersionForMap = releaseVersion.number
			}
		}
		if tagHasVersionNumber == false {
			fmt.Println(redColor+"WARNING: tag", tagVersion.Name, "missing changelog entry"+resetColor)
		}
		if tagHasVersionNumber {
			releaseMetaData.TagReleaseVersionMap[tagVersion.Name] = releaseVersionForMap
		}
	}
	return releaseMetaData
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
			changeLogVersion := changeLogVersion{title: versionNumberAndDate, number: versionNumber, lineNumber: len(changeLogText) - 1}
			versions = append(versions, changeLogVersion)
		}
	}
	if scanner.Err() != nil {
		fmt.Println(scanner.Err())
	}
	changeLog := changeLog{
		changeLogText:   changeLogText,
		changeLogLength: len(changeLogText),
		versions:        versions,
	}
	return changeLog
}

func createReleaseNotes(changeLog changeLog) []string {
	linkReferenceDefinitionRE, err := regexp.Compile(`^\[.+\]: .+`)
	if err != nil {
		log.Fatal(err)
	}
	contributorsAndErrors := bytes.Buffer{}
	for i, line := range changeLog.changeLogText {
		if linkReferenceDefinitionRE.MatchString(line) {
			contributorsAndErrors.WriteString(changeLog.changeLogText[i] + "\n")
		}
	}
	lastVersionIdx := len(changeLog.versions) - 1
	var releaseNotes []string
	for i, version := range changeLog.versions {
		releaseBody := bytes.Buffer{}
		var versionEndLine int
		if i < lastVersionIdx {
			versionEndLine = changeLog.versions[i+1].lineNumber
		} else {
			versionEndLine = changeLog.changeLogLength
		}

		for j := version.lineNumber + 1; j < versionEndLine; j++ {
			if linkReferenceDefinitionRE.MatchString(changeLog.changeLogText[j]) {
				continue
			}
			releaseBody.WriteString(changeLog.changeLogText[j] + "\n")
		}

		releaseNotes = append(releaseNotes, releaseBody.String()+contributorsAndErrors.String())
	}
	return releaseNotes
}

func updateOrCreateGitHubRelease(releaseRequest releaseRequest, requestURL string) {
	postBody, err := json.Marshal(map[string]interface{}{
		"name":     releaseRequest.versionTitle,
		"tag_name": releaseRequest.tagForRelease,
		"body":     releaseRequest.releaseNote,
	})
	if err != nil {
		log.Fatal(err)
	}
	requestBody := bytes.NewBuffer(postBody)
	req, err := http.NewRequest(releaseRequest.requestType, requestURL, requestBody)
	req.Header.Set("Accept", "application/vnd.github.v3+json")
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "token "+releaseRequest.authToken)
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		if resp.StatusCode == http.StatusNotFound {
			fmt.Printf("Error: Status code: %d - GitHub release not found.\n", http.StatusNotFound)
			return
		} else if resp.StatusCode == http.StatusInternalServerError {
			fmt.Printf("Error: Status code: %d - Internal server error.\n", http.StatusInternalServerError)
			return
		} else {
			log.Fatal(err)
		}
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
