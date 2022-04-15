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
	fmt.Println(releaseData.releaseVersionAndTag)
	ifReleaseNotExistMakeReleases(releaseData, *authTokenPtr, repoPath, *isDraftReleasePtr)
	ifChangeLogChangedUpdateReleases(releaseData, *authTokenPtr, repoPath, *isDraftReleasePtr)
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
			makeOrUpdateGitHubRelease(newReleaseRequest{authToken: authToken, repoPath: repoPath, requestType: "POST", urlWithID: "", tagForRelease: tagVersion, versionTitle: releaseData.tagAndReleaseVersion[releaseVersion], releaseNote: releaseData.releaseVersionAndNote[releaseVersion]}, isDraftRelease)
		}
	}
}

func ifChangeLogChangedUpdateReleases(releaseData releaseData, authToken string, repoPath string, isDraftRelease bool) {
	releases := getReleases(authToken, repoPath)
	for _, release := range releases[:] {
		if release.Body != releaseData.releaseVersionAndNote[release.Name] && release.Draft != true {
			fmt.Println("updating")
			makeOrUpdateGitHubRelease(newReleaseRequest{authToken: authToken, repoPath: repoPath, requestType: "PATCH", urlWithID: release.URL, tagForRelease: release.TagName, versionTitle: release.Name, releaseNote: releaseData.releaseVersionAndNote[release.Name]}, false)
		}
	}
}

func validateTagsHaveReleases(validationData validationData) releaseData {
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
	// fmt.Println(resp.Body)
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

func makeOrUpdateGitHubRelease(newReleaseRequest newReleaseRequest, isDraftRelease bool) {
	// https://docs.github.com/en/rest/reference/releases
	// isDraftReleaseTest := fmt.Sprintf("%t", isDraftRelease)
	// fmt.Println(isDraftRelease)
	// fmt.Printf("%v %t", isDraftRelease, isDraftRelease)

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
	// fmt.Println(requestBody)

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
