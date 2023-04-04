// Copyright (C) 2023  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"sync"
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
	TagVersionReleaseBodyMap map[string]string
	ReleaseVersionTagMap     map[string]string
	ReleaseVersionTitleMap   map[string]string
	TagReleaseVersionMap     map[string]string
	LatestReleaseVersion     string
}

type changeLog struct {
	changeLogText []string
	versions      []changeLogVersion
}

type releaseTagValidationInput struct {
	authToken    string
	tags         []tag
	changeLog    changeLog
	releaseNotes []string
	repoPath     string
}

type releaseRequest struct {
	authToken         string
	repoPath          string
	requestType       string
	urlWithID         string
	tagForRelease     string
	versionTitle      string
	releaseNote       string
	makeLatestRelease string
}

// Syntax highlighting for CLI warning messages.
const redColor = "\033[31m"
const resetColor = "\033[0m"

func main() {
	help, authTokenPtr, repoPtr, tagsRepoPtr := parseFlags()
	displayHelp(help)
	pathToChangeLog := getChangeLogPath()
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
	fmt.Println("Created missing releases.")
	updateReleasesIfChanged(releaseMetaData, *authTokenPtr, repoPath)
	fmt.Println("Updated releases.")
}

func parseFlags() (*bool, *string, *string, *string) {
	help := flag.Bool("help", false, "Print usage information. Example: $ go run update-release-notes.go -Repo=quick-lint/quick-lint-js -TagsRepo=quick-lint/quick-lint-js -AuthToken=$(cat token.txt)")
	authTokenPtr := flag.String("AuthToken", "", "Visit: (https://github.com/settings/tokens) generate a token with 'public_repo' or 'repo' permissions. Store access token in a file (token.txt). Example usage: -AuthToken=$(cat token.txt)")
	repoPtr := flag.String("Repo", "quick-lint/quick-lint-js", "GitHub repo where release notes to be released.")
	tagsRepoPtr := flag.String("TagsRepo", "quick-lint/quick-lint-js", "GitHub repo to get release tags from.")
	flag.Parse()
	if *authTokenPtr == "" {
		fmt.Println(redColor + "WARNING: No GitHub access token given for flag -AuthToken. Refer to --help" + resetColor)
	}
	return help, authTokenPtr, repoPtr, tagsRepoPtr
}

func displayHelp(help *bool) {
	if *help {
		fmt.Fprintf(os.Stderr, "Usage: %s [OPTIONS]\n", os.Args[0])
		flag.PrintDefaults()
		os.Exit(0)
	}
}

func getChangeLogPath() string {
	_, filename, _, _ := runtime.Caller(0)
	pathToChangeLog := filepath.Join(filepath.Dir(filename), "../docs/CHANGELOG.md")
	return pathToChangeLog
}

func handleChannels(channel chan bool, waitGroup *sync.WaitGroup) {
	timeout := time.After(10 * time.Second)
	for {
		select {
		case <-channel:
			if len(channel) == 0 {
				waitGroup.Wait()
				close(channel)
				return
			}
		case <-timeout:
			fmt.Println("Timeout: Some goroutines did not complete")
			close(channel)
			return
		}
	}
}

func createMissingReleases(releaseMetaData releaseMetaData, authToken string, repoPath string) {
	createReleaseWaitGroup := &sync.WaitGroup{}
	createReleaseChannel := make(chan bool, len(releaseMetaData.ReleaseVersionTagMap))

	spawnedThread := false
	for releaseVersion, tagVersion := range releaseMetaData.ReleaseVersionTagMap {
		if releaseMetaData.ReleaseVersionTagMap[releaseVersion] == releaseMetaData.TagReleaseVersionMap[tagVersion] {
			makeLatestRelease := "false"
			if releaseVersion == releaseMetaData.LatestReleaseVersion {
				makeLatestRelease = "true"
			} else {
				makeLatestRelease = "false"
			}
			repoOwner, repoName := splitAndEncodeURLPath(repoPath)
			requestURL := fmt.Sprintf("https://api.github.com/repos/%v/%v/releases", repoOwner, repoName)
			postRequest := releaseRequest{
				authToken:         authToken,
				repoPath:          repoPath,
				requestType:       "POST",
				tagForRelease:     tagVersion,
				versionTitle:      releaseMetaData.ReleaseVersionTitleMap[releaseVersion],
				releaseNote:       releaseMetaData.TagVersionReleaseBodyMap[tagVersion],
				makeLatestRelease: makeLatestRelease,
			}
			createReleaseWaitGroup.Add(1)
			go updateOrCreateGitHubRelease(postRequest, requestURL, createReleaseChannel, createReleaseWaitGroup)
			spawnedThread = true
		}
	}
	if spawnedThread {
		handleChannels(createReleaseChannel, createReleaseWaitGroup)
	} else {
		close(createReleaseChannel)
	}
}

func updateReleasesIfChanged(releaseMetaData releaseMetaData, authToken string, repoPath string) {
	updateReleaseWaitGroup := &sync.WaitGroup{}
	updateChannel := make(chan bool, len(releaseMetaData.ReleaseVersionTagMap))

	repoOwner, repoName := splitAndEncodeURLPath(repoPath)
	releases := getReleases(authToken, repoPath)

	spawnedThread := false
	for _, release := range releases {
		if release.Body != releaseMetaData.TagVersionReleaseBodyMap[release.TagName] {
			requestURL := fmt.Sprintf("https://api.github.com/repos/%v/%v/releases/%v", repoOwner, repoName, release.ID)
			patchRequest := releaseRequest{
				authToken:         authToken,
				repoPath:          repoPath,
				requestType:       "PATCH",
				tagForRelease:     release.TagName,
				versionTitle:      release.Name,
				releaseNote:       releaseMetaData.TagVersionReleaseBodyMap[release.TagName],
				makeLatestRelease: "false",
			}
			updateReleaseWaitGroup.Add(1)
			go updateOrCreateGitHubRelease(patchRequest, requestURL, updateChannel, updateReleaseWaitGroup)
			spawnedThread = true
		}
	}
	if spawnedThread {
		handleChannels(updateChannel, updateReleaseWaitGroup)
	} else {
		close(updateChannel)
	}
}

func validateTagsHaveReleases(releaseTagValidationInput releaseTagValidationInput) releaseMetaData {
	releaseMetaData := releaseMetaData{
		TagVersionReleaseBodyMap: make(map[string]string),
		ReleaseVersionTagMap:     make(map[string]string),
		ReleaseVersionTitleMap:   make(map[string]string),
		TagReleaseVersionMap:     make(map[string]string),
		LatestReleaseVersion:     "",
	}

	for i, release := range releaseTagValidationInput.changeLog.versions {
		releaseVersionHasTag := false
		for _, tagVersion := range releaseTagValidationInput.tags {
			if release.number == tagVersion.Name {
				releaseVersionHasTag = true
			}
		}

		if releaseVersionHasTag {
			if i == 0 {
				releaseMetaData.LatestReleaseVersion = release.number
			}
			releaseMetaData.ReleaseVersionTitleMap[release.number] = release.title
			releaseMetaData.ReleaseVersionTagMap[release.number] = release.number
			releaseMetaData.TagVersionReleaseBodyMap[release.number] = releaseTagValidationInput.releaseNotes[i]
		} else {
			fmt.Println(redColor+"WARNING: release", release, "missing tag"+resetColor)
		}
	}

	for _, tag := range releaseTagValidationInput.tags {
		tagHasVersionNumber := false
		for _, release := range releaseTagValidationInput.changeLog.versions {
			if tag.Name == release.number {
				tagHasVersionNumber = true
			}
		}
		if tagHasVersionNumber {
			releaseMetaData.TagReleaseVersionMap[tag.Name] = tag.Name
		} else {
			fmt.Println(redColor+"WARNING: tag", tag.Name, "missing changelog entry"+resetColor)
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
	body, err := io.ReadAll(resp.Body)
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
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	var tags []tag
	err = json.Unmarshal(body, &tags)
	if err != nil {
		log.Fatal(err)
	}
	return tags
}

func getChangeLogInfo(scanner *bufio.Scanner) changeLog {

	titleAndVersionRE := regexp.MustCompile(`##\s*(?P<title>(?P<version>\d+(\.\d+)*).*)`)

	unreleasedRE := regexp.MustCompile(`## Unreleased`)
	var changeLogText []string
	var versions []changeLogVersion

	for scanner.Scan() {
		changeLogText = append(changeLogText, scanner.Text())
		unreleased := unreleasedRE.FindStringSubmatch(scanner.Text())

		titleAndVersionNumber := titleAndVersionRE.FindStringSubmatch(scanner.Text())
		if unreleased != nil {
			fmt.Println(redColor+"WARNING: Line:", len(changeLogText)-1, "## Unreleased section won't be synced to GitHub"+resetColor)
		}
		if titleAndVersionNumber != nil {
			versionIndex := titleAndVersionRE.SubexpIndex("version")
			titleIndex := titleAndVersionRE.SubexpIndex("title")
			title := titleAndVersionNumber[titleIndex]
			versionNumber := titleAndVersionNumber[versionIndex]
			changeLogVersion := changeLogVersion{title: title, number: versionNumber, lineNumber: len(changeLogText) - 1}
			versions = append(versions, changeLogVersion)
		}
	}
	if scanner.Err() != nil {
		fmt.Println(scanner.Err())
	}
	changeLog := changeLog{
		changeLogText: changeLogText,
		versions:      versions,
	}
	return changeLog
}

func createReleaseNotes(changeLog changeLog) []string {
	linkReferenceDefinitionRE, err := regexp.Compile(`^\[.+\]: .+`)
	if err != nil {
		log.Fatal(err)
	}
	contributorsAndErrors := bytes.Buffer{}
	for _, line := range changeLog.changeLogText {
		if linkReferenceDefinitionRE.MatchString(line) {
			contributorsAndErrors.WriteString(line + "\n")
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
			versionEndLine = len(changeLog.changeLogText)
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

func updateOrCreateGitHubRelease(releaseRequest releaseRequest, requestURL string, channel chan<- bool, waitGroup *sync.WaitGroup) {
	defer waitGroup.Done()
	postBody, err := json.Marshal(map[string]interface{}{
		"name":        releaseRequest.versionTitle,
		"tag_name":    releaseRequest.tagForRelease,
		"body":        releaseRequest.releaseNote,
		"make_latest": releaseRequest.makeLatestRelease,
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
	repoPermission := false
	if len(resp.Header["X-Oauth-Scopes"]) > 0 {
		for _, permission := range resp.Header["X-Oauth-Scopes"] {
			// public_repo is the least permission; use repo for a private repo.
			if permission == "public_repo" || permission == "repo" {
				repoPermission = true
			}
		}
	} else {
		log.Fatalln(redColor + "Error: GitHub access Token has no permissions at all for X-Oauth-Scopes (select `public_repo` or `repo` scopes)" + resetColor)
	}
	if !repoPermission {
		log.Fatalln("Error: GitHub access Token doesn't include X-Oauth-Scope: `public_repo` or `repo`.")
	}
	channel <- true
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2023  Matthew "strager" Glazar
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
