// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import (
	_ "embed"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"strconv"
	"strings"
	"time"
)

//go:embed token
var GitHubAccessToken string

type GitHubAPI struct {
	httpClient *http.Client
}

func NewGitHubAPI() *GitHubAPI {
	return &GitHubAPI{
		httpClient: &http.Client{
			Transport: http.DefaultTransport,
			Timeout:   30 * time.Second,
		},
	}
}

func (api *GitHubAPI) TotalNumberOfRuns() (int64, error) {
	url := "https://api.github.com/repos/quick-lint/quick-lint-js/actions/runs?per_page=1"
	type ResponseData struct {
		TotalCount int64 `json:"total_count"`
	}
	var responseData ResponseData
	if err := api.Get(url, &responseData); err != nil {
		return 0, err
	}
	return responseData.TotalCount, nil
}

func (api *GitHubAPI) LoadRuns(page int64, runsPerPage int64) ([]WorkflowRun, error) {
	url := fmt.Sprintf("https://api.github.com/repos/quick-lint/quick-lint-js/actions/runs?per_page=%d&page=%d", runsPerPage, page)
	type ResponseData struct {
		WorkflowRuns []WorkflowRun `json:"workflow_runs"`
	}
	var responseData ResponseData
	if err := api.Get(url, &responseData); err != nil {
		return nil, err
	}
	return responseData.WorkflowRuns, nil
}

// Returns nil if the response hasn't changed since the previous call.
func (api *GitHubAPI) LoadJobsForWorkflowRun(storage *Storage, workflowRunID int64, page int64, jobsPerPage int64) ([]WorkflowJob, error) {
	url := fmt.Sprintf("https://api.github.com/repos/quick-lint/quick-lint-js/actions/runs/%d/jobs?per_page=%d&page=%d", workflowRunID, jobsPerPage, page)

	cachedETag, err := storage.GetGitHubHTTPCacheETag(url)
	if err != nil {
		return nil, err
	}

	request, err := api.newGetRequest(url)
	if err != nil {
		return nil, err
	}
	if cachedETag != "" {
		request.Header.Add("If-None-Match", cachedETag)
	}
	response, err := api.httpClient.Do(request)
	if err != nil {
		return nil, err
	}
	defer response.Body.Close()

	cacheIsValid := response.StatusCode == 304
	if cacheIsValid {
		return nil, nil
	}

	newETag := response.Header.Get("ETag")
	if newETag != "" {
		if err := storage.SetGitHubHTTPCacheETag(url, newETag); err != nil {
			return nil, err
		}
	}

	type ResponseData struct {
		Jobs []WorkflowJob `json:"jobs"`
	}
	var responseData ResponseData
	if err := api.parseResponse(response, &responseData); err != nil {
		return nil, err
	}
	return responseData.Jobs, nil
}

func (api *GitHubAPI) Get(url string, result interface{}) error {
	request, err := api.newGetRequest(url)
	if err != nil {
		return err
	}
	response, err := api.httpClient.Do(request)
	if err != nil {
		return err
	}
	defer response.Body.Close()

	if err := api.parseResponse(response, result); err != nil {
		return err
	}
	return nil
}

func (api *GitHubAPI) newGetRequest(url string) (*http.Request, error) {
	request, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil, err
	}
	request.Header.Add("Accept", "application/vnd.github+json")
	request.Header.Add("User-Agent", "quick-lint-js ci-analytics")
	request.Header.Add("Authorization", "token "+strings.TrimSpace(GitHubAccessToken))
	return request, nil
}

type GitHubRateLimited struct {
	resetTimestamp time.Time
	responseBody   string
}

func (e *GitHubRateLimited) Error() string {
	return "rate limited by GitHub: " + e.responseBody
}

func (e *GitHubRateLimited) SleepUntilResetTimestamp() {
	log.Printf("sleeping until GitHub rate limit resets (%v)\n", e.resetTimestamp)
	sleepDuration := e.resetTimestamp.Sub(time.Now())
	if sleepDuration < 5 {
		sleepDuration = 5
	}
	time.Sleep(sleepDuration)
}

func (api *GitHubAPI) parseResponse(response *http.Response, result interface{}) error {
	if response.StatusCode != 200 {
		body, err := ioutil.ReadAll(response.Body)
		if err != nil {
			return err
		}
		isRateLimited := response.StatusCode == 403 && response.Header.Get("X-Ratelimit-Remaining") == "0"
		if isRateLimited {
			resetTimestampString := response.Header.Get("X-Ratelimit-Reset")
			resetTimestampUnix, err := strconv.ParseInt(resetTimestampString, 10, 64)
			if err == nil {
				return &GitHubRateLimited{
					resetTimestamp: time.Unix(resetTimestampUnix, 0),
					responseBody:   string(body),
				}
			}
		}
		return fmt.Errorf("request failed with HTTP status code %d: %s", response.StatusCode, string(body))
	}

	decoder := json.NewDecoder(response.Body)
	if err := decoder.Decode(result); err != nil {
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
