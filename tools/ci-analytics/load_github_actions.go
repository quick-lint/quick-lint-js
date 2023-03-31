// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import (
	"flag"
	"log"
	"math/rand"
	"os"
	"sync"
)

func main() {
	var err error

	var dontFetchRuns bool
	flag.BoolVar(&dontFetchRuns, "DontFetchRuns", false, "")
	var dontFetchJobs bool
	flag.BoolVar(&dontFetchJobs, "DontFetchJobs", false, "")
	flag.Parse()
	if flag.NArg() != 0 {
		log.Printf("error: unexpected command line argument\n")
		os.Exit(2)
	}

	storage, err := OpenStorage("analytics.sqlite3")
	if err != nil {
		log.Fatal(err)
	}
	defer storage.Close()
	if err := storage.CreateTables(); err != nil {
		log.Fatal(err)
	}

	// TODO(strager): Create one HTTP client per thread. I suspect Go's HTTP
	// client doesn't support parallel requests.
	gitHubAPI := NewGitHubAPI()

	var totalRunCount int64
	if !dontFetchRuns {
	retry_get_total_run_count:
		totalRunCount, err = gitHubAPI.TotalNumberOfRuns()
		if err != nil {
			if SleepIfRateLimited(err) {
				goto retry_get_total_run_count
			}
			log.Fatal(err)
		}
	}

	threadCount := 5
	pool := NewWorkerPool(threadCount)

	var runsPerPage int64 = 100

	didLoadAndSaveJob := sync.Map{}
	loadAndSaveJob := func(runID int64) error {
		if dontFetchJobs {
			return nil
		}
		_, alreadyLoadedAndStored := didLoadAndSaveJob.LoadOrStore(runID, nil)
		if alreadyLoadedAndStored {
			return nil
		}

		var pageNumber int64 = 1
		var jobsPerPage int64 = 100
	retry_load_jobs:
		jobs, err := gitHubAPI.LoadJobsForWorkflowRun(storage, runID, pageNumber, jobsPerPage)
		if err != nil {
			if SleepIfRateLimited(err) {
				goto retry_load_jobs
			}
			return err
		}
		if jobs == nil {
			log.Printf("cached jobs for run %d\n", runID)
		} else {
			log.Printf("loaded %d jobs for run %d\n", len(jobs), runID)
			if err := storage.SaveJobs(jobs); err != nil {
				return err
			}
			log.Printf("saved %d jobs\n", len(jobs))
		}
		return nil
	}

	loadAndSaveRunsPage := func(pageNumber int64) error {
		if dontFetchRuns {
			return nil
		}

	retry_load_runs:
		runs, err := gitHubAPI.LoadRuns(pageNumber, runsPerPage)
		if err != nil {
			if SleepIfRateLimited(err) {
				goto retry_load_runs
			}
			return err
		}
		log.Printf("loaded %d runs from page %d\n", len(runs), pageNumber)

		for _, run := range runs {
			runID := int64(run["id"].(float64)) // Capture in closure.
			pool.AddWork(func() error {
				return loadAndSaveJob(runID)
			})
		}

		if err := storage.SaveRuns(runs); err != nil {
			return err
		}
		log.Printf("saved %d runs\n", len(runs))
		return nil
	}

	if !dontFetchRuns {
		log.Printf("reading %d runs ...\n", totalRunCount)
		for pageNumber := int64(1); pageNumber <= (totalRunCount+runsPerPage-1)/runsPerPage; pageNumber += 1 {
			pn := pageNumber // Capture in closure.
			pool.AddWork(func() error {
				return loadAndSaveRunsPage(pn)
			})
		}
	}

	if !dontFetchJobs {
		runIDsFromPreviousInvocation, err := storage.GetAllRunIDs()
		if err != nil {
			log.Printf("failed to get old workflow run IDs: %v", err)
		}
		for _, runID := range runIDsFromPreviousInvocation {
			rid := runID // Capture in closure.
			pool.AddWork(func() error {
				return loadAndSaveJob(rid)
			})
		}
	}

	errs := pool.WaitAndStop()
	for _, err := range errs {
		if err != nil {
			os.Exit(1)
		}
	}
}

type WorkerPoolWork = func() error

type WorkerPool struct {
	mutex      sync.Mutex
	workerCond *sync.Cond

	// Protected by mutex:
	queue []WorkerPoolWork
	done  bool

	workerErrors []error
	wg           sync.WaitGroup
}

func NewWorkerPool(threadCount int) *WorkerPool {
	pool := &WorkerPool{
		queue:        []WorkerPoolWork{},
		done:         false,
		workerErrors: make([]error, threadCount),
	}
	pool.workerCond = sync.NewCond(&pool.mutex)

	pool.wg.Add(threadCount)
	for i := 0; i < threadCount; i += 1 {
		go pool.runWorker(i)
	}

	return pool
}

func (pool *WorkerPool) AddWork(work WorkerPoolWork) {
	pool.mutex.Lock()
	pool.queue = append(pool.queue, work)
	pool.mutex.Unlock()
	pool.workerCond.Signal()
}

func (pool *WorkerPool) WaitAndStop() []error {
	pool.mutex.Lock()
	pool.done = true
	pool.mutex.Unlock()

	pool.workerCond.Broadcast()
	pool.wg.Wait()
	return pool.workerErrors
}

func (pool *WorkerPool) runWorker(threadIndex int) {
	var err error = nil
	rng := rand.New(rand.NewSource(int64(threadIndex)))
work_loop:
	for {
		pool.mutex.Lock()
		for len(pool.queue) == 0 {
			if pool.done {
				pool.mutex.Unlock()
				break work_loop
			}
			pool.workerCond.Wait()
		}

		workIndex := rng.Intn(len(pool.queue))
		work := TakeAt(&pool.queue, workIndex)
		pool.mutex.Unlock()

		err = work()
		if err != nil {
			log.Printf("worker failed: %v\n", err)
			break
		}
	}
	pool.workerErrors[threadIndex] = err
	pool.wg.Done()
}

func TakeAt[T any](xs *[]T, index int) T {
	x := (*xs)[index]
	(*xs)[index] = (*xs)[len(*xs)-1]
	*xs = (*xs)[0 : len(*xs)-1 : cap(*xs)]
	return x
}

func SleepIfRateLimited(err error) bool {
	if e, isRateLimited := err.(*GitHubRateLimited); isRateLimited {
		log.Printf("%v", e)
		e.SleepUntilResetTimestamp()
		return true
	}
	return false
}

type WorkflowJob = map[string]interface{}
type WorkflowRun = map[string]interface{}

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
