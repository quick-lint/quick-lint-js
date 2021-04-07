// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

// Test quick-lint-js with test262.
//
// https://github.com/tc39/test262

package main

import "bytes"
import "flag"
import "fmt"
import "io"
import "io/ioutil"
import "log"
import "os"
import "os/exec"
import "path/filepath"
import "runtime"
import "sort"
import "strings"
import "sync"
import "sync/atomic"

var testTodo = TestTodo{
	TodoPaths: []string{
		// These files look like tests, but they are not.
		// TODO(strager): Check these files for crashes anyway, but
		// don't check their assertions.
		"tools/generation/test/expected/glob*/normal/*-nested/*.js",
		"tools/generation/test/expected/glob*/normal/*.js",
		"tools/generation/test/expected/multiple/glob/*.js",
		"tools/generation/test/expected/multiple/normal/*-nested/*.js",
		"tools/generation/test/expected/multiple/normal/*.js",
		"tools/generation/test/expected/normal/*.js",
		"tools/generation/test/expected/normal/nested/*.js",
		"tools/lint/test/fixtures/*.js",

		// TODO(strager): Implement non-standard and new features.
		"language/module-code/export-expname_FIXTURE.js",

		// TODO(strager): Implement strict mode.
		"language/directive-prologue/10.1.1-2gs.js",
		"language/directive-prologue/10.1.1-5gs.js",
		"language/directive-prologue/10.1.1-8gs.js",
		"language/directive-prologue/14.1-4gs.js",
		"language/directive-prologue/14.1-5gs.js",

		// TODO(strager): Implement private fields.
		"v8/mjsunit/harmony/private-fields*.js",
	},
	TodoFeatures: [][]byte{
		// TODO(strager): Implement non-standard and new features.
		[]byte("arbitrary-module-namespace-names"),
		[]byte("async-iteration"),
		[]byte("class-fields-private"),
		[]byte("class-methods-private"),
		[]byte("class-static-fields-private"),
		[]byte("class-static-methods-private"),
		[]byte("top-level-await"),
	},
}

func main() {
	quickLintJSExecutable := flag.String(
		"quick-lint-js",
		"quick-lint-js",
		"path to the quick-lint-js executable",
	)
	stopOnFirstFailure := flag.Bool(
		"stop-on-first-failure",
		false,
		"print only the first failing test (if any)",
	)
	flag.Parse()
	if flag.NArg() == 0 {
		os.Stderr.WriteString(fmt.Sprintf("error: missing test fixture directory\n"))
		os.Exit(2)
	}

	var testFiles []TestFile
	for _, testDirectory := range flag.Args() {
		FindTests(testDirectory, &testFiles)
	}
	sort.Slice(testFiles, func(i int, j int) bool {
		return testFiles[i].Path < testFiles[j].Path
	})

	threadCount := runtime.NumCPU()
	queue := MakeWorkQueue(*quickLintJSExecutable, testFiles, threadCount, *stopOnFirstFailure)

	for i := 0; i < queue.threadCount; i++ {
		queue.wg.Add(1)
		go RunWorker(queue, i)
	}

	queue.wg.Wait()

	if *stopOnFirstFailure {
		lowestFailingIndex := -1
		var failure *LintResult = nil
		for threadIndex, failureIndex := range queue.failureIndexes {
			if failureIndex != -1 && (lowestFailingIndex == -1 || failureIndex < lowestFailingIndex) {
				lowestFailingIndex = failureIndex
				failure = queue.failures[threadIndex]
			}
		}
		if failure != nil {
			failure.Dump(os.Stderr)
			os.Exit(1)
		}
	} else {
		if atomic.LoadInt64(&queue.minimumFailingIndex) >= 0 {
			os.Exit(1)
		}
	}
}

// If stopOnFirstFailure is true, then output is deterministic. This makes
// development of quick-lint-js easier.
//
// If stopOnFirstFailure is false, then output is non-deterministic. This makes
// testing faster.
type WorkQueue struct {
	quickLintJSExecutable string
	testFiles             []TestFile
	threadCount           int
	wg                    sync.WaitGroup
	stopOnFirstFailure    bool

	// outputMutex locks dumping test failures.
	//
	// Used only if !stopOnFirstFailure
	outputMutex sync.Mutex

	// minimumFailingIndex is -1 or an index into testFiles.
	//
	// Any thread can *atomically* read from or CAS-write to
	// minimumFailingIndex.
	minimumFailingIndex int64

	// failureIndexes is keyed by thread index. Each value is an index into
	// testFiles, or is -1.
	//
	// Used only if stopOnFirstFailure.
	//
	// Each thread can non-atomically read and write its own entry in the
	// failureIndexes slice. The main thread can read any entry from the
	// failureIndexes slice only after all goroutines have finished.
	failureIndexes []int

	// failures is keyed by thread index.
	//
	// Used only if stopOnFirstFailure.
	//
	// Each thread can read and write its own entry in the failures slice.
	// The main thread can read any entry from the failures slice only after
	// all goroutines have finished.
	failures []*LintResult
}

func MakeWorkQueue(quickLintJSExecutable string, testFiles []TestFile, threadCount int, stopOnFirstFailure bool) *WorkQueue {
	queue := WorkQueue{
		failureIndexes:        make([]int, threadCount),
		failures:              make([]*LintResult, threadCount),
		minimumFailingIndex:   -1,
		quickLintJSExecutable: quickLintJSExecutable,
		stopOnFirstFailure:    stopOnFirstFailure,
		testFiles:             testFiles,
		threadCount:           threadCount,
	}
	for i := 0; i < len(queue.failureIndexes); i++ {
		queue.failureIndexes[i] = -1
	}
	return &queue
}

func RunWorker(queue *WorkQueue, threadIndex int) {
	defer queue.wg.Done()
	for i := threadIndex; i < len(queue.testFiles); i += queue.threadCount {
		if queue.stopOnFirstFailure && queue.HaveEarlierFailure(i) {
			break
		}
		testFile := queue.testFiles[i]
		// TODO(strager): Always ignore warnings.
		ignoreWarnings := !testFile.Expectations.EarlyError

		result := RunQuickLintJS(queue.quickLintJSExecutable, testFile.Path, ignoreWarnings)
		if result.Crashed() ||
			(testFile.Expectations.IsTest && !testFile.Expectations.EarlyError && !result.ExitedWithCode(0)) ||
			(testFile.Expectations.IsTest && testFile.Expectations.EarlyError && result.ExitedWithCode(0)) {
			queue.RecordFailure(threadIndex, i, &result)
			if queue.stopOnFirstFailure {
				break
			}
		}
	}
}

func (queue *WorkQueue) HaveEarlierFailure(index int) bool {
	minimumFailingIndex := atomic.LoadInt64(&queue.minimumFailingIndex)
	return minimumFailingIndex != -1 && int(minimumFailingIndex) < index
}

func (queue *WorkQueue) RecordFailure(threadIndex int, index int, result *LintResult) {
	queue.failures[threadIndex] = result
	queue.failureIndexes[threadIndex] = index

	oldMinimumFailingIndex := atomic.LoadInt64(&queue.minimumFailingIndex)
	for index < int(oldMinimumFailingIndex) {
		if atomic.CompareAndSwapInt64(
			&queue.minimumFailingIndex,
			oldMinimumFailingIndex,
			int64(index),
		) {
			break
		}
		oldMinimumFailingIndex = atomic.LoadInt64(&queue.minimumFailingIndex)
	}

	if !queue.stopOnFirstFailure {
		queue.outputMutex.Lock()
		defer queue.outputMutex.Unlock()
		result.Dump(os.Stderr)
	}
}

func FindTests(test262FixtureDirectory string, testFiles *[]TestFile) {
	err := filepath.Walk(test262FixtureDirectory, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(path, ".js") {
			testExpectations := ReadTestExpectations(testTodo, path)
			if !(testExpectations.IsTodoPath || testExpectations.NeedsTodoFeatures) {
				*testFiles = append(*testFiles, TestFile{
					Path:         path,
					Expectations: testExpectations,
				})
			}
		}
		return nil
	})
	if err != nil {
		log.Fatal(err)
	}
}

type TestTodo struct {
	TodoFeatures [][]byte
	TodoPaths    []string
}

type TestFile struct {
	Path         string
	Expectations TestExpectations
}

type TestExpectations struct {
	EarlyError        bool
	IsTest            bool
	IsTodoPath        bool
	NeedsTodoFeatures bool
}

func ReadTestExpectations(testTodo TestTodo, path string) TestExpectations {
	source, err := ioutil.ReadFile(path)
	if err != nil {
		log.Fatal(err)
	}
	return ParseTestExpectations(testTodo, source, path)
}

func ParseTestExpectations(testTodo TestTodo, source []byte, path string) TestExpectations {
	return TestExpectations{
		EarlyError:        bytes.Contains(source, []byte("phase: parse")),
		IsTest:            bytes.Contains(source, []byte("/*---")),
		IsTodoPath:        pathMatchesAnyPattern(path, testTodo.TodoPaths),
		NeedsTodoFeatures: testSourceRequiresFeatures(source, testTodo.TodoFeatures),
	}
}

func pathMatchesAnyPattern(path string, patterns []string) bool {
	for _, pattern := range patterns {
		if MatchPath(pattern, path) {
			return true
		}
	}
	return false
}

func testSourceRequiresFeatures(source []byte, features [][]byte) bool {
	for _, feature := range features {
		if bytes.Contains(source, feature) {
			return true
		}
	}
	return false
}

func RunQuickLintJS(quickLintJSExecutable string, jsFile string, ignoreWarnings bool) LintResult {
	command := []string{quickLintJSExecutable}
	if ignoreWarnings {
		command = append(command, "--exit-fail-on=-E002,-E003,-E057,-E058,-E059")
	}
	command = append(command, "--", jsFile)

	process := exec.Command(command[0], command[1:]...)
	var output bytes.Buffer
	process.Stdout = &output
	process.Stderr = &output
	if err := process.Start(); err != nil {
		log.Fatal(err)
	}
	// TODO(strager): Time out after 10 seconds.
	err := process.Wait()
	var exitStatus *exec.ExitError
	ok := false
	if err == nil {
		exitStatus = nil
	} else if exitStatus, ok = err.(*exec.ExitError); ok {
	} else {
		log.Fatal(err)
	}
	return LintResult{
		command:    command,
		exitStatus: exitStatus,
		jsFile:     jsFile,
		output:     output,
	}
}

type LintResult struct {
	command    []string
	exitStatus *exec.ExitError
	jsFile     string
	output     bytes.Buffer
}

func (result *LintResult) Crashed() bool {
	return !(result.exitStatus == nil || result.exitStatus.ExitCode() == 1)
}

func (result *LintResult) ExitedWithCode(exitCode int) bool {
	if result.exitStatus == nil {
		return exitCode == 0
	} else {
		return result.exitStatus.ExitCode() == exitCode
	}
}

func (result *LintResult) UserRunnableCommand() string {
	// TODO(strager): Escape components.
	return strings.Join(result.command, " ")
}

func (result *LintResult) Dump(out *os.File) {
	var failureDescription string
	if result.Crashed() {
		failureDescription = "command crashed"
	} else {
		failureDescription = "test failed"
	}
	_, _ = out.WriteString(fmt.Sprintf(
		"error: %s: %s\n",
		failureDescription,
		result.UserRunnableCommand(),
	))
	_, _ = out.Write(result.output.Bytes())
	_, _ = out.WriteString(fmt.Sprintf("\nContents of %s:\n", result.jsFile))
	file, err := os.Open(result.jsFile)
	if err != nil {
		log.Fatal(err)
	}
	if _, err = io.Copy(out, file); err != nil {
		log.Fatal(err)
	}
}

func MatchPath(pattern string, path string) bool {
	patternParts := SplitPathComponents(pattern)
	pathParts := SplitPathComponents(path)
	if len(patternParts) > len(pathParts) {
		return false
	}
	pathParts = pathParts[len(pathParts)-len(patternParts):]

	for i := 0; i < len(patternParts); i++ {
		matched, err := filepath.Match(patternParts[i], pathParts[i])
		if err != nil {
			log.Fatalf("error: invalid pattern: %#v: %v", pattern, err)
		}
		if !matched {
			return false
		}
	}
	return true
}

func SplitPathComponents(path string) []string {
	var components []string
	for path != "" {
		parent, file := filepath.Split(path)
		components = append(components, file)
		path = strings.TrimRight(parent, "\\/")
	}
	ReverseStringSlice(components)
	return components
}

func ReverseStringSlice(items []string) {
	middle := len(items) / 2
	for i := 0; i < middle; i++ {
		j := len(items) - i - 1
		items[i], items[j] = items[j], items[i]
	}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
