// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

var TodoTestFiles []string = []string{
	"language/asi/*.js",

	// TODO(#50): Parse --> comments.
	"annexB/language/comments/multi-line-html-close.js",
	"annexB/language/comments/single-line-html-close.js",

	// TODO(strager): Implement non-standard and new features.
	"language/*/class/*-coalesce.js",
	"language/expressions/assignmenttargettype/*-coalesce-*.js",
	"language/expressions/assignmenttargettype/*-logical-*-assignment-*.js",

	// TODO(#153): Parse V8 %BuiltInFunctions
	"v8/mjsunit/*.js",
	"v8/mjsunit/*/*.js",
	"v8/mjsunit/*/*/*.js",
	"v8/test262/detachArrayBuffer.js",
}

var TodoTestFeatures [][]byte = [][]byte{
	// TODO(strager): Implement non-standard and new features.
	[]byte("async-iteration"),
	[]byte("class-fields-private"),
	[]byte("class-fields-public"),
	[]byte("class-methods-private"),
	[]byte("class-static-fields-private"),
	[]byte("class-static-fields-public"),
	[]byte("class-static-methods-private"),
	[]byte("coalesce-expression"),
	[]byte("import.meta"),
	[]byte("logical-assignment-operators"),
	[]byte("numeric-separator-literal"),
	[]byte("optional-catch-binding"),
	[]byte("optional-chaining"),
	[]byte("top-level-await"),
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

	var testFiles []string
	for _, testDirectory := range flag.Args() {
		FindTests(testDirectory, &testFiles)
	}
	sort.Strings(testFiles)

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
	testFiles             []string
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

func MakeWorkQueue(quickLintJSExecutable string, testFiles []string, threadCount int, stopOnFirstFailure bool) *WorkQueue {
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
		result := RunQuickLintJS(queue.quickLintJSExecutable, queue.testFiles[i])
		if result.Crashed() {
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

func FindTests(test262FixtureDirectory string, testFiles *[]string) {
	err := filepath.Walk(test262FixtureDirectory, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(path, ".js") && ShouldCheckTestFile(path) {
			*testFiles = append(*testFiles, path)
		}
		return nil
	})
	if err != nil {
		log.Fatal(err)
	}
}

func ShouldCheckTestFile(testFile string) bool {
	return !IsTodo(testFile) && !TestRequiresUnimplementedFeatures(testFile)
}

func IsTodo(path string) bool {
	for _, pattern := range TodoTestFiles {
		if MatchPath(pattern, path) {
			return true
		}
	}
	return false
}

func TestRequiresUnimplementedFeatures(path string) bool {
	testContent, err := ioutil.ReadFile(path)
	if err != nil {
		log.Fatal(err)
	}
	for _, testFeature := range TodoTestFeatures {
		if bytes.Contains(testContent, testFeature) {
			return true
		}
	}
	return false
}

func RunQuickLintJS(quickLintJSExecutable string, jsFile string) LintResult {
	command := []string{quickLintJSExecutable, "--", jsFile}
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

func (result *LintResult) UserRunnableCommand() string {
	// TODO(strager): Escape components.
	return strings.Join(result.command, " ")
}

func (result *LintResult) Dump(out *os.File) {
	_, _ = out.WriteString(fmt.Sprintf(
		"error: command crashed: %s\n",
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
