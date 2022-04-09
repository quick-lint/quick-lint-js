// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "path/filepath"
import "sync"
import "path"
import "io"
import "os/exec"
import "bytes"
import "errors"
import "flag"
import "fmt"
import "log"
import "os"

var CollectBinarySizesPath string

func main() {
	var err error

	flag.Parse()
	if flag.NArg() < 1 {
		fmt.Fprintf(os.Stderr, "error: missing builds directory\n")
		os.Exit(2)
	}
	if flag.NArg() < 2 {
		fmt.Fprintf(os.Stderr, "error: missing build-sizes directory\n")
		os.Exit(2)
	}
	buildsDirectoryPath := flag.Arg(0)
	buildSizesDirectoryPath := flag.Arg(1)
	workerCount := 8

	CollectBinarySizesPath, err = filepath.Abs("./tools/build-sizes/collect_binary_sizes.py")
	if err != nil {
		log.Fatal(err)
	}

	pathsChannel := make(chan string, 1024)

	var workersFinished sync.WaitGroup

	for i := 0; i < workerCount; i++ {
		workersFinished.Add(1)
		go func() {
			defer workersFinished.Done()
			RunWorker(buildSizesDirectoryPath, pathsChannel)
		}()
	}

	if err := FindBuildDirectories(buildsDirectoryPath, pathsChannel); err != nil {
		log.Fatal(err)
	}
	close(pathsChannel)

	workersFinished.Wait()
}

func RunWorker(buildSizesDirectoryPath string, pathsChannel chan string) {
	for buildPath := range pathsChannel {
		err := CollectBuildSizeIfNeeded(buildSizesDirectoryPath, buildPath)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error processing %s: %v\n", buildPath, err)
			// Continue.
		}
	}
}

func CollectBuildSizeIfNeeded(buildSizesDirectoryPath string, buildPath string) error {
	// TODO(strager): For performance, scan the
	// build-sizes directory rather than statting each
	// build-sizes/*.json file separately.
	buildSizesJSONFilePath := path.Join(buildSizesDirectoryPath, path.Base(buildPath)+".json")
	buildSizesJSONFileStat, err := os.Stat(buildSizesJSONFilePath)
	if err == nil {
		if buildSizesJSONFileStat.Size() != 0 {
			// Build already processed. Skip.
			return nil
		}
	} else if errors.Is(err, os.ErrNotExist) {
		// Continue.
	} else {
		return err
	}

	fmt.Printf("Processing %s ...\n", buildPath)

	process := exec.Command(CollectBinarySizesPath, ".")
	process.Dir = buildPath
	var buildSizesJSON bytes.Buffer
	process.Stdout = &buildSizesJSON
	process.Stderr = os.Stderr
	if err := process.Start(); err != nil {
		return err
	}
	if err := process.Wait(); err != nil {
		return err
	}
	buildSizesJSONSize := int64(buildSizesJSON.Len())

	buildSizesJSONFile, err := os.Create(buildSizesJSONFilePath)
	if err != nil {
		return err
	}
	defer buildSizesJSONFile.Close()

	bytesWritten, err := io.Copy(buildSizesJSONFile, &buildSizesJSON)
	if err != nil {
		return err
	}
	if bytesWritten != buildSizesJSONSize {
		return fmt.Errorf("failed to write entire JSON file: %s", buildSizesJSONFilePath)
	}

	return nil
}

func FindBuildDirectories(buildsDirectoryPath string, outputPaths chan string) error {
	buildsDirectory, err := os.Open(buildsDirectoryPath)
	if err != nil {
		return err
	}
	defer buildsDirectory.Close()

	for {
		names, err := buildsDirectory.Readdirnames(64)
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			return err
		}
		for _, name := range names {
			outputPaths <- path.Join(buildsDirectoryPath, name)
		}
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
