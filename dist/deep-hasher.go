// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "archive/tar"
import "archive/zip"
import "bytes"
import "compress/gzip"
import "crypto/sha256"
import "errors"
import "fmt"
import "io"
import "io/fs"
import "os"
import "path/filepath"

type DeepHasher struct {
	Hashes map[DeepPath]SHA256Hash
}

type SHA256Hash [32]byte

func NewDeepHasher() DeepHasher {
	return DeepHasher{
		Hashes: make(map[DeepPath]SHA256Hash),
	}
}

func (deepHasher *DeepHasher) DeepHashTarGz(path DeepPath, tarGzFile io.Reader) error {
	tarFile, err := gzip.NewReader(tarGzFile)
	if err != nil {
		return err
	}
	tarReader := tar.NewReader(tarFile)
	for {
		fileHeader, err := tarReader.Next()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return err
		}

		fileContent, err := io.ReadAll(tarReader)
		if err != nil {
			return err
		}

		randomAccessFile := bytes.NewReader(fileContent)
		if err := deepHasher.DeepHashFile(path.Append(fileHeader.Name), randomAccessFile); err != nil {
			return err
		}
	}
	return nil
}

func (deepHasher *DeepHasher) DeepHashZIP(path DeepPath, zipFile io.ReaderAt, zipSize int64) error {
	zipReader, err := zip.NewReader(zipFile, zipSize)
	if err != nil {
		return err
	}
	doMember := func(fileHeader *zip.File) error {
		if fileHeader.FileInfo().IsDir() {
			return nil
		}

		file, err := zipReader.Open(fileHeader.Name)
		if err != nil {
			return err
		}
		defer file.Close()
		fileContent, err := io.ReadAll(file)
		if err != nil {
			return err
		}
		randomAccessFile := bytes.NewReader(fileContent)
		if err := deepHasher.DeepHashFile(path.Append(fileHeader.Name), randomAccessFile); err != nil {
			return err
		}
		return nil
	}
	for _, fileHeader := range zipReader.File {
		if err := doMember(fileHeader); err != nil {
			return err
		}
	}
	return nil
}

func (deepHasher *DeepHasher) DeepHashFile(path DeepPath, file RandomAccessReader) error {
	hasher := sha256.New()
	if _, err := io.Copy(hasher, file); err != nil {
		return err
	}
	hashSlice := hasher.Sum(nil)
	var hash SHA256Hash
	copy(hash[:], hashSlice)
	deepHasher.Hashes[path] = hash
	fileSize, err := file.Seek(0, io.SeekCurrent)
	if err != nil {
		return err
	}
	if _, err := file.Seek(0, io.SeekStart); err != nil {
		return err
	}

	if PathLooksLikeTarGz(path.Last()) {
		if err := deepHasher.DeepHashTarGz(path, file); err != nil {
			return err
		}
	} else if PathLooksLikeZip(path.Last()) {
		if err := deepHasher.DeepHashZIP(path, file, fileSize); err != nil {
			return err
		}
	}
	return nil
}

func (deepHasher *DeepHasher) DeepHashDirectory(dir string) error {
	return filepath.Walk(dir, func(path string, info fs.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		relativePath, err := filepath.Rel(dir, path)
		if err != nil {
			return err
		}
		if !info.Mode().IsRegular() {
			return fmt.Errorf("expected regular file: %q", path)
		}

		file, err := os.Open(path)
		if err != nil {
			return err
		}
		defer file.Close()
		if err := deepHasher.DeepHashFile(NewDeepPath(relativePath), file); err != nil {
			return err
		}

		return nil
	})
}

type RandomAccessReader interface {
	io.Reader
	io.ReaderAt
	io.Seeker
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
