// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// quick-lint-js uses a custom format to store translations. Originally,
// quick-lint-js used GNU gettext's MO format, but it had a number of issues:
//
// * Storing multiple locale requires storing source strings one per
//   locale. This increases file size.
// * The hash function is weak, resulting in many hash collisions.
// * The hash function is imperfect, forcing lookup to compare a matching source
//   string in full for every translation.
// * MO files can be little endian or big endian, complicating lookup.
//
// Our custom format addresses these issues:
//
// * All locales are merged into a single data structure.
// * Hash table lookups happen at compile time, never at run time.
// * The data is always native endian.
//
// The custom format has four parts:
// * the compile-time hash table
// * the mapping table
// * the string table
// * the locale table
// * some constants
//
// These are the constants (C++ code):
//
//     std::size_t const_hash_table_size = /* ... */;
//     std::uint64_t const_hash_offset_basis = /* ... */;
//
//     std::uint32_t locale_count = /* ... */;
//     std::uint16_t mapping_table_size = /* ... */;
//     std::size_t strings_size = /* ... */;
//     std::size_t locale_table_size = /* ... */;
//
// The hash table is compile-time-only. It looks like this (C++ code):
//
//     struct const_hash_entry {
//       std::uint16_t mapping_table_index;
//       std::string_view untranslated;
//     };
//     const_hash_entry const_hash_table[const_hash_table_size];
//
// To generate the index in const_hash_table, use the 64-bit Fowler–Noll–Vo
// FNV-1a hashing algorithm [1] on the UTF-8 untranslated string (excluding any
// null terminators) with the offset_basis parameter set to hash_offset_basis.
// Then, mod the hash by const_hash_table_size.
//
// In the case of a hash collision, use quadratic probing: generate the next
// index by adding the square of the attempt number (mod const_hash_table_size).
//
// The mapping table and the locale table are run-time-only. They look like this
// (C++ code):
//
//     struct mapping_entry {
//       std::uint32_t string_offsets[locale_count];
//     };
//     mapping_entry mapping_table[mapping_table_size];
//
//     char locale_table[locale_table_size] =
//       "en_US\0"
//       "de_DE\0"
//       /* ... */
//       "";  // C++ adds an extra null byte for us.
//
// hash_entry::string_offsets[i] corresponds to the i-th locale listed in
// locale_table.
//
// Entry 0 of the mapping table is unused.
//
// The string table contains 0-terminated UTF-8 strings. String sizes can be
// computed by calculating the difference between the first 0 byte starting at
// the string offset and the string offset.
//
// [1] https://datatracker.ietf.org/doc/html/draft-eastlake-fnv-17.html

package main

import "encoding/binary"
import "bufio"
import "bytes"
import "fmt"
import "io/ioutil"
import "log"
import "os"
import "os/exec"
import "path/filepath"
import "sort"
import "strings"

const maxHashCollisions int = 4

func main() {
	poFiles, err := ListPOFiles()
	if err != nil {
		log.Fatal(err)
	}

	locales := map[string][]TranslationEntry{}
	for _, poFilePath := range poFiles {
		gmo, err := POFileToGMO(poFilePath)
		if err != nil {
			log.Fatal(err)
		}
		locales[POPathToLocaleName(poFilePath)] = ExtractGMOStrings(gmo)
	}

	table := CreateTranslationTable(locales)
	if err := WriteTranslationTableHeader(&table, "src/quick-lint-js/translation-table-generated.h"); err != nil {
		log.Fatal(err)
	}
	if err := WriteTranslationTableSource(&table, "src/translation-table-generated.cpp"); err != nil {
		log.Fatal(err)
	}
}

func ListPOFiles() ([]string, error) {
	poDirectory := "po"
	filesInPODirectory, err := ioutil.ReadDir(poDirectory)
	if err != nil {
		return nil, err
	}
	var poFiles []string
	for _, fileInfo := range filesInPODirectory {
		if filepath.Ext(fileInfo.Name()) == ".po" {
			poFiles = append(poFiles, filepath.Join(poDirectory, fileInfo.Name()))
		}
	}
	// Sort locales to make builds reproducible.
	sort.Strings(poFiles)
	return poFiles, nil
}

func POPathToLocaleName(path string) string {
	return strings.TrimSuffix(filepath.Base(path), filepath.Ext(path))
}

func POFileToGMO(poFilePath string) ([]byte, error) {
	process := exec.Command(
		"msgfmt",
		"--output-file=-",
		"--",
		poFilePath,
	)
	var gmo bytes.Buffer
	process.Stdout = &gmo
	process.Stderr = os.Stderr
	if err := process.Start(); err != nil {
		return nil, err
	}
	if err := process.Wait(); err != nil {
		return nil, err
	}
	return gmo.Bytes(), nil
}

type TranslationEntry struct {
	Untranslated []byte
	Translated   []byte
}

func (entry *TranslationEntry) IsMetadata() bool {
	return len(entry.Untranslated) == 0
}

func ExtractGMOStrings(gmoData []byte) []TranslationEntry {
	var magic uint32 = binary.LittleEndian.Uint32(gmoData[0:])
	var decode binary.ByteOrder
	if magic == 0x950412de {
		decode = binary.LittleEndian
	} else {
		decode = binary.BigEndian
	}

	stringAt := func(tableOffset uint32, index uint32) []byte {
		tableEntryOffset := tableOffset + index*8
		length := decode.Uint32(gmoData[tableEntryOffset+0:])
		offset := decode.Uint32(gmoData[tableEntryOffset+4:])
		return gmoData[offset : offset+length]
	}

	entries := []TranslationEntry{}
	stringCount := decode.Uint32(gmoData[8:])
	originalTableOffset := decode.Uint32(gmoData[12:])
	translatedTableOffset := decode.Uint32(gmoData[16:])
	for i := uint32(0); i < stringCount; i += 1 {
		entries = append(entries, TranslationEntry{
			Untranslated: stringAt(originalTableOffset, i),
			Translated:   stringAt(translatedTableOffset, i),
		})
	}
	return entries
}

// Return value is sorted with no duplicates.
func GetLocaleNames(locales map[string][]TranslationEntry) []string {
	localeNames := []string{}
	for localeName, _ := range locales {
		localeNames = append(localeNames, localeName)
	}
	// Sort to make output deterministic.
	sort.Strings(localeNames)
	return localeNames
}

// Extracts .Untranslated from each TranslationEntry.
//
// Return value is sorted with no duplicates.
func GetAllUntranslated(locales map[string][]TranslationEntry) [][]byte {
	allUntranslated := [][]byte{}
	addUntranslated := func(untranslated []byte) {
		for _, existingUntranslated := range allUntranslated {
			foundDuplicate := bytes.Equal(existingUntranslated, untranslated)
			if foundDuplicate {
				return
			}
		}
		allUntranslated = append(allUntranslated, untranslated)
	}
	for _, localeTranslations := range locales {
		for _, translation := range localeTranslations {
			if !translation.IsMetadata() {
				addUntranslated(translation.Untranslated)
			}
		}
	}
	// Sort to make output deterministic.
	sort.Slice(allUntranslated, func(i int, j int) bool {
		return bytes.Compare(allUntranslated[i], allUntranslated[j]) < 0
	})
	return allUntranslated
}

type TranslationTable struct {
	ConstHashTable       []TranslationTableConstHashEntry
	ConstHashOffsetBasis uint64
	MappingTable         []TranslationTableMappingEntry
	StringTable          []byte
	Locales              []string
	LocaleTable          []byte
}

type TranslationTableConstHashEntry struct {
	MappingTableIndex uint16
	Untranslated      []byte
}

type TranslationTableMappingEntry struct {
	// Key: index of locale in TranslationTable.Locales
	// Value: offset in TranslationTable.StringTable
	StringOffsets []uint32
}

func CreateTranslationTable(locales map[string][]TranslationEntry) TranslationTable {
	table := TranslationTable{}

	addStringToTable := func(stringToAdd []byte, outTable *[]byte) uint32 {
		offset := uint32(len(*outTable))
		*outTable = append(*outTable, stringToAdd...)
		*outTable = append(*outTable, 0)
		return offset
	}

	addString := func(stringToAdd []byte) uint32 {
		return addStringToTable(stringToAdd, &table.StringTable)
	}

	keys := GetAllUntranslated(locales)
	table.Locales = GetLocaleNames(locales)

	for _, localeName := range table.Locales {
		addStringToTable([]byte(localeName), &table.LocaleTable)
	}
	// Add a null byte (i.e. an empty locale) to terminate the list.
	addStringToTable([]byte{}, &table.LocaleTable)

	// HACK(strager): len(keys) is all we need in theory, but we have too many
	// collisions if we make the table that small.
	hashTableSize := len(keys)*3/2 + 1
	if hashTableSize == 0 {
		hashTableSize = 1
	}
	table.ConstHashTable = make([]TranslationTableConstHashEntry, hashTableSize)
	constHashTableEntryUsed := make([]bool, hashTableSize)
	var initialConstHashOffsetBasis uint64 = 0xcbf29ce484222325 // Arbitrary.
	table.ConstHashOffsetBasis = initialConstHashOffsetBasis

retry:
	for i, _ := range table.ConstHashTable {
		table.ConstHashTable[i].MappingTableIndex = 0
		table.ConstHashTable[i].Untranslated = nil
		constHashTableEntryUsed[i] = false
	}

	for keyIndex, key := range keys {
		var entryIndex uint16
		for attempt := 0; attempt <= maxHashCollisions; attempt += 1 {
			entryIndex = table.ConstIndexByUntranslated(key, attempt)
			if !constHashTableEntryUsed[entryIndex] {
				goto foundEntry
			}
		}
		{
			// Too many hash collision occurred.
			attempt := table.ConstHashOffsetBasis - initialConstHashOffsetBasis
			if attempt >= 1000000 {
				log.Fatalf("couldn't find a perfect offset basis after many tries")
			}
			if attempt%10000 == 0 {
				fmt.Printf("%d attempts ...\n", attempt)
			}
			table.ConstHashOffsetBasis += 1
			goto retry
		}

	foundEntry:
		table.ConstHashTable[entryIndex].MappingTableIndex = uint16(keyIndex + 1)
		table.ConstHashTable[entryIndex].Untranslated = key
		constHashTableEntryUsed[entryIndex] = true
	}

	table.StringTable = []byte{0}
	mappingTableSize := len(keys) + 1
	table.MappingTable = make([]TranslationTableMappingEntry, mappingTableSize)
	for i := 0; i < mappingTableSize; i += 1 {
		mappingEntry := &table.MappingTable[i]
		mappingEntry.StringOffsets = make([]uint32, len(locales))
	}
	for localeIndex, localeName := range table.Locales {
		localeTranslations := locales[localeName]
		for _, translation := range localeTranslations {
			if !translation.IsMetadata() {
				constHashEntry := table.FindConstHashEntryByUntranslated(translation.Untranslated)
				table.MappingTable[constHashEntry.MappingTableIndex].StringOffsets[localeIndex] = addString(translation.Translated)
			}
		}
	}

	return table
}

// Returns an index into table.ConstHashTable.
func (table *TranslationTable) ConstIndexByUntranslated(originalString []byte, attempt int) uint16 {
	hash := HashFNV1a64WithOffsetBasis(originalString, table.ConstHashOffsetBasis)
	return uint16((hash + uint64(attempt*attempt)) % uint64(len(table.ConstHashTable)))
}

// Returns an index into table.ConstHashTable. If the entry matching
// originalString does not exist, returns nil.
func (table *TranslationTable) FindConstHashEntryByUntranslated(originalString []byte) *TranslationTableConstHashEntry {
	for attempt := 0; attempt <= maxHashCollisions; attempt += 1 {
		hashIndex := table.ConstIndexByUntranslated(originalString, attempt)
		entry := &table.ConstHashTable[hashIndex]
		if bytes.Equal(entry.Untranslated, originalString) {
			return entry
		}
	}
	return nil
}

func (table *TranslationTable) LookUpMappingByUntranslated(originalString []byte) *TranslationTableMappingEntry {
	entry := table.FindConstHashEntryByUntranslated(originalString)
	if entry == nil {
		return nil
	}
	return &table.MappingTable[entry.MappingTableIndex]
}

func (table *TranslationTable) ReadString(stringOffset uint32) []byte {
	stringData := table.StringTable[stringOffset:]
	stringLength := bytes.IndexByte(stringData, 0)
	return stringData[:stringLength]
}

func WriteTranslationTableHeader(table *TranslationTable, path string) error {
	outputFile, err := os.Create(path)
	if err != nil {
		log.Fatal(err)
	}
	defer outputFile.Close()
	writer := bufio.NewWriter(outputFile)

	writer.WriteString(
		`// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_TRANSLATION_TABLE_GENERATED_H
#define QUICK_LINT_JS_TRANSLATION_TABLE_GENERATED_H

// This file is **GENERATED** by tools/compile-translations.go.

#include <cstddef>
#include <cstdint>
#include <iterator>
#include <quick-lint-js/consteval.h>
#include <quick-lint-js/hash-fnv.h>
#include <quick-lint-js/translation-table.h>
#include <string_view>

namespace quick_lint_js {
using namespace std::literals::string_view_literals;

`)
	fmt.Fprintf(writer, "constexpr std::uint32_t translation_table_locale_count = %d;\n", len(table.Locales))
	fmt.Fprintf(writer, "constexpr std::uint16_t translation_table_mapping_table_size = %d;\n", len(table.MappingTable))
	fmt.Fprintf(writer, "constexpr std::size_t translation_table_string_table_size = %d;\n", len(table.StringTable))
	fmt.Fprintf(writer, "constexpr std::size_t translation_table_locale_table_size = %d;\n", len(table.LocaleTable))
	fmt.Fprintf(writer, "\n")

	writer.WriteString(
		`QLJS_CONSTEVAL std::uint16_t translation_table_const_hash_table_look_up(
    std::string_view untranslated, std::uint16_t default_result) {
  struct const_hash_entry {
    std::uint16_t mapping_table_index;
    const char* untranslated;
  };

  // clang-format off
  constexpr const_hash_entry const_hash_table[] = {
`)
	for _, constHashEntry := range table.ConstHashTable {
		fmt.Fprintf(writer, "          {%d, \"", constHashEntry.MappingTableIndex)
		DumpStringLiteralBody(string(constHashEntry.Untranslated), writer)
		writer.WriteString("\"},\n")
	}
	fmt.Fprintf(writer,
		`  };
  // clang-format on

  std::uint64_t hash = hash_fnv_1a_64(untranslated, %dULL);
  std::uint64_t table_size = %d;
  for (std::uint64_t attempt = 0; attempt <= %d; ++attempt) {
    const const_hash_entry& hash_entry = const_hash_table[(hash + attempt*attempt) %% table_size];
    if (hash_entry.mapping_table_index == 0) {
      break;
    }
    if (hash_entry.untranslated == untranslated) {
      return hash_entry.mapping_table_index;
    }
  }
  return default_result;
}
}

#endif

`, table.ConstHashOffsetBasis, len(table.ConstHashTable), maxHashCollisions)
	WriteCopyrightFooter(writer)

	if err := writer.Flush(); err != nil {
		log.Fatal(err)
	}
	return nil
}

func WriteTranslationTableSource(table *TranslationTable, path string) error {
	outputFile, err := os.Create(path)
	if err != nil {
		log.Fatal(err)
	}
	defer outputFile.Close()
	writer := bufio.NewWriter(outputFile)

	writer.WriteString(
		`// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// This file is **GENERATED** by tools/compile-translations.go.

#include <quick-lint-js/translation-table.h>

namespace quick_lint_js {
const translation_table translation_data = {
    .mapping_table =
        {
`)
	for _, mappingEntry := range table.MappingTable {
		writer.WriteString("            {")
		for i, stringOffset := range mappingEntry.StringOffsets {
			if i != 0 {
				writer.WriteString(", ")
			}
			fmt.Fprintf(writer, "%d", stringOffset)
		}
		writer.WriteString("},\n")
	}
	writer.WriteString(
		`        },

    // clang-format off
    .string_table =
`)
	DumpStringTable(table.StringTable, "        u8", writer)

	writer.WriteString(
		`,
    // clang-format on

    .locale_table =
`)

	DumpStringTable(table.LocaleTable, "        ", writer)

	writer.WriteString(
		`,
};
}

`)
	WriteCopyrightFooter(writer)

	if err := writer.Flush(); err != nil {
		log.Fatal(err)
	}
	return nil
}

func WriteCopyrightFooter(writer *bufio.Writer) {
	writer.WriteString(
		`// quick-lint-js finds bugs in JavaScript programs.
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
`)
}

func DumpStringTable(strings []byte, linePrefix string, writer *bufio.Writer) {
	for len(strings) != 0 {
		writer.WriteString(linePrefix)
		writer.WriteRune('"')
		stringLength := bytes.IndexByte(strings, 0)
		s := string(strings[:stringLength])
		DumpStringLiteralBody(s, writer)
		strings = strings[stringLength+1:]
		if len(strings) != 0 {
			// C++ adds a \0 for us, so we don't need to add one ourselves.
			writer.WriteString(`\0`)
		}
		writer.WriteRune('"')
		if len(strings) != 0 {
			writer.WriteRune('\n')
		}
	}
}

func DumpStringLiteralBody(s string, writer *bufio.Writer) {
	for _, c := range s {
		if c < 0x20 || c >= 0x7f {
			fmt.Fprintf(writer, `\u%04x`, c)
		} else if c == '\\' || c == '"' {
			writer.WriteRune('\\')
			writer.WriteRune(c)
		} else {
			writer.WriteRune(c)
		}
	}
}

func HashFNV1a64(data []byte) uint64 {
	return HashFNV1a64WithOffsetBasis(data, 0xcbf29ce484222325)
}

func HashFNV1a64WithOffsetBasis(data []byte, offsetBasis uint64) uint64 {
	var hash uint64 = offsetBasis
	for _, b := range data {
		hash ^= uint64(b)
		hash *= 0x00000100_000001b3
	}
	return hash
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
