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
// * The hash function is perfect (i.e. no collisions), speeding up lookups.
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
// The mapping table and the locale table are run-time-only. They look like this
// (C++ code):
//
//     struct mapping_entry {
//       std::uint32_t string_offsets[locale_count + 1];
//     };
//     mapping_entry mapping_table[mapping_table_size];
//
//     char locale_table[locale_table_size] =
//       "en_US\0"
//       "de_DE\0"
//       /* ... */
//       "\0";
//
// hash_entry::string_offsets[i] corresponds to the i-th locale listed in
// locale_table.
//
// hash_entry::string_offsets[locale_count] refers to the original
// (untranslated) string.
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

	isMetadata := func(entry *TranslationEntry) bool {
		return len(entry.Untranslated) == 0
	}

	addStringToTable := func(stringToAdd []byte, outTable *[]byte) uint32 {
		offset := uint32(len(*outTable))
		*outTable = append(*outTable, stringToAdd...)
		*outTable = append(*outTable, 0)
		return offset
	}

	addString := func(stringToAdd []byte) uint32 {
		return addStringToTable(stringToAdd, &table.StringTable)
	}

	var keys [][]byte
	addKey := func(key []byte) {
		for _, existingKey := range keys {
			foundDuplicate := bytes.Equal(existingKey, key)
			if foundDuplicate {
				return
			}
		}
		keys = append(keys, key)
	}
	for localeName, localeTranslations := range locales {
		table.Locales = append(table.Locales, localeName)
		for _, translation := range localeTranslations {
			if !isMetadata(&translation) {
				addKey(translation.Untranslated)
			}
		}
	}
	// Sort to make output deterministic.
	sort.Strings(table.Locales)
	sort.Slice(keys, func(i int, j int) bool {
		return bytes.Compare(keys[i], keys[j]) < 0
	})

	for _, localeName := range table.Locales {
		addStringToTable([]byte(localeName), &table.LocaleTable)
	}

	// HACK(strager): len(keys) is all we need in theory, but we have too many
	// collisions if we make the table that small.
	hashTableSize := len(keys)*10 + 1
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
		entryIndex := table.ConstIndexByUntranslated(key)
		if constHashTableEntryUsed[entryIndex] {
			// Hash collision occurred.
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
			if !isMetadata(&translation) {
				constHashEntry := &table.ConstHashTable[table.ConstIndexByUntranslated(translation.Untranslated)]
				table.MappingTable[constHashEntry.MappingTableIndex].StringOffsets[localeIndex] = addString(translation.Translated)
			}
		}
	}

	return table
}

// Returns an index into table.ConstHashTable.
func (table *TranslationTable) ConstIndexByUntranslated(originalString []byte) uint16 {
	hash := HashFNV1a64WithOffsetBasis(originalString, table.ConstHashOffsetBasis)
	return uint16(hash % uint64(len(table.ConstHashTable)))
}

func (table *TranslationTable) LookUpMappingByUntranslated(originalString []byte) *TranslationTableMappingEntry {
	hashIndex := table.ConstIndexByUntranslated(originalString)
	return &table.MappingTable[table.ConstHashTable[hashIndex].MappingTableIndex]
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
	fmt.Fprintf(writer, "constexpr std::uint32_t translation_table_locale_count = %d;\n", len(table.LocaleTable))
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
  const const_hash_entry& hash_entry = const_hash_table[hash %% %d];
  if (hash_entry.untranslated == untranslated) {
    return hash_entry.mapping_table_index;
  } else {
    return default_result;
  }
}
}

#endif

`, table.ConstHashOffsetBasis, len(table.ConstHashTable))
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
translation_table translation_data = {
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
