// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "bytes"
import "reflect"
import "testing"

func TestHashFNV(t *testing.T) {
	assertEqual := func(actual uint64, expected uint64) {
		if actual != expected {
			t.Errorf("expected %#v, but got %#v", expected, actual)
		}
	}

	t.Run("FNV1a default offset_basis", func(t *testing.T) {
		// https://datatracker.ietf.org/doc/html/draft-eastlake-fnv-17.html#page-26
		assertEqual(HashFNV1a64([]byte("")), 0xcbf29ce484222325)
		assertEqual(HashFNV1a64([]byte("\x00")), 0xaf63bd4c8601b7df)
		assertEqual(HashFNV1a64([]byte("a")), 0xaf63dc4c8601ec8c)
		assertEqual(HashFNV1a64([]byte("a\x00")), 0x089be207b544f1e4)
		assertEqual(HashFNV1a64([]byte("foobar")), 0x85944171f73967e8)
		assertEqual(HashFNV1a64([]byte("foobar\x00")), 0x34531ca7168b8f38)
		// https://docs.aws.amazon.com/redshift/latest/dg/r_FNV_HASH.html
		assertEqual(HashFNV1a64([]byte("Amazon Redshift")), 0x6c048340799c899e)
	})
}

func TestGMO(t *testing.T) {
	assertEqual := func(actual []TranslationEntry, expected []TranslationEntry) {
		if !reflect.DeepEqual(actual, expected) {
			t.Errorf("expected %#v, but got %#v", expected, actual)
		}
	}

	t.Run("empty .mo has no strings", func(t *testing.T) {
		gmoFileData := []byte{
			0xde, 0x12, 0x04, 0x95, // magic (little endian)
			0, 0, 0, 0, // revision
			0, 0, 0, 0, // number of strings
			0, 0, 0, 0, // original strings offset
			0, 0, 0, 0, // translated strings offset
			0, 0, 0, 0, // hash table size
			0, 0, 0, 0, // hash table offset
		}
		strings := ExtractGMOStrings(gmoFileData)
		assertEqual(strings, []TranslationEntry{})
	})

	t.Run("little endian .mo with one string", func(t *testing.T) {
		gmoFileData := []byte{
			0xde, 0x12, 0x04, 0x95, // 0x00: magic (little endian)

			0, 0, 0, 0, // 0x04: revision
			1, 0, 0, 0, // 0x08: number of strings
			0x1c, 0, 0, 0, // 0x0c: original strings offset
			0x24, 0, 0, 0, // 0x10: translated strings offset
			0, 0, 0, 0, // 0x14: hash table size
			0, 0, 0, 0, // 0x18: hash table offset

			2, 0, 0, 0, // 0x1c: original_strings[0].length
			0x2c, 0, 0, 0, // 0x20: original_strings[0].offset

			2, 0, 0, 0, // 0x24: translated_strings[0].length
			0x2f, 0, 0, 0, // 0x28: translated_strings[0].offset

			'h', 'i', 0, // 0x2c: original_strings[0] data
			'y', 'o', 0, // 0x2f: translated_strings[0] data
		}
		strings := ExtractGMOStrings(gmoFileData)
		assertEqual(strings, []TranslationEntry{
			TranslationEntry{[]byte("hi"), []byte("yo")},
		})
	})

	t.Run("big endian .mo with two strings", func(t *testing.T) {
		gmoFileData := []byte{

			0x95, 0x04, 0x12, 0xde, // 0x00: magic (big endian)

			0, 0, 0, 0, // 0x04: revision
			0, 0, 0, 2, // 0x08: number of strings
			0, 0, 0, 0x1c, // 0x0c: original strings offset
			0, 0, 0, 0x2c, // 0x10: translated strings offset
			0, 0, 0, 0, // 0x14: hash table size
			0, 0, 0, 0, // 0x18: hash table offset

			0, 0, 0, 5, // 0x1c: original_strings[0].length
			0, 0, 0, 0x3c, // 0x20: original_strings[0].offset
			0, 0, 0, 7, // 0x24: original_strings[1].length
			0, 0, 0, 0x42, // 0x28: original_strings[1].offset

			0, 0, 0, 7, // 0x2c: translated_strings[0].length
			0, 0, 0, 0x4a, // 0x30: translated_strings[0].offset
			0, 0, 0, 9, // 0x34: translated_strings[1].length
			0, 0, 0, 0x52, // 0x38: translated_strings[1].offset

			// 0x3c: original_strings[0] data
			'h', 'e', 'l', 'l', 'o', 0,
			// 0x42: original_strings[1] data
			'g', 'o', 'o', 'd', 'b', 'y', 'e', 0,
			// 0x4a: translated_strings[0] data
			'b', 'o', 'n', 'j', 'o', 'u', 'r', 0,
			// 0x52: translated_strings[1] data
			'a', 'u', ' ', 'r', 'e', 'v', 'o', 'i', 'r',
			0,
		}
		strings := ExtractGMOStrings(gmoFileData)
		assertEqual(strings, []TranslationEntry{
			TranslationEntry{[]byte("hello"), []byte("bonjour")},
			TranslationEntry{[]byte("goodbye"), []byte("au revoir")},
		})
	})
}

func TestCreateTranslationTable(t *testing.T) {
	checkTableIntegrity := func(t *testing.T, table *TranslationTable) {
		if len(table.ConstHashTable) == 0 {
			t.Errorf("hash table should never be empty")
		}
		if len(table.MappingTable) == 0 {
			t.Errorf("mapping table should never be empty")
		}
		for i, constHashEntry := range table.ConstHashTable {
			if int(constHashEntry.MappingTableIndex) >= len(table.MappingTable) {
				t.Errorf("hash table entry %d's MappingTableIndex should be between 0 and %d-1, but it is %d",
					i,
					len(table.MappingTable),
					constHashEntry.MappingTableIndex)
			}
			if len(constHashEntry.Untranslated) != 0 && constHashEntry.MappingTableIndex == 0 {
				t.Errorf("hash table entry %d has filled but MappingTableIndex is 0", i)
			}
		}
		for i, mappingEntry := range table.MappingTable {
			if len(mappingEntry.StringOffsets) != len(table.Locales) {
				t.Errorf("mapping table entry %d should have %d strings, but it has %d (%#v)",
					i,
					len(table.Locales),
					len(mappingEntry.StringOffsets),
					mappingEntry.StringOffsets)
			}

			for j, stringOffset := range mappingEntry.StringOffsets {
				stringData := table.StringTable[stringOffset:]
				if !bytes.Contains(stringData, []byte{0}) {
					t.Errorf("hash table entry %d locale %d points to string with no null terminator (%#v ...)",
						i,
						j,
						stringData)
				}
			}
		}
	}

	assertTableStringEquals := func(t *testing.T, table *TranslationTable, stringOffset uint32, expected string) {
		actualBytes := table.ReadString(stringOffset)
		if !reflect.DeepEqual(actualBytes, []byte(expected)) {
			t.Errorf("expected %#v, but got %#v", expected, actualBytes)
		}
	}

	t.Run("no locales or translation strings", func(t *testing.T) {
		table := CreateTranslationTable(map[string][]TranslationEntry{})
		checkTableIntegrity(t, &table)
		if len(table.Locales) != 0 {
			t.Errorf("expected no locales, but got %#v", table.Locales)
		}
	})

	t.Run("locales with no translation strings", func(t *testing.T) {
		table := CreateTranslationTable(map[string][]TranslationEntry{
			"en_US": []TranslationEntry{},
			"de_DE": []TranslationEntry{},
		})
		checkTableIntegrity(t, &table)
		expectedLocaleNames := []string{"de_DE", "en_US"}
		if !reflect.DeepEqual(table.Locales, expectedLocaleNames) {
			t.Errorf("expected %#v, but got %#v", expectedLocaleNames, table.Locales)
		}
		expectedLocaleTable := []byte("de_DE\x00en_US\x00")
		if !reflect.DeepEqual(table.LocaleTable, expectedLocaleTable) {
			t.Errorf("expected %#v, but got %#v", expectedLocaleTable, table.LocaleTable)
		}
	})

	t.Run("one locale with several translation strings", func(t *testing.T) {
		table := CreateTranslationTable(map[string][]TranslationEntry{
			"fr_FR": []TranslationEntry{
				TranslationEntry{[]byte("hello"), []byte("bonjour")},
				TranslationEntry{[]byte("goodbye"), []byte("au revoir")},
				TranslationEntry{[]byte("yes"), []byte("oui")},
				TranslationEntry{[]byte("no"), []byte("non")},
			},
		})
		checkTableIntegrity(t, &table)

		helloEntry := table.LookUpMappingByUntranslated([]byte("hello"))
		assertTableStringEquals(t, &table, helloEntry.StringOffsets[0], "bonjour")

		goodbyeEntry := table.LookUpMappingByUntranslated([]byte("goodbye"))
		assertTableStringEquals(t, &table, goodbyeEntry.StringOffsets[0], "au revoir")

		yesEntry := table.LookUpMappingByUntranslated([]byte("yes"))
		assertTableStringEquals(t, &table, yesEntry.StringOffsets[0], "oui")

		noEntry := table.LookUpMappingByUntranslated([]byte("no"))
		assertTableStringEquals(t, &table, noEntry.StringOffsets[0], "non")
	})

	t.Run("several locales with common translation strings", func(t *testing.T) {
		table := CreateTranslationTable(map[string][]TranslationEntry{
			"fr_FR": []TranslationEntry{
				TranslationEntry{[]byte("hello"), []byte("bonjour")},
			},
			"de_DE": []TranslationEntry{
				TranslationEntry{[]byte("hello"), []byte("hallo")},
			},
		})
		checkTableIntegrity(t, &table)

		helloEntry := table.LookUpMappingByUntranslated([]byte("hello"))
		assertTableStringEquals(t, &table, helloEntry.StringOffsets[0], "hallo")
		assertTableStringEquals(t, &table, helloEntry.StringOffsets[1], "bonjour")
	})

	t.Run("translation table excludes metadata", func(t *testing.T) {
		table := CreateTranslationTable(map[string][]TranslationEntry{
			"fr_FR": []TranslationEntry{
				TranslationEntry{[]byte(""), []byte("metadata goes here")},
			},
			"de_DE": []TranslationEntry{
				TranslationEntry{[]byte(""), []byte("(meta data)")},
			},
		})
		checkTableIntegrity(t, &table)

		for mappingIndex, mappingEntry := range table.MappingTable {
			for localeIndex, stringOffset := range mappingEntry.StringOffsets {
				s := table.ReadString(stringOffset)
				if bytes.Equal(s, []byte("metadata goes here")) || bytes.Equal(s, []byte("(meta data)")) {
					t.Errorf("mapping table entry %d locale %d should not have metadata string %#v",
						mappingIndex,
						localeIndex,
						s)
				}
			}
		}
	})
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
