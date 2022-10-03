// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "bytes"
import "reflect"
import "testing"

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
		if len(table.AbsoluteMappingTable) == 0 {
			t.Errorf("mapping table should never be empty")
		}
		for i, constLookupEntry := range table.ConstLookupTable {
			if len(constLookupEntry.Untranslated) == 0 {
				t.Errorf("const original table entry %d has empty Untranslated", i)
			}
		}
		if len(table.AbsoluteMappingTable) > 0 {
			nullMapping := table.AbsoluteMappingTable[0]
			for j, stringOffset := range nullMapping.StringOffsets {
				if table.StringTable[stringOffset] != 0x00 {
					t.Errorf("first mapping entry locale %d points to non-empty string", j)
				}
			}
		}
		for i, mappingEntry := range table.AbsoluteMappingTable {
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
					t.Errorf("mapping table entry %d locale %d points to string with no null terminator (%#v ...)",
						i,
						j,
						stringData)
				}
			}
		}

		if len(table.AbsoluteMappingTable) != len(table.RelativeMappingTable) {
			t.Errorf("relative mapping table has %d entries but absolute mapping table has %d entries",
				len(table.RelativeMappingTable),
				len(table.AbsoluteMappingTable))
		}
		lastPresentStringOffsets := make([]uint32, len(table.Locales))
		for i := 1; i < len(table.AbsoluteMappingTable); i += 1 {
			absoluteEntry := table.AbsoluteMappingTable[i]
			relativeEntry := table.RelativeMappingTable[i]
			for j, _ := range table.Locales {
				var expectedRelativeOffset uint32
				stringOffset := absoluteEntry.StringOffsets[j]
				if stringOffset == 0 {
					expectedRelativeOffset = 0
				} else {
					expectedRelativeOffset = stringOffset - lastPresentStringOffsets[j]
				}
				if relativeEntry.StringOffsets[j] != expectedRelativeOffset {
					t.Errorf("relative mapping table entry %d locale %d has relative offset %d but expected %d",
						i, j,
						expectedRelativeOffset,
						relativeEntry.StringOffsets[j])
				}
				if stringOffset != 0 {
					lastPresentStringOffsets[j] = stringOffset
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

	assertOffsetsEqual := func(t *testing.T, actual uint32, expected uint32) {
		if actual != expected {
			t.Errorf("expected %#v, but got %#v", expected, actual)
		}
	}

	t.Run("no locales or translation strings", func(t *testing.T) {
		table := CreateTranslationTable(map[string][]TranslationEntry{})
		checkTableIntegrity(t, &table)
		expectedLocaleNames := []string{""}
		if !reflect.DeepEqual(table.Locales, expectedLocaleNames) {
			t.Errorf("expected no locales, but got %#v", table.Locales)
		}
	})

	t.Run("locales with no translation strings", func(t *testing.T) {
		table := CreateTranslationTable(map[string][]TranslationEntry{
			"en_US": []TranslationEntry{},
			"de_DE": []TranslationEntry{},
		})
		checkTableIntegrity(t, &table)
		expectedLocaleNames := []string{"de_DE", "en_US", ""}
		if !reflect.DeepEqual(table.Locales, expectedLocaleNames) {
			t.Errorf("expected %#v, but got %#v", expectedLocaleNames, table.Locales)
		}
		expectedLocaleTable := []byte("de_DE\x00en_US\x00\x00")
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

		for mappingIndex, mappingEntry := range table.AbsoluteMappingTable {
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

	t.Run("mapping entries are relative", func(t *testing.T) {
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

	t.Run("relative mappings are 0 for missing strings", func(t *testing.T) {
		table := CreateTranslationTable(map[string][]TranslationEntry{
			"": []TranslationEntry{
				TranslationEntry{[]byte("a"), []byte("a")},
				TranslationEntry{[]byte("b"), []byte("b")},
				TranslationEntry{[]byte("c"), []byte("c")},
				TranslationEntry{[]byte("d"), []byte("d")},
			},
			"fr_FR": []TranslationEntry{
				TranslationEntry{[]byte("a"), []byte("[a]")},
				TranslationEntry{[]byte("b"), []byte("[b]")},
				TranslationEntry{[]byte("d"), []byte("[d]")},
			},
		})
		checkTableIntegrity(t, &table)

		expectedStringTable := []byte{
			0x0,                   // 0: ""
			0x5b, 0x61, 0x5d, 0x0, // 1: "[a]"
			0x5b, 0x62, 0x5d, 0x0, // 5: "[b]"
			0x5b, 0x64, 0x5d, 0x0, // 9: "[d]"
			0x61, 0x0, // 13: "a"
			0x62, 0x0, // 15: "b"
			0x63, 0x0, // 17: "c"
			0x64, 0x0, // 19: "d"
		}
		if !bytes.Equal(table.StringTable, expectedStringTable) {
			t.Errorf("unexpected string table %#v", table.StringTable)
		}
		fr := 0
		r := table.RelativeMappingTable
		assertOffsetsEqual(t, r[1].StringOffsets[fr], 1) // "a" -> "[a]" (1) relative to 0
		assertOffsetsEqual(t, r[2].StringOffsets[fr], 4) // "b" -> "[b]" (5) relative to "[a]" (1)
		assertOffsetsEqual(t, r[3].StringOffsets[fr], 0) // "c" -> (none)
		assertOffsetsEqual(t, r[4].StringOffsets[fr], 4) // "d" -> "[d]" (9) relative to "[b]" (5)
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
