// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "reflect"
import "testing"

func TestMatchPath(t *testing.T) {
	assertPathMatches := func(pattern string, path string) {
		if !MatchPath(pattern, path) {
			t.Errorf(
				"expected true, but got false: MatchPath(%#v, %#v)",
				pattern,
				path,
			)
		}
	}
	assertPathDoesNotMatch := func(pattern string, path string) {
		if MatchPath(pattern, path) {
			t.Errorf(
				"expected false, but got true: MatchPath(%#v, %#v)",
				pattern,
				path,
			)
		}
	}

	t.Run("just file name", func(t *testing.T) {
		assertPathMatches("file.js", "/path/to/file.js")
		assertPathDoesNotMatch("notfile.js", "/path/to/file.js")
		assertPathDoesNotMatch("e.js", "/path/to/file.js")
	})

	t.Run("match with file name pattern ignores directory names", func(t *testing.T) {
		assertPathDoesNotMatch("path", "/path/to/file.js")
		assertPathDoesNotMatch("to", "/path/to/file.js")
		assertPathDoesNotMatch("/", "/path/to/file.js")
	})

	t.Run("match file name and parent", func(t *testing.T) {
		assertPathMatches("to/file.js", "/path/to/file.js")
		assertPathDoesNotMatch("other/file.js", "/path/to/file.js")
		assertPathDoesNotMatch("o/file.js", "/path/to/file.js")
		assertPathDoesNotMatch("to/file", "/path/to/file.js")
	})

	t.Run("match file glob with parent directory", func(t *testing.T) {
		assertPathMatches("to/*", "/path/to/file.js")
	})

	t.Run("realistic", func(t *testing.T) {
		assertPathMatches(
			"comment/migrated_0036.js",
			"/home/strager/tmp/Projects/esprima/test/fixtures/comment/migrated_0036.js",
		)
		assertPathDoesNotMatch(
			"comment/migrated_0036.js",
			"/home/strager/tmp/Projects/esprima/test/fixtures/expression/primary/object/migrated_0036.js",
		)
		assertPathMatches(
			"expression/primary/object/migrated_0036.js",
			"/home/strager/tmp/Projects/esprima/test/fixtures/expression/primary/object/migrated_0036.js",
		)
	})
}

func TestReverseStringSlice(t *testing.T) {
	assertEqual := func(actual []string, expected []string) {
		if !reflect.DeepEqual(actual, expected) {
			t.Errorf("expected %#v, but got %#v", actual, expected)
		}
	}

	assertReversingEquals := func(toReverse []string, expected []string) {
		ReverseStringSlice(toReverse)
		assertEqual(toReverse, expected)
	}

	t.Run("empty", func(t *testing.T) {
		assertReversingEquals([]string{}, []string{})
	})

	t.Run("single", func(t *testing.T) {
		assertReversingEquals([]string{"a"}, []string{"a"})
	})

	t.Run("two", func(t *testing.T) {
		assertReversingEquals([]string{"a", "a"}, []string{"a", "a"})
		assertReversingEquals([]string{"a", "b"}, []string{"b", "a"})
	})

	t.Run("three", func(t *testing.T) {
		assertReversingEquals(
			[]string{"a", "a", "a"},
			[]string{"a", "a", "a"},
		)
		assertReversingEquals(
			[]string{"a", "b", "c"},
			[]string{"c", "b", "a"},
		)
	})

	t.Run("four", func(t *testing.T) {
		assertReversingEquals(
			[]string{"a", "b", "c", "d"},
			[]string{"d", "c", "b", "a"},
		)
	})
}

func TestParseTestExpectations(t *testing.T) {
	assertEarlyError := func(expectations TestExpectations, expected bool) {
		if expectations.EarlyError != expected {
			t.Errorf("expected EarlyError to be %#v, but got %#v", expected, expectations.EarlyError)
		}
	}

	assertIsTest := func(expectations TestExpectations, expected bool) {
		if expectations.IsTest != expected {
			t.Errorf("expected IsTest to be %#v, but got %#v", expected, expectations.IsTest)
		}
	}

	assertIsTodoPath := func(expectations TestExpectations, expected bool) {
		if expectations.IsTodoPath != expected {
			t.Errorf("expected IsTodoPath to be %#v, but got %#v", expected, expectations.IsTodoPath)
		}
	}

	assertNeedsTodoFeatures := func(expectations TestExpectations, expected bool) {
		if expectations.NeedsTodoFeatures != expected {
			t.Errorf("expected NeedsTodoFeatures to be %#v, but got %#v", expected, expectations.NeedsTodoFeatures)
		}
	}

	t.Run("no features", func(t *testing.T) {
		source := []byte(`/*---
es5id: test
description: >
    test
---*/
print("hello world");
`)
		testTodo := TestTodo{TodoFeatures: [][]byte{
			[]byte("async-iteration"),
			[]byte("import.meta"),
		}}
		expectations := ParseTestExpectations(testTodo, source, "test.js")
		assertNeedsTodoFeatures(expectations, false)
	})

	t.Run("no todo features", func(t *testing.T) {
		source := []byte(`/*---
es5id: test
description: >
    test
features: [default-parameters, let]
---*/
print("hello world");
`)
		testTodo := TestTodo{TodoFeatures: [][]byte{
			[]byte("async-iteration"),
			[]byte("import.meta"),
		}}
		expectations := ParseTestExpectations(testTodo, source, "test.js")
		assertNeedsTodoFeatures(expectations, false)
	})

	t.Run("needs todo features", func(t *testing.T) {
		source := []byte(`/*---
es5id: test
description: >
    test
features: [async-iteration]
---*/
print("hello world");
`)
		testTodo := TestTodo{TodoFeatures: [][]byte{
			[]byte("async-iteration"),
			[]byte("import.meta"),
		}}
		expectations := ParseTestExpectations(testTodo, source, "test.js")
		assertNeedsTodoFeatures(expectations, true)
	})

	t.Run("missing frontmatter", func(t *testing.T) {
		source := []byte("print('hello world');\n")
		testTodo := TestTodo{TodoFeatures: [][]byte{
			[]byte("async-iteration"),
			[]byte("import.meta"),
		}}
		expectations := ParseTestExpectations(testTodo, source, "test.js")
		assertEarlyError(expectations, false)
		assertIsTest(expectations, false)
		assertNeedsTodoFeatures(expectations, false)
	})

	t.Run("not todo path", func(t *testing.T) {
		source := []byte("print('hello world');\n")
		testTodo := TestTodo{TodoPaths: []string{
			"orange.js",
			"rotten/*.js",
		}}
		expectations := ParseTestExpectations(testTodo, source, "banana.js")
		assertIsTodoPath(expectations, false)
	})

	t.Run("todo path", func(t *testing.T) {
		source := []byte("print('hello world');\n")
		testTodo := TestTodo{TodoPaths: []string{
			"orange.js",
			"rotten/*.js",
		}}
		expectations := ParseTestExpectations(testTodo, source, "rotten/banana.js")
		assertIsTodoPath(expectations, true)
	})

	t.Run("error-less frontmatter", func(t *testing.T) {
		source := []byte(`/*---
es5id: test
description: >
    test
---*/
print("hello world");
`)
		expectations := ParseTestExpectations(TestTodo{}, source, "test.js")
		assertEarlyError(expectations, false)
		assertIsTest(expectations, true)
	})

	t.Run("early syntax error", func(t *testing.T) {
		source := []byte(`/*---
es5id: test
description: >
  test
info: |
  test
negative:
  phase: parse
  type: SyntaxError
---*/
print("hello world"
`)
		expectations := ParseTestExpectations(TestTodo{}, source, "test.js")
		assertEarlyError(expectations, true)
		assertIsTest(expectations, true)
	})

	t.Run("resolution error", func(t *testing.T) {
		source := []byte(`/*---
description: >
    test
esid: test
info: |
    test
negative:
  phase: resolution
  type: SyntaxError
flags: [module]
---*/
import {} from "./module-with-syntax-error.mjs";
`)
		expectations := ParseTestExpectations(TestTodo{}, source, "test.js")
		assertEarlyError(expectations, false)
		assertIsTest(expectations, true)
	})

	t.Run("runtime error", func(t *testing.T) {
		source := []byte(`/*---
esid: test
description: test
info: |
    test
negative:
  phase: runtime
  type: SyntaxError
flags: [noStrict]
---*/
eval('print("hello world"');
`)
		expectations := ParseTestExpectations(TestTodo{}, source, "test.js")
		assertEarlyError(expectations, false)
		assertIsTest(expectations, true)
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
