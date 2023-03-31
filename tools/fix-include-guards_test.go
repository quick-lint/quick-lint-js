// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "bytes"
import "testing"

func TestIncludeGuardMacroName(t *testing.T) {
	assertEquals := func(actual string, expected string) {
		if actual != expected {
			t.Errorf("expected %#v, but got %#v", expected, actual)
		}
	}

	t.Run("no quick-lint-js in path fails", func(t *testing.T) {
		assertEquals(IncludeGuardMacroName("hello.h"), "")
		assertEquals(IncludeGuardMacroName("hi/hello.h"), "")
	})

	t.Run("single component after quick-lint-js", func(t *testing.T) {
		assertEquals(IncludeGuardMacroName("quick-lint-js/hello.h"), "QUICK_LINT_JS_HELLO_H")
		assertEquals(IncludeGuardMacroName("quick-lint-js/this-is-a-test-123.hpp"), "QUICK_LINT_JS_THIS_IS_A_TEST_123_HPP")
	})

	t.Run("two components after quick-lint-js", func(t *testing.T) {
		assertEquals(IncludeGuardMacroName("quick-lint-js/component/hello.h"), "QUICK_LINT_JS_COMPONENT_HELLO_H")
	})

	t.Run("quick-lint-js in subdirectory", func(t *testing.T) {
		assertEquals(IncludeGuardMacroName("src/quick-lint-js/hello.h"), "QUICK_LINT_JS_HELLO_H")
		assertEquals(IncludeGuardMacroName("test/quick-lint-js/hi.h"), "QUICK_LINT_JS_HI_H")
		assertEquals(IncludeGuardMacroName("dir/subdir/quick-lint-js/hi.h"), "QUICK_LINT_JS_HI_H")
	})

	t.Run("multiple quick-lint-js components in path", func(t *testing.T) {
		assertEquals(IncludeGuardMacroName("quick-lint-js/src/quick-lint-js/hello.h"), "QUICK_LINT_JS_HELLO_H")
	})

}

func TestAddIncludeGuardIfMissing(t *testing.T) {
	// TODO(strager)
}

func TestFixExistingIncludeGuard(t *testing.T) {
	assertEquals := func(actual []byte, expected []byte) {
		if !bytes.Equal(actual, expected) {
			t.Errorf("expected %#v, but got %#v", string(expected), string(actual))
		}
	}

	t.Run("good include guards are untouched", func(t *testing.T) {
		source := []byte("#ifndef HELLO_H\n#define HELLO_H\n")
		assertEquals(FixExistingIncludeGuard(source, "HELLO_H"), source)
	})

	t.Run("bad #ifndef line is fixed", func(t *testing.T) {
		badSource := []byte("#ifndef HELL_H\n#define HELLO_H\n")
		godSource := []byte("#ifndef HELLO_H\n#define HELLO_H\n")
		assertEquals(FixExistingIncludeGuard(badSource, "HELLO_H"), godSource)
	})

	t.Run("bad #define line is fixed", func(t *testing.T) {
		badSource := []byte("#ifndef HELLO_H\n#define HELL_H\n")
		godSource := []byte("#ifndef HELLO_H\n#define HELLO_H\n")
		assertEquals(FixExistingIncludeGuard(badSource, "HELLO_H"), godSource)
	})

	t.Run("#define is added after #ifndef if missing", func(t *testing.T) {
		badSource := []byte("#ifndef HELLO_H\nunrelated\n")
		godSource := []byte("#ifndef HELLO_H\n#define HELLO_H\nunrelated\n")
		assertEquals(FixExistingIncludeGuard(badSource, "HELLO_H"), godSource)
	})

	t.Run("#define is only fixed immediately after #ifndef", func(t *testing.T) {
		source := []byte("#ifndef HELLO_H\n#define HELLO_H\n#define HELL_H\n")
		assertEquals(FixExistingIncludeGuard(source, "HELLO_H"), source)
	})

	t.Run("#define before #ifndef is ignored", func(t *testing.T) {
		source := []byte("#define UNRELATED_H\n#ifndef HELLO_H\n#define HELLO_H\n")
		assertEquals(FixExistingIncludeGuard(source, "HELLO_H"), source)
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
