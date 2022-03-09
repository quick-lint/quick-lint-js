// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "reflect"
import "testing"

type spyWriter struct {
	writes []spyWriterChunk
}

type spyWriterChunk struct {
	data    []byte
	isFinal bool
}

func newSpyWriter() spyWriter {
	return spyWriter{
		writes: []spyWriterChunk{},
	}
}

func (w *spyWriter) writeChunk(data []byte, isFinalChunk bool) (int, error) {
	dataCopy := make([]byte, len(data))
	copy(dataCopy, data)
	w.writes = append(w.writes, spyWriterChunk{
		data:    dataCopy,
		isFinal: isFinalChunk,
	})
	return len(dataCopy), nil
}

func TestChunkingWriter(t *testing.T) {
	assertWriteCalls := func(t *testing.T, spy *spyWriter, expected []spyWriterChunk) {
		if !reflect.DeepEqual(spy.writes, expected) {
			t.Errorf(
				"expected %#v, but got %#v",
				expected,
				spy.writes,
			)
		}
	}

	t.Run("no writes creates one final empty chunk", func(t *testing.T) {
		spy := newSpyWriter()
		w := newChunkingWriter(&spy, 2)
		if err := w.Close(); err != nil {
			t.Error(err)
		}

		assertWriteCalls(t, &spy, []spyWriterChunk{
			spyWriterChunk{data: []byte{}, isFinal: true},
		})
	})

	t.Run("single small write is final", func(t *testing.T) {
		spy := newSpyWriter()
		w := newChunkingWriter(&spy, 256)
		if _, err := w.Write([]byte{0x01}); err != nil {
			t.Error(err)
		}
		if err := w.Close(); err != nil {
			t.Error(err)
		}
		assertWriteCalls(t, &spy, []spyWriterChunk{
			spyWriterChunk{data: []byte{0x01}, isFinal: true},
		})
	})

	t.Run("small writes are grouped into one chunk", func(t *testing.T) {
		spy := newSpyWriter()
		w := newChunkingWriter(&spy, 256)
		if _, err := w.Write([]byte{0x01}); err != nil {
			t.Error(err)
		}
		if _, err := w.Write([]byte{0x02, 0x03}); err != nil {
			t.Error(err)
		}
		if _, err := w.Write([]byte{0x04}); err != nil {
			t.Error(err)
		}

		assertWriteCalls(t, &spy, []spyWriterChunk{})
		if err := w.Close(); err != nil {
			t.Error(err)
		}
		assertWriteCalls(t, &spy, []spyWriterChunk{
			spyWriterChunk{data: []byte{0x01, 0x02, 0x03, 0x04}, isFinal: true},
		})
	})

	t.Run("chunk-sized writes get their own write call", func(t *testing.T) {
		spy := newSpyWriter()
		w := newChunkingWriter(&spy, 2)
		if _, err := w.Write([]byte{0x01, 0x02}); err != nil {
			t.Error(err)
		}
		if _, err := w.Write([]byte{0x03, 0x04}); err != nil {
			t.Error(err)
		}
		if err := w.Close(); err != nil {
			t.Error(err)
		}

		assertWriteCalls(t, &spy, []spyWriterChunk{
			spyWriterChunk{data: []byte{0x01, 0x02}},
			spyWriterChunk{data: []byte{0x03, 0x04}, isFinal: true},
		})
	})

	t.Run("bigger-than-chunk-sized writes are split into multiple write calls", func(t *testing.T) {
		spy := newSpyWriter()
		w := newChunkingWriter(&spy, 3)
		if _, err := w.Write([]byte{0x01, 0x02, 0x03, 0x04}); err != nil {
			t.Error(err)
		}
		if _, err := w.Write([]byte{0x05, 0x06, 0x07, 0x08}); err != nil {
			t.Error(err)
		}
		if err := w.Close(); err != nil {
			t.Error(err)
		}

		assertWriteCalls(t, &spy, []spyWriterChunk{
			spyWriterChunk{data: []byte{0x01, 0x02, 0x03}},
			spyWriterChunk{data: []byte{0x04, 0x05, 0x06}},
			spyWriterChunk{data: []byte{0x07, 0x08}, isFinal: true},
		})
	})

	t.Run("huger-than-chunk-sized writes are split into multiple write calls", func(t *testing.T) {
		spy := newSpyWriter()
		w := newChunkingWriter(&spy, 2)
		if _, err := w.Write([]byte{0x01, 0x02, 0x03, 0x04, 0x05}); err != nil {
			t.Error(err)
		}
		if err := w.Close(); err != nil {
			t.Error(err)
		}

		assertWriteCalls(t, &spy, []spyWriterChunk{
			spyWriterChunk{data: []byte{0x01, 0x02}},
			spyWriterChunk{data: []byte{0x03, 0x04}},
			spyWriterChunk{data: []byte{0x05}, isFinal: true},
		})
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
