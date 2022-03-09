// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

// This file implements a writer for APPX archives.
//
// APPX archives are ZIP archives with certain constraints
// or extensions, such as an AppxBlockMap.xml file and
// 64-bit data descriptor fields.
//
// MSIX installers use the APPX format.
//
// References:
//
// * ZIP file format: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
package main

import "archive/zip"
import "bytes"
import "compress/flate"
import "crypto/sha256"
import "encoding/base64"
import "encoding/binary"
import "encoding/xml"
import "fmt"
import "hash"
import "hash/crc32"
import "io"

const APPXBlockMapFileName = "AppxBlockMap.xml"

const appxBlockSize int64 = 1 << 16
const appxBlockUncompressedSize int64 = -1

const zipVersion uint16 = 0x002d

// ZIP flags
const sizeInDataDescriptor uint16 = 0x0008

type APPXWriter struct {
	// file wraps the underlying file we are writing to.
	file *countingWriter
	// The last discovered error.
	err error

	currentFile appxWriterInProgressFile
	files       []appxFileInfo
}

func NewAPPXWriter(file io.Writer) APPXWriter {
	return APPXWriter{
		file:        newCountingWriter(file),
		err:         nil,
		currentFile: nil,
		files:       []appxFileInfo{},
	}
}

// Be sure to later call ReadAPPXBlockMapForRawFiles after
// calling CreateRaw.
func (w *APPXWriter) CreateRaw(header *zip.FileHeader) (io.Writer, error) {
	fileInfo := w.addNewFile(header)
	fileInfo.needBlocksFromExistingBlockMap = true
	writer := appxRawFileWriter{
		w:                w,
		dataOffset:       w.tell(),
		crc32:            header.CRC32,
		uncompressedSize: header.UncompressedSize64,
	}
	w.currentFile = &writer
	return &writer, w.err
}

func (w *APPXWriter) Create(name string) (io.Writer, error) {
	return w.CreateHeader(&zip.FileHeader{
		Name:   name,
		Method: zip.Deflate,
	})
}

func (w *APPXWriter) CreateHeader(header *zip.FileHeader) (io.Writer, error) {
	w.addNewFile(header)

	var blockWriter chunkingWriter
	switch header.Method {
	case zip.Store:
		writer := &appxStoringFileWriter{
			w:           w,
			dataOffset:  w.tell(),
			blockWriter: nil,
			crc32:       crc32.NewIEEE(),
			blocks:      []appxBlock{},
		}
		blockWriter = newChunkingWriter(writer, appxBlockSize)
		writer.blockWriter = &blockWriter
		w.currentFile = writer

	case zip.Deflate:
		compressor, err := flate.NewWriter(w.file, flate.BestCompression)
		if err != nil {
			return nil, err
		}
		writer := &appxDeflatingFileWriter{
			w:                w,
			dataOffset:       w.tell(),
			compressor:       compressor,
			blockWriter:      nil,
			crc32:            crc32.NewIEEE(),
			blocks:           []appxBlock{},
			uncompressedSize: 0,
		}
		blockWriter = newChunkingWriter(writer, appxBlockSize)
		writer.blockWriter = &blockWriter
		w.currentFile = writer

	default:
		return nil, fmt.Errorf("unsupported compression method %#x", header.Method)
	}
	return &blockWriter, w.err
}

func (w *APPXWriter) addNewFile(header *zip.FileHeader) *appxFileInfo {
	w.finishFileIfNeeded()

	w.files = append(w.files, appxFileInfo{
		flags:                          sizeInDataDescriptor,
		compressionMethod:              header.Method,
		lastModFileTime:                header.ModifiedTime, // TODO(strager)
		lastModFileDate:                header.ModifiedDate, // TODO(strager)
		crc32:                          0,                   // Written by finalize.
		compressedSize:                 0,                   // Written by finalize.
		uncompressedSize:               0,                   // Written by finalize.
		fileName:                       []byte(header.Name),
		localHeaderOffset:              w.tell(),
		needBlocksFromExistingBlockMap: false,
		blocks:                         nil, // Written by finalize.
	})
	fileInfo := w.lastFile()
	w.writeLocalFileHeader(*fileInfo)
	return fileInfo
}

func (w *APPXWriter) finishFileIfNeeded() {
	if w.currentFile != nil {
		file := w.lastFile()
		w.currentFile.finalize(file)
		w.writeDataDescriptor(*file)
		w.currentFile = nil
	}
}

// When modifying an APPX file, you may want to use
// CreateRaw to avoid recompressing files. In this case, you
// must call ReadAPPXBlockMapForRawFiles with the old APPX's
// AppxBlockMap.xml to preserve the hashes.
func (w *APPXWriter) ReadAPPXBlockMapForRawFiles(xmlFile io.Reader) error {
	type APPXBlockMapXMLBlock struct {
		HashBase64 string `xml:"Hash,attr"`
		Size       *int64 `xml:"Size,attr"`
	}
	type APPXBlockMapXMLFile struct {
		Blocks []APPXBlockMapXMLBlock `xml:"Block"`
		Name   []byte                 `xml:"Name,attr"`
		Size   int64                  `xml:"Size,attr"`
	}
	type APPXBlockMapXML struct {
		Files []APPXBlockMapXMLFile `xml:"File"`
	}
	var blockMap APPXBlockMapXML
	if err := xml.NewDecoder(xmlFile).Decode(&blockMap); err != nil {
		return err
	}

	appxBlocksFromXML := func(xmlBlocks []APPXBlockMapXMLBlock) []appxBlock {
		outBlocks := make([]appxBlock, len(xmlBlocks))
		for blockIndex, xmlBlock := range xmlBlocks {
			size := appxBlockUncompressedSize
			if xmlBlock.Size != nil {
				size = *xmlBlock.Size
			}
			outBlocks[blockIndex] = appxBlock{
				sha256Base64: xmlBlock.HashBase64,
				size:         size,
			}
		}
		return outBlocks
	}

	for fileIndex := range w.files {
		file := &w.files[fileIndex]
		if file.needBlocksFromExistingBlockMap {
			for _, blockMapFile := range blockMap.Files {
				if bytes.Equal(blockMapFile.Name, file.fileNameForAPPXBlockMap()) {
					file.blocks = appxBlocksFromXML(blockMapFile.Blocks)
				}
			}
		}
	}
	return nil
}

func (w *APPXWriter) WriteAPPXBlockMap() error {
	filesExceptAPPXBlockMap := w.files
	entry, err := w.CreateHeader(&zip.FileHeader{
		Name:         APPXBlockMapFileName,
		Method:       zip.Store,
		ModifiedTime: 0, // TODO(strager)
		ModifiedDate: 0, // TODO(strager)
	})
	if err != nil {
		return err
	}
	if err := writeAPPXBlockMap(entry, filesExceptAPPXBlockMap); err != nil {
		return err
	}
	return nil
}

func writeAPPXBlockMap(w io.Writer, files []appxFileInfo) error {
	fmt.Fprintf(w, "<?xml version='1.0' encoding='UTF-8' standalone='no'?>\n")
	fmt.Fprintf(w, "<BlockMap xmlns='http://schemas.microsoft.com/appx/2010/blockmap' HashMethod='http://www.w3.org/2001/04/xmlenc#sha256'>\n")
	for _, file := range files {
		fmt.Fprintf(w, "<File Name='")
		xml.EscapeText(w, file.fileNameForAPPXBlockMap())
		fmt.Fprintf(w, "' Size='%d' LfhSize='%d'>\n", file.uncompressedSize, file.localFileHeaderSize())
		for _, block := range file.blocks {
			hash := block.sha256Base64
			if block.size == appxBlockUncompressedSize {
				fmt.Fprintf(w, "  <Block Hash='%s'/>\n", hash)
			} else {
				fmt.Fprintf(w, "  <Block Hash='%s' Size='%d'/>\n", hash, block.size)
			}
		}
		fmt.Fprintf(w, "</File>\n")
	}
	fmt.Fprintf(w, "</BlockMap>\n")
	// TODO(strager): Report Fprintf errors.
	return nil
}

func (w *APPXWriter) Close() error {
	w.finishFileIfNeeded()

	var centralDirectoryInfo appxCentralDirectoryInfo
	centralDirectoryInfo.offset = w.tell()
	for _, file := range w.files {
		w.writeCentralDirectoryHeader(&file)
	}
	centralDirectoryInfo.endOffset = w.tell()
	w.writeZip64EndOfCentralDirectoryRecord(centralDirectoryInfo)
	w.writeZip64EndOfCentralDirectoryLocator(centralDirectoryInfo)
	w.writeEndOfCentralDirectoryRecord()

	return w.err
}

type appxCentralDirectoryInfo struct {
	offset    int64
	endOffset int64
}

func (w *APPXWriter) writeLocalFileHeader(file appxFileInfo) {
	// Keep in sync with localFileHeaderSize.
	w.u32(0x04034b50)                 // local file header signature
	w.u16(zipVersion)                 // version needed to extract
	w.u16(file.flags)                 // general purpose bit flag
	w.u16(file.compressionMethod)     // compression method
	w.u16(file.lastModFileTime)       // last mod file time
	w.u16(file.lastModFileDate)       // last mod file date
	w.u32(0)                          // crc-32
	w.u32(0)                          // compressed size
	w.u32(0)                          // uncompressed size
	w.u16(uint16(len(file.fileName))) // file name length
	w.u16(0)                          // extra field length
	w.bytes(file.fileName)            // file name
	// extra field (empty)
}

func (w *APPXWriter) writeDataDescriptor(file appxFileInfo) {
	w.u32(0x08074b50)                    // data descriptor signature
	w.u32(file.crc32)                    // crc-32
	w.u64(uint64(file.compressedSize))   // compressed size
	w.u64(uint64(file.uncompressedSize)) // uncompressed size
}

func (w *APPXWriter) writeCentralDirectoryHeader(file *appxFileInfo) {
	zip64ExtraFieldDataSize := 3 * 8

	w.u32(0x02014b50)                              // central file header signature
	w.u16(zipVersion)                              // version made by
	w.u16(zipVersion)                              // version needed to extract
	w.u16(file.flags)                              // general purpose bit flag
	w.u16(file.compressionMethod)                  // compression method
	w.u16(file.lastModFileTime)                    // last mod file time
	w.u16(file.lastModFileDate)                    // last mod file date
	w.u32(file.crc32)                              // crc-32
	w.u32(0xffffffff)                              // compressed size (Zip64)
	w.u32(0xffffffff)                              // uncompressed size (Zip64)
	w.u16(uint16(len(file.fileName)))              // file name length
	w.u16(uint16(2 + 2 + zip64ExtraFieldDataSize)) // extra field length
	w.u16(0)                                       // file comment length
	w.u16(0)                                       // disk number start
	w.u16(0)                                       // internal file attributes
	w.u32(0)                                       // external file attributes
	w.u32(0xffffffff)                              // relative offset of local header (Zip64)

	w.bytes(file.fileName) // file name

	// Zip64 extra field
	w.u16(0x0001)                          // header ID: Zip64
	w.u16(uint16(zip64ExtraFieldDataSize)) // data size
	w.u64(uint64(file.uncompressedSize))   // original size
	w.u64(uint64(file.compressedSize))     // compressed size
	w.u64(uint64(file.localHeaderOffset))  // relative header offset
}

func (w *APPXWriter) writeZip64EndOfCentralDirectoryRecord(centralDirectoryInfo appxCentralDirectoryInfo) {
	w.u32(0x06064b50)                                                           // zip64 end of central dir signature
	w.u64(2 + 2 + 4 + 4 + 8 + 8 + 8 + 8)                                        // size of zip64 end of central directory record
	w.u16(zipVersion)                                                           // version made by
	w.u16(zipVersion)                                                           // version needed to extract
	w.u32(0)                                                                    // number of this disk
	w.u32(0)                                                                    // number of the disk with the start of the central directory
	w.u64(uint64(len(w.files)))                                                 // total number of entries in the central directory on this disk
	w.u64(uint64(len(w.files)))                                                 // total number of entries in the central directory
	w.u64(uint64(centralDirectoryInfo.endOffset - centralDirectoryInfo.offset)) // size of the central directory
	w.u64(uint64(centralDirectoryInfo.offset))                                  // offset of start of central directory with respect to the starting disk number
}

func (w *APPXWriter) writeZip64EndOfCentralDirectoryLocator(centralDirectoryInfo appxCentralDirectoryInfo) {
	w.u32(0x07064b50)                             // zip64 end of central dir locator signature
	w.u32(0)                                      // number of the disk with the start of the zip64 end of central directory
	w.u64(uint64(centralDirectoryInfo.endOffset)) // relative offset of the zip64 end of central directory record
	w.u32(1)                                      // total number of disks
}

func (w *APPXWriter) writeEndOfCentralDirectoryRecord() {
	w.u32(0x06054b50) // end of central dir signature
	w.u16(0xffff)     // number of this disk
	w.u16(0xffff)     // number of the disk with the start of the central directory
	w.u16(0xffff)     // total number of entries in the central directory on this disk
	w.u16(0xffff)     // total number of entries in the central directory
	w.u32(0xffffffff) // size of the central directory
	w.u32(0xffffffff) // offset of start of central directory with respect to the starting disk number
	w.u16(0)          // .ZIP file comment length
}

func (w *APPXWriter) lastFile() *appxFileInfo {
	return &w.files[len(w.files)-1]
}

func (w *APPXWriter) tell() int64 {
	return w.file.totalBytesWritten
}

func (w *APPXWriter) u16(data uint16) {
	if w.err != nil {
		return
	}
	w.err = binary.Write(w.file, binary.LittleEndian, data)
}

func (w *APPXWriter) u32(data uint32) {
	if w.err != nil {
		return
	}
	w.err = binary.Write(w.file, binary.LittleEndian, data)
}

func (w *APPXWriter) u64(data uint64) {
	if w.err != nil {
		return
	}
	w.err = binary.Write(w.file, binary.LittleEndian, data)
}

func (w *APPXWriter) bytes(data []byte) {
	if w.err != nil {
		return
	}
	_, w.err = w.file.Write(data)
}

type appxWriterInProgressFile interface {
	finalize(outFile *appxFileInfo)
}

// appxRawFileWriter writes data to a ZIP file as-is. See
// APPXWriter.OpenRaw.
type appxRawFileWriter struct {
	w          *APPXWriter
	dataOffset int64

	uncompressedSize uint64
	crc32            uint32
}

func (w *appxRawFileWriter) Write(data []byte) (int, error) {
	w.w.bytes(data)
	return len(data), w.w.err
}

func (f *appxRawFileWriter) finalize(outFile *appxFileInfo) {
	outFile.compressedSize = f.w.tell() - f.dataOffset
	outFile.uncompressedSize = int64(f.uncompressedSize)
	outFile.crc32 = f.crc32
}

// appxRawFileWriter writes data to a ZIP file without
// compression. See APPXWriter.CreateHeader.
type appxStoringFileWriter struct {
	w           *APPXWriter
	dataOffset  int64
	blockWriter *chunkingWriter

	crc32  hash.Hash32
	blocks []appxBlock
}

func (w *appxStoringFileWriter) writeChunk(data []byte, isFinalChunk bool) (int, error) {
	w.w.bytes(data)
	w.crc32.Write(data)
	w.blocks = append(w.blocks, newAPPXBlock(data, appxBlockUncompressedSize))
	return len(data), w.w.err
}

func (w *appxStoringFileWriter) finalize(outFile *appxFileInfo) {
	if err := w.blockWriter.Close(); err != nil {
		w.w.err = err
	}
	size := w.w.tell() - w.dataOffset
	outFile.compressedSize = size
	outFile.uncompressedSize = size
	outFile.crc32 = w.crc32.Sum32()
	outFile.blocks = w.blocks
}

// appxRawFileWriter writes data to a ZIP file using Deflate
// (zlib) compression. See APPXWriter.CreateHeader.
type appxDeflatingFileWriter struct {
	w           *APPXWriter
	dataOffset  int64
	compressor  *flate.Writer
	blockWriter *chunkingWriter

	crc32            hash.Hash32
	blocks           []appxBlock
	uncompressedSize int64
}

func (w *appxDeflatingFileWriter) writeChunk(data []byte, isFinalChunk bool) (int, error) {
	offsetBeforeData := w.w.tell()
	if _, err := w.compressor.Write(data); err != nil {
		w.w.err = err
	}
	if err := w.compressor.Flush(); err != nil {
		w.w.err = err
	}
	if isFinalChunk {
		if err := w.compressor.Close(); err != nil {
			w.w.err = err
		}
	} else {
		// Make it possible to seek to the next chunk. This
		// is required for APPX validation.
		w.compressor.Reset(w.w.file)
	}
	offsetAfterData := w.w.tell()

	w.crc32.Write(data)
	w.uncompressedSize += int64(len(data))
	w.blocks = append(w.blocks, newAPPXBlock(data, offsetAfterData-offsetBeforeData))
	return len(data), w.w.err
}

func (w *appxDeflatingFileWriter) finalize(outFile *appxFileInfo) {
	if err := w.blockWriter.Close(); err != nil {
		w.w.err = err
	}

	outFile.compressedSize = w.w.tell() - w.dataOffset
	outFile.uncompressedSize = w.uncompressedSize
	outFile.crc32 = w.crc32.Sum32()
	outFile.blocks = w.blocks
}

type appxFileInfo struct {
	// Used in the ZIP central directory:
	flags             uint16
	compressionMethod uint16
	lastModFileTime   uint16
	lastModFileDate   uint16
	crc32             uint32
	compressedSize    int64
	uncompressedSize  int64
	fileName          []byte
	localHeaderOffset int64

	// Used in AppxBlockMap.xml:
	// If needBlocksFromExistingBlockMap is true, then the
	// file was opened with OpenRaw. In this case, blocks
	// will be populated by reading the old
	// AppxBlockMap.xml.
	needBlocksFromExistingBlockMap bool
	blocks                         []appxBlock
}

// fileNameForAPPXBlockMap returns the name that would
// appear in the AppxBlockMap.xml file (before XML
// escaping).
func (file *appxFileInfo) fileNameForAPPXBlockMap() []byte {
	return bytes.ReplaceAll(file.fileName, []byte("/"), []byte("\\"))
}

func (file *appxFileInfo) localFileHeaderSize() int {
	// Keep in sync with writeLocalFileHeader.
	return 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + len(file.fileName)
}

// appxBlock represents a <Block> element in
// AppxBlockMap.xml.
type appxBlock struct {
	sha256Base64 string
	// If this file is compressed, 'size' stores the number
	// of compressed bytes in the ZIP file. If this file is
	// uncompressed, 'size' is appxBlockUncompressedSize.
	size int64
}

func newAPPXBlock(data []byte, size int64) appxBlock {
	hash := sha256.Sum256(data)
	return appxBlock{
		sha256Base64: base64.StdEncoding.EncodeToString(hash[:]),
		size:         size,
	}
}

// chunkingWriter buffers data until chunkSize bytes have
// been written. Then, it calls the io.Writer's Write method
// with a chunkSize-sized slice of data.
type chunkingWriter struct {
	writer chunkWriter

	chunkSize int64
	buffer    []byte
}

type chunkWriter interface {
	writeChunk(data []byte, isFinalChunk bool) (int, error)
}

func newChunkingWriter(writer chunkWriter, chunkSize int64) chunkingWriter {
	return chunkingWriter{
		writer:    writer,
		chunkSize: chunkSize,
		buffer:    []byte{},
	}
}

func (w *chunkingWriter) Write(data []byte) (int, error) {
	w.buffer = append(w.buffer, data...)
	if err := w.flushCompleteChunks(); err != nil {
		return len(data), err
	}
	return len(data), nil
}

func (w *chunkingWriter) Close() error {
	if err := w.flushCompleteChunks(); err != nil {
		return err
	}
	_, err := w.writer.writeChunk(w.buffer, true)
	if err != nil {
		return err
	}
	w.buffer = w.buffer[:0]
	return nil
}

func (w *chunkingWriter) flushCompleteChunks() error {
	for int64(len(w.buffer)) > w.chunkSize {
		bytesWritten, err := w.writer.writeChunk(w.buffer[:w.chunkSize], false)
		if err != nil {
			return err
		}
		w.buffer = w.buffer[bytesWritten:]
	}
	return nil
}

type countingWriter struct {
	w                 io.Writer
	totalBytesWritten int64
}

func newCountingWriter(w io.Writer) *countingWriter {
	return &countingWriter{w: w, totalBytesWritten: 0}
}

func (w *countingWriter) Write(data []byte) (int, error) {
	bytesWritten, err := w.w.Write(data)
	w.totalBytesWritten += int64(bytesWritten)
	return bytesWritten, err
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
