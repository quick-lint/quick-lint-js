import Foundation

// MARK: ValueReader

/// Represents a JSON value decoded from the sajson AST.  This type provides decoded access
/// to array and object elements lazily.
///
/// WARNING: Do NOT store these outside of your immediate parsing code - `ValueReader`
/// accesses memory owned by the Document, and if the Document is deallocated before a
/// ValueReader is used, Bad Things Will Happen.
public enum ValueReader {
    case integer(Int32)
    case double(Float64)
    case null
    case bool(Bool)
    case string(String)
    case array(ArrayReader)
    case object(ObjectReader)

    // Deeply inflate this ValueReader into a typed Value.
    public var value: Value {
        switch self {
        case .integer(let i): return .integer(i)
        case .double(let d): return .double(d)
        case .null: return .null
        case .bool(let b): return .bool(b)
        case .string(let s): return .string(s)
        case .array(let reader):
            var result: [Value] = []
            result.reserveCapacity(reader.count)
            for element in reader {
                result.append(element.value)
            }
            return .array(result)
        case .object(let reader):
            var result: [String: Value] = [:]
            for (key, element) in reader {
                result[key] = element.value
            }
            return .object(result)
        }
    }

    // Deeply inflate this ValueReader into an untyped Swift Any.
    public var valueAsAny: Any {
        switch self {
        case .integer(let i): return i
        case .double(let d): return d
        case .null: return NSNull()
        case .bool(let b): return b
        case .string(let s): return s
        case .array(let reader):
            var result: [Any] = []
            result.reserveCapacity(reader.count)
            for element in reader {
                result.append(element.valueAsAny)
            }
            return result
        case .object(let reader):
            var result: [String: Any] = [:]
            for (key, element) in reader {
                result[key] = element.valueAsAny
            }
            return result
        }
    }
}

// Encapsulates logic required to read from an array.
public struct ArrayReader: Sequence {
    fileprivate init(payload: UnsafePointer<UInt>, input: UnsafeBufferPointer<UInt8>) {
        self.payload = payload
        self.input = input
    }

    public subscript(i: Int)-> ValueReader {
        if i >= count {
            preconditionFailure("Index out of range: \(i)")
        }

        let element = payload[1 + i]
        let elementTag = UInt8(element & 7)
        let elementOffset = Int(element >> 3)
        return ASTNode(tag: elementTag, payload: payload.advanced(by: elementOffset), input: input).valueReader
    }

    public var count: Int {
        return Int(payload[0])
    }

    // MARK: Sequence

    public struct Iterator: IteratorProtocol {
        fileprivate init(arrayReader: ArrayReader) {
            self.arrayReader = arrayReader
        }

        public mutating func next() -> ValueReader? {
            if currentIndex < arrayReader.count {
                let value = arrayReader[currentIndex]
                currentIndex += 1
                return value
            } else {
                return nil
            }
        }

        private var currentIndex = 0
        private let arrayReader: ArrayReader
    }

    public func makeIterator() -> ArrayReader.Iterator {
        return Iterator(arrayReader: self)
    }

    // MARK: Private

    private let payload: UnsafePointer<UInt>
    private let input: UnsafeBufferPointer<UInt8>
}

// Encapsulates logic required to read from an object.
public struct ObjectReader: Sequence {
    fileprivate init(payload: UnsafePointer<UInt>, input: UnsafeBufferPointer<UInt8>) {
        self.payload = payload
        self.input = input
    }

    public var count: Int {
        return Int(payload[0])
    }

    public subscript(i: Int) -> (String, ValueReader) {
        if i >= count {
            preconditionFailure("Index out of range: \(i)")
        }

        let start = Int(payload[1 + i * 3])
        let end = Int(payload[2 + i * 3])
        let value = Int(payload[3 + i * 3])

        let key = decodeString(input, start, end)

        let valueTag = UInt8(value & 7)
        let valueOffset = Int(value >> 3)
        return (key, ASTNode(tag: valueTag, payload: payload.advanced(by: valueOffset), input: input).valueReader)
    }

    public subscript(key: String) -> ValueReader? {
        let objectLocation = sajson_find_object_key(payload, key, key.lengthOfBytes(using: .utf8), input.baseAddress!)
        if objectLocation >= count {
            return nil
        }

        let element = payload[3 + objectLocation * 3]
        let elementTag = UInt8(element & 7)
        let elementOffset = Int(element >> 3)
        return ASTNode(tag: elementTag, payload: payload.advanced(by: elementOffset), input: input).valueReader
    }

    /// Returns the object as a dictionary. Should generally be avoided, as it is less efficient than directly reading
    /// values.
    public func asDictionary() -> [String: ValueReader] {
        var result = [String: ValueReader](minimumCapacity: self.count)
        for i in 0..<self.count {
            let start = Int(payload[1 + i * 3])
            let end = Int(payload[2 + i * 3])
            let value = Int(payload[3 + i * 3])

            let key = decodeString(input, start, end)

            let valueTag = UInt8(value & 7)
            let valueOffset = Int(value >> 3)
            result[key] = ASTNode(tag: valueTag, payload: payload.advanced(by: valueOffset), input: input).valueReader
        }
        return result
    }

    // MARK: Sequence

    public struct Iterator: IteratorProtocol {
        fileprivate init(objectReader: ObjectReader) {
            self.objectReader = objectReader
        }

        public mutating func next() -> (String, ValueReader)? {
            if currentIndex < objectReader.count {
                let value = objectReader[currentIndex]
                currentIndex += 1
                return value
            } else {
                return nil
            }
        }

        private var currentIndex = 0
        private let objectReader: ObjectReader
    }
    
    public func makeIterator() -> ObjectReader.Iterator {
        return Iterator(objectReader: self)
    }

    // MARK: Private

    private let payload: UnsafePointer<UInt>
    private let input: UnsafeBufferPointer<UInt8>
}

// MARK: Value

/// Represents a fully-decoded JSON parse tree.  This API is provided for convenience, but for
/// optimal performance, consider using `ValueReader` instead.
public enum Value {
    case integer(Int32)
    case double(Float64)
    case null
    case bool(Bool)
    case string(String)
    case array([Value])
    case object([String: Value])
}

// Internal type that represents a decodable sajson AST node.
private struct ASTNode {
    // Keep this in sync with sajson::type
    private struct RawTag {
        static let integer: UInt8 = 0
        static let double: UInt8 = 1
        static let null: UInt8 = 2
        static let bfalse: UInt8 = 3
        static let btrue: UInt8 = 4
        static let string: UInt8 = 5
        static let array: UInt8 = 6
        static let object: UInt8 = 7
    }
    
    fileprivate init(tag: UInt8, payload: UnsafePointer<UInt>, input: UnsafeBufferPointer<UInt8>) {
        self.tag = tag
        self.payload = payload
        self.input = input
    }

    public var valueReader: ValueReader {
        switch tag {
        case RawTag.integer:
            // This syntax to read the bottom bits of a UInt as an Int32 is insane.
            return payload.withMemoryRebound(to: Int32.self, capacity: 1) { p in
                return .integer(p[0])
            }
        case RawTag.double:
            if MemoryLayout<Int>.size == MemoryLayout<Int32>.size {
                let lo = UInt64(payload[0])
                let hi = UInt64(payload[1])
                let bitPattern = lo | (hi << 32)
                return .double(Float64(bitPattern: bitPattern))
            } else {
                return .double(Float64(bitPattern: UInt64(payload[0])))
            }
        case RawTag.null:
            return .null
        case RawTag.bfalse:
            return .bool(false)
        case RawTag.btrue:
            return .bool(true)
        case RawTag.string:
            let start = Int(payload[0])
            let end = Int(payload[1])
            return .string(decodeString(input, start, end))
        case RawTag.array:
            return .array(ArrayReader(payload: payload, input: input))
        case RawTag.object:
            return .object(ObjectReader(payload: payload, input: input))
        default:
            fatalError("Unknown sajson value type - memory corruption detected?")
        }
    }


    // MARK: Private

    private let tag: UInt8
    private let payload: UnsafePointer<UInt>
    private let input: UnsafeBufferPointer<UInt8>
}

// Internal function that, as cheaply as possible, converts an in-memory buffer
// containing UTF-8 into a Swift string.
private func decodeString(_ input: UnsafeBufferPointer<UInt8>, _ start: Int, _ end: Int) -> String {
    // TODO: are the following two lines a single copy?
    // Does it validate the correctness of the UTF-8 or can we force it to simply memcpy?
    let data = Data(bytesNoCopy: UnsafeMutableRawPointer(mutating: input.baseAddress!.advanced(by: start)), count: end - start, deallocator: .none)
    return String(data: data, encoding: .utf8)!
}

/// Represents a parsed JSON document and a handle to any referenced memory.
public final class Document {
    internal init(doc: OpaquePointer!, input: Data) {
        self.doc = doc
        self.input = input

        let rootTag = sajson_get_root_tag(doc)
        let rootValuePaylod = sajson_get_root(doc)!
        let inputPointer = sajson_get_input(doc)!
        let inputLength = sajson_get_input_length(doc)

        self.rootNode = ASTNode(
            tag: rootTag,
            payload: rootValuePaylod,
            input: UnsafeBufferPointer(start: inputPointer, count: inputLength))
    }
    
    deinit {
        sajson_free_document(doc)
    }

    /// Provides access to the root value reader of the JSON document.  Using `ValueReader`
    /// is faster than `Value` because it avoids the need to construct intermediate Swift
    /// arrays and dictionaries.
    ///
    /// This function is structured as a closure rather than a property to prevent accidentally
    /// holding onto a `ValueReader` before the `Document` has been deallocated.
    public func withRootValueReader<T>(_ cb: (ValueReader) -> T) -> T {
        return cb(rootNode.valueReader)
    }

    /// Decodes the entire document into a Swift `Value` tree.
    /// This accessor is convenient, but for optimum performance, use `withRootValueReader` instead.
    var rootValue: Value {
        return withRootValueReader { rootReader in
            return rootReader.value
        }
    }

    // MARK: Private

    private let rootNode: ASTNode
    private let doc: OpaquePointer!
    private let input: Data // We need to hold onto the memory from the buffer we parsed
}

public final class ParseError: Error {
    internal init(line: Int, column: Int, message: String) {
        self.line = line
        self.column = column
        self.message = message
    }
    
    public let line: Int
    public let column: Int
    public let message: String
}

public enum AllocationStrategy {
    /// Allocates one machine word per byte in the input document.  This mode is the fastest.
    case single

    /// Dynamically grows the AST buffer and parse stack at the cost of being about 10% slower.
    case dynamic
}

/// Parses an input document given a buffer of JSON data.  For efficiency, this function
/// mutates the given data.  Use `parse(allocationStrategy:input:)` if you intend to use
/// the passed buffer again.
///
/// Throws `ParseError` on failure.
public func parse(allocationStrategy: AllocationStrategy, mutating: inout Data) throws -> Document {
    let inputLength = mutating.count

    let dptr: OpaquePointer! = mutating.withUnsafeMutableBytes { (ptr: UnsafeMutablePointer<Int8>) in
        switch allocationStrategy {
        case .single:
            return sajson_parse_single_allocation(ptr, inputLength)
        case .dynamic:
            return sajson_parse_dynamic_allocation(ptr, inputLength)
        }
    }

    if dptr == nil {
        fatalError("Out of memory: failed to allocate document structure")
    }

    if sajson_has_error(dptr) != 0 {
        throw ParseError(
            line: sajson_get_error_line(dptr),
            column: sajson_get_error_column(dptr),
            message: String(cString: sajson_get_error_message(dptr)))
    }

    return Document(doc: dptr, input: mutating)
}

/// Parses an input document given a buffer of JSON data.
///
/// Throws `ParseError` on failure.
public func parse(allocationStrategy: AllocationStrategy, input: Data) throws -> Document {
    var copy = input
    return try parse(allocationStrategy: allocationStrategy, mutating: &copy)
}

/// Parses an input document given a String containing JSON.  If you have binary data, it's
/// more efficient to use `parse(allocationStrategy:input:)` or `parse(allocationStrategy:mutating:)`
/// instead.
///
/// Throws `ParseError` on failure.
public func parse(allocationStrategy: AllocationStrategy, input: String) throws -> Document {
    var copy = input.data(using: .utf8)!
    return try parse(allocationStrategy: allocationStrategy, mutating: &copy)
}
