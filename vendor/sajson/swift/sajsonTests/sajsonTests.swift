import Foundation
import XCTest
import sajson_swift

class sajsonTests: XCTestCase {
    func test_empty_array() {
        _ = try! parse(allocationStrategy: .single, input: "[]")
    }

    func test_zero_embedded_strings() {
        let doc = try! parse(allocationStrategy: .single, input: "[\"a\\u0000b\"]")
        doc.withRootValueReader { docValue in
            guard case .array(let array) = docValue else { XCTFail(); return }
            XCTAssertEqual(1, array.count)
            guard case .string(let str) = array[0] else { XCTFail(); return }
            XCTAssertEqual("a\0b", str)
        }
    }

    func test_array() {
        let doc = try! parse(allocationStrategy: .single, input: "[10, \"Hello\"]")
        doc.withRootValueReader { docValue in
            guard case .array(let array) = docValue else { XCTFail(); return }
            XCTAssert(array.count == 2)
            guard case .integer(10) = array[0] else { XCTFail(); return }

            guard case .string("Hello") = array[1] else { XCTFail(); return }
        }
    }

    func test_object() {
        let doc = try! parse(allocationStrategy: .single, input: "{\"hello\": \"world\", \"hello2\": null}")
        doc.withRootValueReader { docValue in
            guard case .object(let objectReader) = docValue else { XCTFail(); return }
            XCTAssert(objectReader.count == 2)
            guard case .some(.string("world")) = objectReader["hello"] else { XCTFail(); return }

            guard case .some(.null) = objectReader["hello2"] else { XCTFail(); return }

            // Accessing a non-existent key will return none.
            guard case .none = objectReader["hello3"] else { XCTFail(); return }
        }
    }

    func test_mutating_parse_changes_input() {
        var data = "[\"\\n\"]".data(using: .utf8)!
        let _ = try! parse(allocationStrategy: .single, mutating: &data)
        XCTAssertEqual(10, data[2])
    }

    // MARK: Benchmarks

    func test_large_json_benchmark_parse_only() {
        let largeJsonData = createLargeTestJsonData(objectCount: 1000)

        measure {
            _ = try! parse(allocationStrategy: .single, input: largeJsonData)
        }
    }


    func test_large_json_benchmark_all() {
        let largeJsonData = createLargeTestJsonData(objectCount: 1000)

        measure {
            let doc = try! parse(allocationStrategy: .single, input: largeJsonData)
            doc.withRootValueReader { swiftValue in

                //XCTAssert(swiftValue.array?[0].object?["0"]?.string != nil)

                guard case .array(let array) = swiftValue else {
                    preconditionFailure()
                }

                // Verify that something was actually deserialized.
                XCTAssert(array.count == 1000)
            }
        }
    }

    func test_floats() {
        let doc = try! parse(allocationStrategy: .single, input: "{\"start\": 12.948, \"end\": 42.1234}")
        doc.withRootValueReader { docValue in
            guard case .object(let objectReader) = docValue else { XCTFail(); return }
            XCTAssert(objectReader.count == 2)
            guard case .some(.double(let start)) = objectReader["start"] else { XCTFail(); return }
            guard case .some(.double(let end)) = objectReader["end"] else { XCTFail(); return }

            XCTAssertEqual(12.948, start, accuracy: 0.001)
            XCTAssertEqual(42.1234, end, accuracy: 0.001)
        }
    }

    // MARK: Helpers

    func createLargeTestJsonData(objectCount: Int) -> Data {
        var largeArray = [[String: Any]]()
        for _ in 0..<objectCount {
            largeArray.append(createTestJsonObject())
        }
        return try! JSONSerialization.data(withJSONObject: largeArray)
    }

    func createTestJsonObject() -> [String: Any] {
        var jsonDict =  [String: Any]()
        for i in 0..<100 {
            jsonDict["\(i)"] = randomString()
        }
        for i in 100..<200 {
            jsonDict["\(i)"] = randomInt()
        }
        return jsonDict
    }

    private func randomString() -> String {
        return UUID().uuidString
    }

    private func randomInt() -> Int32 {
        return Int32(arc4random_uniform(UInt32(Int32.max)))
    }
}
