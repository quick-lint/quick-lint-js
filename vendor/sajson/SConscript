Import("*")

unittestpp_env = env.Clone()
unittestpp_env.Append(CPPPATH=["#/third-party/UnitTest++/src"])
unittestpp_env.Library(
    "libraries/unittestpp",
    [
        "third-party/UnitTest++/src/AssertException.cpp",
        "third-party/UnitTest++/src/Checks.cpp",
        "third-party/UnitTest++/src/CurrentTest.cpp",
        "third-party/UnitTest++/src/DeferredTestReporter.cpp",
        "third-party/UnitTest++/src/DeferredTestResult.cpp",
        "third-party/UnitTest++/src/MemoryOutStream.cpp",
        "third-party/UnitTest++/src/ReportAssert.cpp",
        "third-party/UnitTest++/src/Test.cpp",
        "third-party/UnitTest++/src/TestDetails.cpp",
        "third-party/UnitTest++/src/TestList.cpp",
        "third-party/UnitTest++/src/TestReporter.cpp",
        "third-party/UnitTest++/src/TestReporterStdout.cpp",
        "third-party/UnitTest++/src/TestResults.cpp",
        "third-party/UnitTest++/src/TestRunner.cpp",
        "third-party/UnitTest++/src/TimeConstraint.cpp",
        "third-party/UnitTest++/src/XmlTestReporter.cpp",
        "third-party/UnitTest++/src/Posix/SignalTranslator.cpp",
        "third-party/UnitTest++/src/Posix/TimeHelpers.cpp",
    ],
)

test_env = env.Clone(tools=[unittestpp, sajson])
test_env.Program("test", ["tests/test.cpp", "tests/test_no_stl.cpp"])

test_unsorted_env = test_env.Clone()
test_unsorted_env.Append(CPPDEFINES=["SAJSON_UNSORTED_OBJECT_KEYS"])
test_unsorted_env.Program(
    "test_unsorted",
    [test_unsorted_env.Object("tests/test_unsorted.o", "tests/test.cpp")],
)

bench_env = env.Clone(tools=[sajson])
bench_env.Append(CPPDEFINES=["NDEBUG"])
bench_env.Program("bench", ["benchmark/benchmark.cpp"])

parse_stats_env = env.Clone(tools=[sajson])
parse_stats_env.Program("parse_stats", ["example/main.cpp"])
