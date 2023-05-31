# CMake generated Testfile for 
# Source directory: C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/test
# Build directory: C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/test
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(quick-lint-js-test "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/test/quick-lint-js-test.exe")
set_tests_properties(quick-lint-js-test PROPERTIES  _BACKTRACE_TRIPLES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/test/CMakeLists.txt;244;add_test;C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/test/CMakeLists.txt;0;")
add_test(quick-lint-js-test-cli "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/test/quick-lint-js-test-cli.exe")
set_tests_properties(quick-lint-js-test-cli PROPERTIES  ENVIRONMENT "QUICK_LINT_JS_EXE=C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/out/build/x64-Debug/quick-lint-js.exe" _BACKTRACE_TRIPLES "C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/test/CMakeLists.txt;320;add_test;C:/Users/jaitj/source/repos/jaitjacob/quick-lint-js/test/CMakeLists.txt;0;")
