// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <filesystem>
#include <quick-lint-js/cli/arg-parser.h>
#include <quick-lint-js/cli/text-diag-reporter.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/hash-set.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/diag/buffering-diag-reporter.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/typescript-test.h>
#include <string>
#include <system_error>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
constexpr String8_View ignored_tests[] = {
    // These tests error even with the TypeScript compiler, or cause the
    // TypeScript compiler to generate invalid code. Maybe one of the test
    // directives causes its errors to be ignored by the test runner, but I
    // can't tell which test directive would do this.
    u8"/classes/classStaticBlock/classStaticBlock22.ts"sv,
    u8"/classes/members/privateNames/privateNameInInExpression.ts"sv,
    u8"/classes/propertyMemberDeclarations/staticPropertyNameConflicts.ts"sv,
    u8"/es2018/invalidTaggedTemplateEscapeSequences.ts"sv,
    u8"/es2020/modules/exportAsNamespace3.ts"sv,
    u8"/es6/for-ofStatements/for-of53.ts"sv,
    u8"/esDecorators/classDeclaration/accessors/esDecorators-classDeclaration-accessors-nonStaticAbstract.ts"sv,
    u8"/esDecorators/classDeclaration/fields/esDecorators-classDeclaration-fields-nonStaticAbstract.ts"sv,
    u8"/esDecorators/classDeclaration/fields/esDecorators-classDeclaration-fields-nonStaticAbstractAccessor.ts"sv,
    u8"/esDecorators/classDeclaration/methods/esDecorators-classDeclaration-methods-nonStaticAbstract.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.1.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.10.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.11.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.12.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.2.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.3.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.4.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.5.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.6.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.7.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.8.ts"sv,
    u8"/externalModules/topLevelAwaitErrors.9.ts"sv,
    u8"/jsFileCompilationExternalPackageError.ts"sv,
    u8"/jsx/jsxAttributeInitializer.ts"sv,
    u8"/node/nodeModulesDeclarationEmitWithPackageExports.ts"sv,
    u8"/node/nodeModulesImportTypeModeDeclarationEmitErrors1.ts"sv,
    u8"/salsa/privateIdentifierExpando.ts"sv,
    u8"/unicodeEscapesInNames02.ts"sv,
    u8"/usedImportNotElidedInJs.ts"sv,

    // TypeScript's compiler supports TypeScript extensions in JavaScript code,
    // but quick-lint-js does not.
    u8"/node/allowJs/nodeModulesAllowJs1.ts"sv,
    u8"/node/allowJs/nodeModulesAllowJsExportAssignment.ts"sv,
    u8"/node/allowJs/nodeModulesAllowJsImportAssignment.ts"sv,
    u8"/node/allowJs/nodeModulesAllowJsSynchronousCallErrors.ts"sv,

    // These tests have errors but the tests explicitly disable checking.
    u8"/parser/ecmascript5/ArrowFunctionExpressions/parserArrowFunctionExpression10.ts"sv,
    u8"/parser/ecmascript5/ArrowFunctionExpressions/parserArrowFunctionExpression13.ts"sv,
    u8"/parser/ecmascript5/ArrowFunctionExpressions/parserArrowFunctionExpression14.ts"sv,
    u8"/parser/ecmascript5/ArrowFunctionExpressions/parserArrowFunctionExpression15.ts"sv,
    u8"/parser/ecmascript5/ArrowFunctionExpressions/parserArrowFunctionExpression16.ts"sv,
    u8"/parser/ecmascript5/ArrowFunctionExpressions/parserArrowFunctionExpression17.ts"sv,
    u8"/salsa/plainJSRedeclare3.ts"sv,

    // TODO(strager): quick-lint-js should strip metadata comments and
    // whitespace from the beginning of the test file.
    u8"/emitBundleWithShebang1.ts"sv,

    // TODO(#1122): quick-lint-js does not track namespace aliases properly.
    u8"/importedModuleClassNameClash.ts"sv,

    // TODO(#1004): Implement import assertions.
    u8"/importAssertion/"sv,
    u8"/node/nodeModulesImportAssertions.ts"sv,
    u8"/node/nodeModulesImportModeDeclarationEmit1.ts"sv,
    u8"/node/nodeModulesImportModeDeclarationEmit2.ts"sv,
    u8"/node/nodeModulesImportModeDeclarationEmitErrors1.ts"sv,

    // TODO(#1123): quick-lint-js does not parse UTF-16 source files.
    u8"/bom-utf16be.ts"sv,
    u8"/bom-utf16le.ts"sv,
    u8"/collisionCodeGenModuleWithUnicodeNames.ts"sv,
    u8"/promiseTest.ts"sv,
    u8"/unicodeIdentifierNames.ts"sv,

    // This test correctly emits E0451 (a warning).
    // TODO(strager): Disable E0451 for this test but still check this test.
    u8"/controlFlow/controlFlowCommaOperator.ts"sv,
    u8"/controlFlow/controlFlowIfStatement.ts"sv,
    u8"/controlFlow/controlFlowTruthiness.ts"sv,

    // This test correctly emits E0193 (a warning).
    // TODO(strager): Disable E0193 for this test but still check this test.
    u8"/jsx/tsxReactEmitNesting.tsx"sv,
    u8"/keywordInJsxIdentifier.tsx"sv,

    // This test correctly emits E0196 (a warning).
    // TODO(strager): Disable E0196 for this test but still check this test.
    u8"/downlevelLetConst14.ts"sv,
    u8"/downlevelLetConst15.ts"sv,
    u8"/es6/for-ofStatements/for-of23.ts"sv,
    u8"/initializePropertiesWithRenamedLet.ts"sv,
    u8"/narrowingDestructuring.ts"sv,
    u8"/statements/for-ofStatements/ES5For-of24.ts"sv,

    // FIXME(#735)
    u8"/deeplyNestedConditionalTypes.ts"sv,
    u8"/parsingDeepParenthensizedExpression.ts"sv,

    // This test correctly emits E0266 (a warning).
    // TODO(strager): Disable E0266 for this test but still check this test.
    u8"/castOfAwait.ts"sv,

    // This test correctly emits E0347 (a warning).
    // TODO(strager): Disable E0347 for this test but still check this test.
    u8"/narrowingByTypeofInSwitch.ts"sv,
    u8"/switchCaseInternalComments.ts"sv,

    // These tests correctly emit E0369 (a warning).
    // TODO(strager): Disable E0369 for all tests and don't ignore these tests.
    u8"/expressions/nullishCoalescingOperator/nullishCoalescingOperator1.ts"sv,

    // These tests correctly emit E0427 (a warning).
    // TODO(strager): Disable E0427 for all tests and don't ignore these tests.
    u8"/es5-asyncFunctionSwitchStatements.ts"_sv,
    u8"/es6/moduleExportsSystem/topLevelVarHoistingCommonJS.ts"sv,
    u8"/narrowByClauseExpressionInSwitchTrue2.ts"_sv,

    // This test correctly emits E0451 (a warning).
    // TODO(strager): Disable E0451 for this test but still check this test.
    u8"/capturedLetConstInLoop1.ts"sv,

    // TODO(#1136): Implement 'using' declarations.
    u8"/statements/VariableStatements/usingDeclarations/"sv,
};

struct Test_TypeScript_Options {
  std::vector<const char*> test_case_paths;
};

Test_TypeScript_Options parse_test_options(int argc, char** argv) {
  Test_TypeScript_Options o;

  Arg_Parser parser(argc, argv);
  QLJS_ARG_PARSER_LOOP(parser) {
    QLJS_ARGUMENT(const char* argument) {
      o.test_case_paths.push_back(argument);
    }

    QLJS_UNRECOGNIZED_OPTION(const char* unrecognized) {
      std::fprintf(stderr, "error: unrecognized option: %s\n", unrecognized);
      std::exit(2);
    }
  }

  return o;
}

class Expected_Test_Results {
 public:
  struct Expectations {
    bool has_errors;
  };

  Result<void, Platform_File_IO_Error> load_from_tests_directory(
      const char* baselines_path) {
    auto visit_entry = [&](const char* entry_name) -> void {
      static constexpr std::string_view errors_suffix = ".errors.txt"sv;

      std::string_view entry_name_view(entry_name);
      if (ends_with(entry_name_view, errors_suffix)) {
        entry_name_view.remove_suffix(errors_suffix.size());
        this->test_cases_expecting_error_.emplace(entry_name_view);
      }
    };
    Result<void, Platform_File_IO_Error> list = list_directory(
        concat(std::string_view(baselines_path), "/baselines/reference/"sv)
            .c_str(),
        std::move(visit_entry));
    if (!list.ok()) {
      return list.propagate();
    }
    return {};
  }

  std::optional<Expectations> get_test_case_expectations(
      const char* path) const {
    std::string_view test_name = path_file_name(path);
    for (std::string_view suffix : {".ts"sv, ".tsx"sv}) {
      if (ends_with(test_name, suffix)) {
        test_name.remove_suffix(suffix.size());
        return Expectations{
            // FIXME(strager): We shouldn't have to create an std::string here.
            .has_errors = this->test_cases_expecting_error_.contains(
                std::string(test_name)),
        };
      }
    }
    return std::nullopt;
  }

 private:
  Hash_Set<std::string> test_cases_expecting_error_;
};

bool should_ignore_test(String8_View path) {
  for (String8_View ignore_pattern : ignored_tests) {
    if (contains(path, ignore_pattern)) {
      return true;
    }
  }
  return false;
}

void process_test_case_file(Expected_Test_Results& expected_results,
                            const char* path) {
  String8_View path_view = to_string8_view(path);
  if (should_ignore_test(path_view)) {
    std::fprintf(stderr, "note: ignoring %s\n", path);
    return;
  }
  std::fprintf(stderr, "note: checking %s\n", path);
  std::optional<Expected_Test_Results::Expectations> expected =
      expected_results.get_test_case_expectations(path);
  if (!expected.has_value()) {
    std::fprintf(stderr,
                 "fatal: failed to load expectations for test case %s\n", path);
    std::exit(1);
  }

  Result<Padded_String, Read_File_IO_Error> raw_source = read_file(path);
  if (!raw_source.ok()) {
    std::fprintf(stderr, "fatal: failed to load file %s: %s\n", path,
                 raw_source.error_to_string().c_str());
    std::exit(1);
  }

  Global_Declared_Variable_Set globals;
  globals.add_literally_everything();

  Memory_Output_Stream diags;
  Text_Diag_Reporter text_reporter(Translator(), &diags,
                                   /*escape_errors=*/false);

  for (TypeScript_Test_Unit& unit :
       extract_units_from_typescript_test(std::move(*raw_source), path_view)) {
    std::optional<Linter_Options> options = unit.get_linter_options();
    if (options.has_value()) {
      // TODO(strager): Indicate which unit we are looking at.
      text_reporter.set_source(&unit.data, path);
      parse_and_lint(&unit.data, text_reporter, globals, *options);
    }
  }
  diags.flush();

  String8 diags_text = diags.get_flushed_string8();
  bool did_error = !diags_text.empty();
  bool should_error = expected->has_errors;
  if (did_error != should_error) {
    if (did_error) {
      std::fprintf(stderr, "fail: test case errored but should not have: %s\n",
                   path);
      std::fwrite(diags_text.data(), 1, diags_text.size(), stderr);
      std::exit(1);
    } else {
      // quick-lint-js probably failed to report a TypeScript type error. For
      // now, we ignore these false negatives.
      //
      // TODO(strager): Filter out type errors from TypeScript's .errors.txt and
      // compare.
    }
  }
}

void process_test_case_directory_or_file(
    Expected_Test_Results& expected_results, const char* path) {
  struct Test_Case_Visitor final : public List_Directory_Visitor {
    Expected_Test_Results& expected_results;
    const char* root_path;

    explicit Test_Case_Visitor(Expected_Test_Results& expected_results,
                               const char* root_path)
        : expected_results(expected_results), root_path(root_path) {}

    void visit_file(const std::string& file_path) override {
      if (ends_with(file_path, ".ts"sv) || ends_with(file_path, ".tsx"sv)) {
        process_test_case_file(this->expected_results, file_path.c_str());
      }
    }

    void on_error(const Platform_File_IO_Error& error, int depth) override {
      if (depth == 0 && error.is_not_a_directory_error()) {
        process_test_case_file(this->expected_results, this->root_path);
        return;
      }
      std::fprintf(stderr, "fatal: error while traversing directory: %s\n",
                   error.to_string().c_str());
      std::exit(1);
    }
  };
  Test_Case_Visitor visitor(expected_results, path);
  list_directory_recursively(path, visitor);
}

std::string find_typescript_tests_directory(const char* descendant_path) {
  Result<Canonical_Path_Result, Canonicalize_Path_IO_Error> path_result =
      canonicalize_path(descendant_path);
  if (!path_result.ok()) {
    std::fprintf(stderr, "fatal: failed to canonicalize: %s\n",
                 path_result.error_to_string().c_str());
    std::exit(1);
  }
  Canonical_Path path = std::move(*path_result).canonical();
  for (;;) {
    if (path_file_name(path.path()) == "tests") {
      return std::move(path).path();
    }
    if (!path.parent()) {
      std::fprintf(stderr,
                   "fatal: failed to find TypeScript tests dir by searching "
                   "parents of path %s\n",
                   descendant_path);
    }
  }
}
}
}

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  Test_TypeScript_Options o = parse_test_options(argc, argv);
  if (o.test_case_paths.empty()) {
    std::fprintf(stderr, "error: missing path to TypeScript test case\n");
    return 2;
  }

  Expected_Test_Results expected_results;
  Result<void, Platform_File_IO_Error> load =
      expected_results.load_from_tests_directory(
          find_typescript_tests_directory(o.test_case_paths[0]).c_str());
  if (!load.ok()) {
    std::fprintf(stderr, "fatal: failed to load baselines: %s\n",
                 load.error_to_string().c_str());
    std::exit(1);
  }

  for (const char* test_case_path : o.test_case_paths) {
    process_test_case_directory_or_file(expected_results, test_case_path);
  }

  return 0;
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
