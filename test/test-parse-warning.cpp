// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/array.h>
#include <quick-lint-js/cli/cli-location.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/parse.h>
#include <quick-lint-js/parse-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/spy-visitor.h>
#include <string>
#include <string_view>
#include <vector>

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;
using namespace std::literals::string_literals;

namespace quick_lint_js
{
  namespace
  {
    class test_parse_warning : public test_parse_expression
    {
    };
    // TODO(strager): Move test_error_equals_does_not_distribute_over_or tests into
    // their own test file.
    class test_error_equals_does_not_distribute_over_or
        : public test_parse_expression
    {
    };

    TEST_F(test_parse_warning, condition_with_assignment_from_literal)
    {
      {
        test_parser p(u8"if (x = 42) {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_OFFSETS(
                            p.code, diag_assignment_makes_condition_constant, //
                            assignment_operator, strlen(u8"if (x "), u8"="_sv),
                    }));
      }

      {
        test_parser p(u8"if (o.prop = 'hello') {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_OFFSETS(
                            p.code, diag_assignment_makes_condition_constant, //
                            assignment_operator, strlen(u8"if (o.prop "), u8"="_sv),
                    }));
      }

      for (string8_view code : {
               u8"while (x = 'hello') {}"_sv,
               u8"for (; x = 'hello'; ) {}"_sv,
               u8"do {} while (x = 'hello');"_sv,
           })
      {
        SCOPED_TRACE(out_string8(code));
        test_parser p(code, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE(diag_assignment_makes_condition_constant),
                    }));
      }
    }

    TEST_F(test_parse_warning, non_condition_with_assignment_from_literal)
    {
      for (string8_view code : {
               u8"with (x = 'hello') {}"_sv,
               u8"for (x = 'hello'; ; ) {}"_sv,
               u8"for (; ; x = 'hello') {}"_sv,
               u8"switch (x = 'hello') {}"_sv,
           })
      {
        SCOPED_TRACE(out_string8(code));
        test_parser p(code, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    TEST_F(test_parse_warning,
           condition_with_assignment_from_literal_with_parentheses)
    {
      {
        test_parser p(u8"if ((x = 42)) {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    TEST_F(test_parse_warning, condition_with_updating_assignment_from_literal)
    {
      {
        test_parser p(u8"if (x += 42) {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    TEST_F(test_parse_warning, condition_with_assignment_from_non_literal)
    {
      {
        test_parser p(u8"if (x = y) {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.variable_assignments, ElementsAreArray({u8"x"}));
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    TEST_F(test_error_equals_does_not_distribute_over_or, examples)
    {
      {
        test_parser p(u8"if (x === 'A' || 'B') {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_2_OFFSETS(
                            p.code, diag_equals_does_not_distribute_over_or, //
                            or_operator, strlen(u8"if (x === 'A' "), u8"||"_sv,
                            equals_operator, strlen(u8"if (x "), u8"==="_sv),
                    }));
      }

      {
        test_parser p(u8"if (x === 10 || 0) {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_2_OFFSETS(
                            p.code, diag_equals_does_not_distribute_over_or, //
                            or_operator, strlen(u8"if (x === 10 "), u8"||"_sv,
                            equals_operator, strlen(u8"if (x "), u8"==="_sv),
                    }));
      }

      {
        test_parser p(u8"if (x == 'A' || 'B') {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_2_OFFSETS(
                            p.code, diag_equals_does_not_distribute_over_or, //
                            or_operator, strlen(u8"if (x == 'A' "), u8"||"_sv,
                            equals_operator, strlen(u8"if (x "), u8"=="_sv),
                    }));
      }
    }

    TEST_F(test_error_equals_does_not_distribute_over_or, not_equals)
    {
      {
        test_parser p(u8"if (x != 'A' || 'B') {}"_sv, capture_diags);
        p.parse_and_visit_module();
        EXPECT_THAT(p.errors, IsEmpty());
      }

      {
        test_parser p(u8"if (x !== 'A' || 'B') {}"_sv, capture_diags);
        p.parse_and_visit_module();
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    TEST_F(test_error_equals_does_not_distribute_over_or, null_and_undefined)
    {
      {
        test_parser p(u8"if (x == 'A' || null) {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x"}));
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_2_OFFSETS(
                            p.code, diag_equals_does_not_distribute_over_or, //
                            or_operator, strlen(u8"if (x == 'A' "), u8"||"_sv,
                            equals_operator, strlen(u8"if (x "), u8"=="_sv),
                    }));
      }

      {
        test_parser p(u8"if (x == 'A' || undefined) {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.variable_uses, ElementsAreArray({u8"x", u8"undefined"}));
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_2_OFFSETS(
                            p.code, diag_equals_does_not_distribute_over_or, //
                            or_operator, strlen(u8"if (x == 'A' "), u8"||"_sv,
                            equals_operator, strlen(u8"if (x "), u8"=="_sv),
                    }));
      }

      {
        test_parser p(u8"if (x === 10 || null) {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_2_OFFSETS(
                            p.code, diag_equals_does_not_distribute_over_or, //
                            or_operator, strlen(u8"if (x === 10 "), u8"||"_sv,
                            equals_operator, strlen(u8"if (x "), u8"==="_sv),
                    }));
      }

      {
        test_parser p(u8"if (x === 10 || undefined) {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_2_OFFSETS(
                            p.code, diag_equals_does_not_distribute_over_or, //
                            or_operator, strlen(u8"if (x === 10 "), u8"||"_sv,
                            equals_operator, strlen(u8"if (x "), u8"==="_sv),
                    }));
      }
    }

    TEST_F(test_error_equals_does_not_distribute_over_or, logical_and)
    {
      {
        test_parser p(u8"if (x == 'A' && 'B') {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    TEST_F(test_error_equals_does_not_distribute_over_or, non_constant)
    {
      {
        test_parser p(u8"if (x === 'A' || y) {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    TEST_F(test_parse_warning, warn_on_pointless_string_compare)
    {
      {
        test_parser p(u8"s.toLowerCase() == 'banana'"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
      {
        test_parser p(u8"s.toLowerCase() == 'BANANA'"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_OFFSETS(
                            p.code, diag_pointless_string_comp_contains_upper,
                            span_operator, strlen(u8"s.toLowerCase() "), u8"=="_sv),
                    }));
      }
      {
        test_parser p(u8"s.toUpperCase() == 'BANANA'"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
      {
        test_parser p(u8"s.toUpperCase() == 'banana'"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_OFFSETS(
                            p.code, diag_pointless_string_comp_contains_lower,
                            span_operator, strlen(u8"s.toUpperCase() "), u8"=="_sv),
                    }));
      }
      {
        test_parser p(u8"s.toLowerCase() == \"BANANA\""_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors,
                    ElementsAreArray({
                        DIAG_TYPE_OFFSETS(
                            p.code, diag_pointless_string_comp_contains_upper,
                            span_operator, strlen(u8"s.toLowerCase() "), u8"=="_sv),
                    }));
      }
    }

    TEST_F(test_parse_warning, warn_on_pointless_string_compare_all_operators)
    {
      {
        for (string8_view op : {u8"=="_sv, u8"==="_sv, u8"!="_sv, u8"!=="_sv})
        {
          test_parser p(concat(u8"s.toLowerCase() "_sv, op, u8" 'Banana'"_sv),
                        capture_diags);
          p.parse_and_visit_statement();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code, diag_pointless_string_comp_contains_upper,
                      span_operator, strlen(u8"s.toLowerCase() "), string8(op)),
              }));
        }
      }
      {
        test_parser p(u8"s.toLowerCase() || 'BANANA'"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    TEST_F(test_parse_warning,
           warn_on_pointless_string_compare_function_signatures)
    {
      {
        test_parser p(u8"s.tolowercase() == 'BANANA'"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
      {
        test_parser p(u8"s.myToLowerCase() == 'BANANA'"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
      {
        test_parser p(u8"stringBuilder.build().toLowerCase() == 'BANANA'"_sv,
                      capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(p.code, diag_pointless_string_comp_contains_upper,
                                  span_operator,
                                  strlen(u8"stringBuilder.build().toLowerCase() "),
                                  u8"=="_sv),
            }));
      }
      {
        test_parser p(u8"o.arr[0]() == 'BANANA'"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
      {
        test_parser p(u8"'BANANA' == s.toLowerCase()"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(p.code, diag_pointless_string_comp_contains_upper,
                                  span_operator, strlen(u8"'BANANA' "), u8"=="_sv),
            }));
      }
    }

    TEST_F(test_parse_warning,
           warn_on_comma_between_member_array_subscript_operators)
    {
      {
        test_parser p(u8"a[1, 2, 3]"_sv, capture_diags);
        p.parse_and_visit_expression();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_2_OFFSETS(
                    p.code, diag_misleading_comma_operator_in_index_operation,
                    comma, strlen(u8"a[1, 2"), u8","_sv, left_square, strlen(u8"a"),
                    u8"["_sv),
            }));
      }

      {
        test_parser p(u8"a[pow(1, 2), 2]"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_2_OFFSETS(
                    p.code, diag_misleading_comma_operator_in_index_operation,
                    comma, strlen(u8"a[pow(1, 2)"), u8","_sv, left_square,
                    strlen(u8"a"), u8"["_sv),
            }));
      }

      {
        test_parser p(u8"a[b[1967, 1975]]"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_2_OFFSETS(
                    p.code, diag_misleading_comma_operator_in_index_operation,
                    comma, strlen(u8"a[b[1967"), u8","_sv, left_square,
                    strlen(u8"a[b"), u8"["_sv),
            }));
      }

      {
        test_parser p(u8"a = [1, 2, 3]"_sv, capture_diags);
        p.parse_and_visit_expression();
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    TEST_F(test_parse_warning, warn_on_comma_operator_in_conditional_statement)
    {
      {
        test_parser p(u8"if(false, true){}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(
                    p.code, diag_misleading_comma_operator_in_conditional_statement,
                    comma, strlen(u8"if(false"), u8","_sv),
            }));
      }

      {
        test_parser p(u8"do{i++}while(i < 0, true)"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(
                    p.code, diag_misleading_comma_operator_in_conditional_statement,
                    comma, strlen(u8"do{i++}while(i < 0"), u8","_sv),
            }));
      }

      {
        test_parser p(u8"do{i++}while(i < (0, true))"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }

      {
        test_parser p(u8"for(; i < 5, i < 3; ){}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(
                    p.code, diag_misleading_comma_operator_in_conditional_statement,
                    comma, strlen(u8"for(; i < 5"), u8","_sv),
            }));
      }

      {
        test_parser p(u8"for(let i = 0, j = 0;;){}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }

      {
        test_parser p(u8"for(i = 0, j = 0;;){}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }

      {
        test_parser p(u8"for(;; ++i, ++j){}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }

      {
        test_parser p(u8"switch(cond1, cond2){case 1:break;}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(
                    p.code, diag_misleading_comma_operator_in_conditional_statement,
                    comma, strlen(u8"switch(cond1"), u8","_sv),
            }));
      }
    }

    TEST_F(test_parse_warning,
           warn_on_pointless_string_compare_complex_expressions)
    {
      {
        test_parser p(u8"if(s.toLowerCase() === 'BANANA') {}"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(p.code, diag_pointless_string_comp_contains_upper,
                                  span_operator, strlen(u8"if(s.toLowerCase() "),
                                  u8"==="_sv),
            }));
      }
      {
        test_parser p(u8"((s.toLowerCase())) === 'BANANA'"_sv, capture_diags);
        p.parse_and_visit_expression();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(p.code, diag_pointless_string_comp_contains_upper,
                                  span_operator, strlen(u8"((s.toLowerCase())) "),
                                  u8"==="_sv),
            }));
      }
      {
        test_parser p(u8"(((s.toLowerCase())) === ((('BANANA'))))"_sv,
                      capture_diags);
        p.parse_and_visit_expression();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(p.code, diag_pointless_string_comp_contains_upper,
                                  span_operator, strlen(u8"(((s.toLowerCase())) "),
                                  u8"==="_sv),
            }));
      }
      {
        test_parser p(
            u8"s.toLowerCase() == 'BANANA' && s.toUpperCase() !== 'orange'"_sv,
            capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(p.code, diag_pointless_string_comp_contains_upper,
                                  span_operator, strlen(u8"s.toLowerCase() "),
                                  u8"=="_sv),
                DIAG_TYPE_OFFSETS(
                    p.code, diag_pointless_string_comp_contains_lower,
                    span_operator,
                    strlen(u8"s.tolowerCASE() == 'BANANA' && s.toUpperCase() "),
                    u8"!=="_sv),
            }));
      }
      {
        test_parser p(
            u8"((s.toLowerCase() == 'BANANA') && s.toUpperCase() !== 'orange')"_sv,
            capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(p.code, diag_pointless_string_comp_contains_upper,
                                  span_operator, strlen(u8"((s.toLowerCase() "),
                                  u8"=="_sv),
                DIAG_TYPE_OFFSETS(
                    p.code, diag_pointless_string_comp_contains_lower,
                    span_operator,
                    strlen(u8"((s.toLowerCASE() == 'BANANA') && s.toUpperCase() "),
                    u8"!=="_sv),
            }));
      }
    }

    TEST_F(test_parse_warning, warn_on_pointless_string_compare_literals)
    {
      {
        test_parser p(u8"s.toLowerCase() == 'Ba\\u006Eana'"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
      {
        test_parser p(u8"s.toLowerCase() == 0xeF || s.toUpperCase() == 0xeF"_sv,
                      capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
      {
        test_parser p(u8"s.toUpperCase() == 'C:\\User\\tom'"_sv, capture_diags);
        p.parse_and_visit_statement();
        EXPECT_THAT(p.errors, IsEmpty());
      }
    }

    TEST_F(test_parse_warning,
           warn_on_pointless_strict_compare_against_array_literals)
    {
      for (string8 op : {u8"===", u8"!=="})
      {
        {
          test_parser p(concat(u8"x "_sv, op, u8" []"_sv), capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code,
                      diag_pointless_strict_comp_against_empty_array_literal,
                      equals_operator, strlen(u8"x "), op),
              }));
        }
        {
          test_parser p(concat(u8"x "_sv, op, u8" [1, 2, 3]"_sv), capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code, diag_pointless_strict_comp_against_array_literal,
                      equals_operator, strlen(u8"x "), op),
              }));
        }
      }
    }

    TEST_F(test_parse_warning, warn_on_pointless_compare_against_literals)
    {
      for (string8 op : {u8"==", u8"!=", u8"===", u8"!=="})
      {
        {
          test_parser p(concat(u8"x "_sv, op, u8" {}"_sv), capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(p.errors,
                      ElementsAreArray({
                          DIAG_TYPE_OFFSETS(
                              p.code, diag_pointless_comp_against_object_literal,
                              equals_operator, strlen(u8"x "), op),
                      }));
        }
        {
          test_parser p(concat(u8"x "_sv, op, u8" class C{}"_sv), capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(p.errors,
                      ElementsAreArray({
                          DIAG_TYPE_OFFSETS(
                              p.code, diag_pointless_comp_against_class_literal,
                              equals_operator, strlen(u8"x "), op),
                      }));
        }
        {
          test_parser p(
              concat(u8"x "_sv, op,
                     u8" ((parameter) => { some_object.call(parameter); })"_sv),
              capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(p.errors,
                      ElementsAreArray({
                          DIAG_TYPE_OFFSETS(
                              p.code, diag_pointless_comp_against_arrow_function,
                              equals_operator, strlen(u8"x "), op),
                      }));
        }
        {
          test_parser p(concat(u8"x "_sv, op, u8" /some_pattern/a"_sv),
                        capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code,
                      diag_pointless_comp_against_regular_expression_literal,
                      equals_operator, strlen(u8"x "), op),
              }));
        }
      }
    }

    TEST_F(test_parse_warning,
           warn_on_pointless_compare_against_literals_complex_expressions)
    {
      {
        test_parser p(
            u8"({} == {} && (x) === [1, 2, 3]) || ((/pattern/) == y.prop)"_sv,
            capture_diags);
        p.parse_and_visit_expression();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(p.code,
                                  diag_pointless_comp_against_object_literal,
                                  equals_operator, strlen(u8"({} "), u8"=="_sv),
                DIAG_TYPE_OFFSETS(
                    p.code, diag_pointless_strict_comp_against_array_literal,
                    equals_operator, strlen(u8"({} == {} && (x) "), u8"==="_sv),
                DIAG_TYPE_OFFSETS(
                    p.code, diag_pointless_comp_against_regular_expression_literal,
                    equals_operator,
                    strlen(u8"({} == {} && (x) === [1, 2, 3]) || ((/pattern/) "),
                    u8"=="_sv),
            }));
      }
      {
        test_parser p(u8"x === y || ({}) != obj.prop"_sv, capture_diags);
        p.parse_and_visit_expression();
        EXPECT_THAT(
            p.errors,
            ElementsAreArray({
                DIAG_TYPE_OFFSETS(
                    p.code, diag_pointless_comp_against_object_literal,
                    equals_operator, strlen(u8"x === y || ({}) "), u8"!="_sv),
            }));
      }

      TEST_F(test_parse_warning, warn_on_mistyped_strict_inequality_operator)
      {
        {
          test_parser p(u8"x! == y"_sv, capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code, diag_mistyped_strict_inequality_operator,
                      equals_operator, strlen(u8"x! "), u8"=="_sv),
              }));
        }
        {
          test_parser p(u8"'hello'! == 'world'"_sv, capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code, diag_mistyped_strict_inequality_operator,
                      equals_operator, strlen(u8"'hello'! "), u8"=="_sv),
              }));
        }
        {
          test_parser p(u8"True! == False"_sv, capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code, diag_mistyped_strict_inequality_operator,
                      equals_operator, strlen(u8"True! "), u8"=="_sv),
              }));
        }
        {
          test_parser p(u8"(x! == y) == z"_sv, capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code, diag_mistyped_strict_inequality_operator,
                      equals_operator, strlen(u8"(x! "), u8"=="_sv),
              }));
        }
        {
          test_parser p(u8"x! == (y == z)"_sv, capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code, diag_mistyped_strict_inequality_operator,
                      equals_operator, strlen(u8"x! "), u8"=="_sv),
              }));
        }
        {
          test_parser p(u8"if (length + 1! == constraints.getMaxLength()) {}"_sv,
                        capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code, diag_mistyped_strict_inequality_operator,
                      equals_operator, strlen(u8"if (length + 1! "), u8"=="_sv),
              }));
        }
        {
          test_parser p(u8"if (typeof diagnostic.code! == 'undefined') {}"_sv,
                        capture_diags);
          p.parse_and_visit_expression();
          EXPECT_THAT(
              p.errors,
              ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      p.code, diag_mistyped_strict_inequality_operator,
                      equals_operator, strlen(u8"if (typeof diagnostic.code! "),
                      u8"=="_sv),
              }));
        }
      }
    }
  }
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
