// included first to verify sajson includes.
#include <sajson.h>
#include <sajson_ostream.h>

#include <UnitTest++.h>

#include <random>

using sajson::document;
using sajson::literal;
using sajson::string;
using sajson::TYPE_ARRAY;
using sajson::TYPE_DOUBLE;
using sajson::TYPE_FALSE;
using sajson::TYPE_INTEGER;
using sajson::TYPE_NULL;
using sajson::TYPE_OBJECT;
using sajson::TYPE_STRING;
using sajson::TYPE_TRUE;
using sajson::value;

namespace {

inline bool success(const document& doc) {
    if (!doc.is_valid()) {
        fprintf(
            stderr,
            "parse failed at %i, %i: %s\n",
            static_cast<int>(doc.get_error_line()),
            static_cast<int>(doc.get_error_column()),
            doc.get_error_message_as_cstring());
        return false;
    }
    return true;
}

const size_t ast_buffer_size = 8096;
size_t ast_buffer[ast_buffer_size];

/**
 * Modern clang complains about obvious self-assignment, but we want
 * to do that in tests. Hide it from clang.
 */
template <typename T>
const T& self_ref(const T& v) {
    return v;
}
}

#define ABSTRACT_TEST(name)                                              \
    static void name##internal(                                          \
        sajson::document (*parse)(const sajson::string&));               \
    TEST(single_allocation_##name) {                                     \
        name##internal([](const sajson::string& literal) {               \
            return sajson::parse(sajson::single_allocation(), literal);  \
        });                                                              \
    }                                                                    \
    TEST(dynamic_allocation_##name) {                                    \
        name##internal([](const sajson::string& literal) {               \
            return sajson::parse(sajson::dynamic_allocation(), literal); \
        });                                                              \
    }                                                                    \
    TEST(bounded_allocation_##name) {                                    \
        name##internal([](const sajson::string& literal) {               \
            return sajson::parse(                                        \
                sajson::bounded_allocation(ast_buffer, ast_buffer_size), \
                literal);                                                \
        });                                                              \
    }                                                                    \
    static void name##internal(sajson::document (*parse)(const sajson::string&))

ABSTRACT_TEST(empty_array) {
    const sajson::document& document = parse(literal("[]"));
    assert(success(document));
    const value& root = document.get_root();
    CHECK_EQUAL(true, document.is_valid());
    CHECK_EQUAL(TYPE_ARRAY, root.get_type());
    CHECK_EQUAL(0u, root.get_length());
}

ABSTRACT_TEST(array_whitespace) {
    const sajson::document& document = parse(literal(" [ ] "));
    assert(success(document));
    const value& root = document.get_root();
    CHECK_EQUAL(TYPE_ARRAY, root.get_type());
    CHECK_EQUAL(0u, root.get_length());
}

ABSTRACT_TEST(array_zero) {
    const sajson::document& document = parse(literal("[0]"));
    assert(success(document));
    const value& root = document.get_root();
    CHECK_EQUAL(TYPE_ARRAY, root.get_type());
    CHECK_EQUAL(1u, root.get_length());

    const value& e0 = root.get_array_element(0);
    CHECK_EQUAL(TYPE_INTEGER, e0.get_type());
    CHECK_EQUAL(0, e0.get_number_value());
}

ABSTRACT_TEST(nested_array) {
    const sajson::document& document = parse(literal("[[]]"));
    assert(success(document));
    const value& root = document.get_root();
    CHECK_EQUAL(TYPE_ARRAY, root.get_type());
    CHECK_EQUAL(1u, root.get_length());

    const value& e1 = root.get_array_element(0);
    CHECK_EQUAL(TYPE_ARRAY, e1.get_type());
    CHECK_EQUAL(0u, e1.get_length());
}

ABSTRACT_TEST(packed_arrays) {
    const sajson::document& document = parse(literal("[0,[0,[0],0],0]"));
    assert(success(document));
    const value& root = document.get_root();
    CHECK_EQUAL(TYPE_ARRAY, root.get_type());
    CHECK_EQUAL(3u, root.get_length());

    const value& root0 = root.get_array_element(0);
    CHECK_EQUAL(TYPE_INTEGER, root0.get_type());
    CHECK_EQUAL(0, root0.get_number_value());

    const value& root2 = root.get_array_element(2);
    CHECK_EQUAL(TYPE_INTEGER, root2.get_type());
    CHECK_EQUAL(0, root2.get_number_value());

    const value& root1 = root.get_array_element(1);
    CHECK_EQUAL(TYPE_ARRAY, root1.get_type());
    CHECK_EQUAL(3u, root1.get_length());

    const value& sub0 = root1.get_array_element(0);
    CHECK_EQUAL(TYPE_INTEGER, sub0.get_type());
    CHECK_EQUAL(0, sub0.get_number_value());

    const value& sub2 = root1.get_array_element(2);
    CHECK_EQUAL(TYPE_INTEGER, sub2.get_type());
    CHECK_EQUAL(0, sub2.get_number_value());

    const value& sub1 = root1.get_array_element(1);
    CHECK_EQUAL(TYPE_ARRAY, sub1.get_type());
    CHECK_EQUAL(1u, sub1.get_length());

    const value& inner = sub1.get_array_element(0);
    CHECK_EQUAL(TYPE_INTEGER, inner.get_type());
    CHECK_EQUAL(0, inner.get_number_value());
}

ABSTRACT_TEST(deep_nesting) {
    const sajson::document& document = parse(literal("[[[[]]]]"));
    assert(success(document));
    const value& root = document.get_root();
    CHECK_EQUAL(TYPE_ARRAY, root.get_type());
    CHECK_EQUAL(1u, root.get_length());

    const value& e1 = root.get_array_element(0);
    CHECK_EQUAL(TYPE_ARRAY, e1.get_type());
    CHECK_EQUAL(1u, e1.get_length());

    const value& e2 = e1.get_array_element(0);
    CHECK_EQUAL(TYPE_ARRAY, e2.get_type());
    CHECK_EQUAL(1u, e2.get_length());

    const value& e3 = e2.get_array_element(0);
    CHECK_EQUAL(TYPE_ARRAY, e3.get_type());
    CHECK_EQUAL(0u, e3.get_length());
}

ABSTRACT_TEST(more_array_integer_packing) {
    const sajson::document& document = parse(literal("[[[[0]]]]"));
    assert(success(document));
    const value& root = document.get_root();
    CHECK_EQUAL(TYPE_ARRAY, root.get_type());
    CHECK_EQUAL(1u, root.get_length());

    const value& e1 = root.get_array_element(0);
    CHECK_EQUAL(TYPE_ARRAY, e1.get_type());
    CHECK_EQUAL(1u, e1.get_length());

    const value& e2 = e1.get_array_element(0);
    CHECK_EQUAL(TYPE_ARRAY, e2.get_type());
    CHECK_EQUAL(1u, e2.get_length());

    const value& e3 = e2.get_array_element(0);
    CHECK_EQUAL(TYPE_ARRAY, e3.get_type());
    CHECK_EQUAL(1u, e3.get_length());

    const value& e4 = e3.get_array_element(0);
    CHECK_EQUAL(TYPE_INTEGER, e4.get_type());
    CHECK_EQUAL(0, e4.get_integer_value());
}

SUITE(integers) {
    ABSTRACT_TEST(negative_and_positive_integers) {
        const sajson::document& document = parse(literal(" [ 0, -1, 22] "));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(3u, root.get_length());

        const value& e0 = root.get_array_element(0);
        CHECK_EQUAL(TYPE_INTEGER, e0.get_type());
        CHECK_EQUAL(0, e0.get_integer_value());
        CHECK_EQUAL(0, e0.get_number_value());

        const value& e1 = root.get_array_element(1);
        CHECK_EQUAL(TYPE_INTEGER, e1.get_type());
        CHECK_EQUAL(-1, e1.get_integer_value());
        CHECK_EQUAL(-1, e1.get_number_value());

        const value& e2 = root.get_array_element(2);
        CHECK_EQUAL(TYPE_INTEGER, e2.get_type());
        CHECK_EQUAL(22, e2.get_integer_value());
        CHECK_EQUAL(22, e2.get_number_value());
    }

    ABSTRACT_TEST(integers) {
        const sajson::document& document
            = parse(literal("[0,1,2,3,4,5,6,7,8,9,10]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(11u, root.get_length());

        for (int i = 0; i < 11; ++i) {
            const value& e = root.get_array_element(i);
            CHECK_EQUAL(TYPE_INTEGER, e.get_type());
            CHECK_EQUAL(i, e.get_integer_value());
        }
    }

    ABSTRACT_TEST(integer_whitespace) {
        const sajson::document& document = parse(literal(" [ 0 , 0 ] "));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(2u, root.get_length());
        const value& element = root.get_array_element(1);
        CHECK_EQUAL(TYPE_INTEGER, element.get_type());
        CHECK_EQUAL(0, element.get_integer_value());
    }

    ABSTRACT_TEST(leading_zeroes_disallowed) {
        const sajson::document& document = parse(literal("[01]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(3u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_EXPECTED_COMMA, document._internal_get_error_code());
    }

    ABSTRACT_TEST(exponent_overflow) {
        // https://github.com/chadaustin/sajson/issues/37
        const auto& document
            = parse(literal("[0e9999990066, 1e9999990066, 1e-9999990066]"));
        CHECK_EQUAL(true, document.is_valid());
        const auto& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(3u, root.get_length());

        const value& zero = root.get_array_element(0);
        CHECK_EQUAL(TYPE_DOUBLE, zero.get_type());
        CHECK_EQUAL(0.0, zero.get_double_value());

        const value& inf = root.get_array_element(1);
        CHECK_EQUAL(TYPE_DOUBLE, inf.get_type());
        CHECK_EQUAL(
            std::numeric_limits<double>::infinity(), inf.get_double_value());

        const value& zero2 = root.get_array_element(2);
        CHECK_EQUAL(TYPE_DOUBLE, zero2.get_type());
        CHECK_EQUAL(0.0, zero2.get_double_value());
    }
}

ABSTRACT_TEST(unit_types) {
    const sajson::document& document
        = parse(literal("[ true , false , null ]"));
    assert(success(document));
    const value& root = document.get_root();
    CHECK_EQUAL(TYPE_ARRAY, root.get_type());
    CHECK_EQUAL(3u, root.get_length());

    const value& e0 = root.get_array_element(0);
    CHECK_EQUAL(TYPE_TRUE, e0.get_type());

    const value& e1 = root.get_array_element(1);
    CHECK_EQUAL(TYPE_FALSE, e1.get_type());

    const value& e2 = root.get_array_element(2);
    CHECK_EQUAL(TYPE_NULL, e2.get_type());
}

SUITE(doubles) {
    ABSTRACT_TEST(doubles) {
        const sajson::document& document = parse(literal("[-0,-1,-34.25]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(3u, root.get_length());

        const value& e0 = root.get_array_element(0);
        CHECK_EQUAL(TYPE_INTEGER, e0.get_type());
        CHECK_EQUAL(-0, e0.get_integer_value());

        const value& e1 = root.get_array_element(1);
        CHECK_EQUAL(TYPE_INTEGER, e1.get_type());
        CHECK_EQUAL(-1, e1.get_integer_value());

        const value& e2 = root.get_array_element(2);
        CHECK_EQUAL(TYPE_DOUBLE, e2.get_type());
        CHECK_EQUAL(-34.25, e2.get_double_value());
    }

    ABSTRACT_TEST(large_number) {
        const auto& document = parse(literal("[1496756396000]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(1u, root.get_length());

        const value& element = root.get_array_element(0);
        CHECK_EQUAL(TYPE_DOUBLE, element.get_type());
        CHECK_EQUAL(1496756396000.0, element.get_double_value());

        int64_t out;
        CHECK_EQUAL(true, element.get_int53_value(&out));
        CHECK_EQUAL(1496756396000LL, out);
    }

    ABSTRACT_TEST(exponents) {
        const sajson::document& document
            = parse(literal("[2e+3,0.5E-5,10E+22]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(3u, root.get_length());

        const value& e0 = root.get_array_element(0);
        CHECK_EQUAL(TYPE_DOUBLE, e0.get_type());
        CHECK_EQUAL(2000, e0.get_double_value());

        const value& e1 = root.get_array_element(1);
        CHECK_EQUAL(TYPE_DOUBLE, e1.get_type());
        CHECK_CLOSE(0.000005, e1.get_double_value(), 1e-20);

        const value& e2 = root.get_array_element(2);
        CHECK_EQUAL(TYPE_DOUBLE, e2.get_type());
        CHECK_EQUAL(10e22, e2.get_double_value());
    }

    ABSTRACT_TEST(long_no_exponent) {
        const sajson::document& document
            = parse(literal("[9999999999,99999999999]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(2u, root.get_length());

        const value& e0 = root.get_array_element(0);
        CHECK_EQUAL(TYPE_DOUBLE, e0.get_type());
        CHECK_EQUAL(9999999999.0, e0.get_double_value());

        const value& e1 = root.get_array_element(1);
        CHECK_EQUAL(TYPE_DOUBLE, e1.get_type());
        CHECK_EQUAL(99999999999.0, e1.get_double_value());
    }

    ABSTRACT_TEST(exponent_offset) {
        const sajson::document& document = parse(literal("[0.005e3]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(1u, root.get_length());

        const value& e0 = root.get_array_element(0);
        CHECK_EQUAL(TYPE_DOUBLE, e0.get_type());
        CHECK_EQUAL(5.0, e0.get_double_value());
    }

    ABSTRACT_TEST(missing_exponent) {
        const sajson::document& document = parse(literal("[0e]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(4u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_MISSING_EXPONENT,
            document._internal_get_error_code());
    }

    ABSTRACT_TEST(missing_exponent_plus) {
        const sajson::document& document = parse(literal("[0e+]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(5u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_MISSING_EXPONENT,
            document._internal_get_error_code());
    }

    ABSTRACT_TEST(invalid_2_byte_utf8) {
        const auto& document = parse(literal("[\"\xdf\x7f\"]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(4u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_INVALID_UTF8, document._internal_get_error_code());
    }

    ABSTRACT_TEST(invalid_3_byte_utf8) {
        const auto& document = parse(literal("[\"\xef\x8f\x7f\"]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(5u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_INVALID_UTF8, document._internal_get_error_code());
    }

    ABSTRACT_TEST(invalid_4_byte_utf8) {
        const auto& document = parse(literal("[\"\xf7\x8f\x8f\x7f\"]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(6u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_INVALID_UTF8, document._internal_get_error_code());
    }

    ABSTRACT_TEST(invalid_utf8_prefix) {
        const auto& document = parse(literal("[\"\xff\"]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(3u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_INVALID_UTF8, document._internal_get_error_code());
    }
}

SUITE(int53) {
    ABSTRACT_TEST(int32) {
        const auto& document = parse(literal("[-54]"));
        assert(success(document));
        const value& root = document.get_root();
        const value& element = root.get_array_element(0);

        int64_t out;
        CHECK_EQUAL(true, element.get_int53_value(&out));
        CHECK_EQUAL(-54, out);
    }

    ABSTRACT_TEST(integer_double) {
        const auto& document = parse(literal("[10.0]"));
        assert(success(document));
        const value& root = document.get_root();
        const value& element = root.get_array_element(0);

        int64_t out;
        CHECK_EQUAL(true, element.get_int53_value(&out));
        CHECK_EQUAL(10, out);
    }

    ABSTRACT_TEST(non_integer_double) {
        const auto& document = parse(literal("[10.5]"));
        assert(success(document));
        const value& root = document.get_root();
        const value& element = root.get_array_element(0);
        CHECK_EQUAL(TYPE_DOUBLE, element.get_type());
        CHECK_EQUAL(10.5, element.get_double_value());

        int64_t out;
        CHECK_EQUAL(false, element.get_int53_value(&out));
    }

    ABSTRACT_TEST(endpoints) {
        // TODO: What should we do about (1<<53)+1?
        // When parsed into a double it loses that last bit of precision, so
        // sajson doesn't distinguish between 9007199254740992 and
        // 9007199254740993. So for now ignore this boundary condition and unit
        // test one extra value away.
        const auto& document
            = parse(literal("[-9007199254740992, 9007199254740992, "
                            "-9007199254740994, 9007199254740994]"));
        assert(success(document));
        const value& root = document.get_root();
        const value& e0 = root.get_array_element(0);
        const value& e1 = root.get_array_element(1);
        const value& e2 = root.get_array_element(2);
        const value& e3 = root.get_array_element(3);

        int64_t out;

        CHECK_EQUAL(true, e0.get_int53_value(&out));
        CHECK_EQUAL(-9007199254740992LL, out);

        CHECK_EQUAL(true, e1.get_int53_value(&out));
        CHECK_EQUAL(9007199254740992LL, out);

        CHECK_EQUAL(false, e2.get_int53_value(&out));
        CHECK_EQUAL(false, e3.get_int53_value(&out));
    }
}

SUITE(commas) {
    ABSTRACT_TEST(leading_comma_array) {
        const sajson::document& document = parse(literal("[,1]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(2u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_UNEXPECTED_COMMA,
            document._internal_get_error_code());
    }

    ABSTRACT_TEST(leading_comma_object) {
        const sajson::document& document = parse(literal("{,}"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(2u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_MISSING_OBJECT_KEY,
            document._internal_get_error_code());
    }

    ABSTRACT_TEST(trailing_comma_array) {
        const sajson::document& document = parse(literal("[1,2,]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(6u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_EXPECTED_VALUE, document._internal_get_error_code());
    }

    ABSTRACT_TEST(trailing_comma_object) {
        const sajson::document& document = parse(literal("{\"key\": 0,}"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(11u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_MISSING_OBJECT_KEY,
            document._internal_get_error_code());
    }
}

SUITE(strings) {
    ABSTRACT_TEST(strings) {
        const sajson::document& document = parse(literal("[\"\", \"foobar\"]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(2u, root.get_length());

        const value& e0 = root.get_array_element(0);
        CHECK_EQUAL(TYPE_STRING, e0.get_type());
        CHECK_EQUAL(0u, e0.get_string_length());
        CHECK_EQUAL("", e0.as_string());
        CHECK_EQUAL("", e0.as_cstring());

        const value& e1 = root.get_array_element(1);
        CHECK_EQUAL(TYPE_STRING, e1.get_type());
        CHECK_EQUAL(6u, e1.get_string_length());
        CHECK_EQUAL("foobar", e1.as_string());
        CHECK_EQUAL("foobar", e1.as_cstring());
    }

    ABSTRACT_TEST(common_escapes) {
        // \"\\\/\b\f\n\r\t
        const sajson::document& document
            = parse(literal("[\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(1u, root.get_length());

        const value& e0 = root.get_array_element(0);
        CHECK_EQUAL(TYPE_STRING, e0.get_type());
        CHECK_EQUAL(8u, e0.get_string_length());
        CHECK_EQUAL("\"\\/\b\f\n\r\t", e0.as_string());
        CHECK_EQUAL("\"\\/\b\f\n\r\t", e0.as_cstring());
    }

    ABSTRACT_TEST(escape_midstring) {
        const sajson::document& document = parse(literal("[\"foo\\tbar\"]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(1u, root.get_length());

        const value& e0 = root.get_array_element(0);
        CHECK_EQUAL(TYPE_STRING, e0.get_type());
        CHECK_EQUAL(7u, e0.get_string_length());
        CHECK_EQUAL("foo\tbar", e0.as_string());
        CHECK_EQUAL("foo\tbar", e0.as_cstring());
    }

    ABSTRACT_TEST(unfinished_string) {
        const sajson::document& document = parse(literal("[\""));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        // CHECK_EQUAL(3, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_UNEXPECTED_END, document._internal_get_error_code());
    }

    ABSTRACT_TEST(unfinished_escape) {
        const sajson::document& document = parse(literal("[\"\\"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        // CHECK_EQUAL(3, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_UNEXPECTED_END, document._internal_get_error_code());
    }

    ABSTRACT_TEST(unprintables_are_not_valid_in_strings) {
        const sajson::document& document = parse(literal("[\"\x19\"]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        // CHECK_EQUAL(3, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_ILLEGAL_CODEPOINT,
            document._internal_get_error_code());
        CHECK_EQUAL(25, document._internal_get_error_argument());
        CHECK_EQUAL(
            "illegal unprintable codepoint in string: 25",
            document.get_error_message_as_string());
    }

    ABSTRACT_TEST(unprintables_are_not_valid_in_strings_after_escapes) {
        const sajson::document& document = parse(literal("[\"\\n\x01\"]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(2u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_ILLEGAL_CODEPOINT,
            document._internal_get_error_code());
        CHECK_EQUAL(1, document._internal_get_error_argument());
        CHECK_EQUAL(
            "illegal unprintable codepoint in string: 1",
            document.get_error_message_as_string());
    }

    ABSTRACT_TEST(utf16_surrogate_pair) {
        const sajson::document& document
            = parse(literal("[\"\\ud950\\uDf21\"]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(1u, root.get_length());

        const value& e0 = root.get_array_element(0);
        CHECK_EQUAL(TYPE_STRING, e0.get_type());
        CHECK_EQUAL(4u, e0.get_string_length());
        CHECK_EQUAL("\xf1\xa4\x8c\xa1", e0.as_string());
        CHECK_EQUAL("\xf1\xa4\x8c\xa1", e0.as_cstring());
    }

    ABSTRACT_TEST(utf8_shifting) {
        const auto& document
            = parse(literal("[\"\\n\xc2\x80\xe0\xa0\x80\xf0\x90\x80\x80\"]"));
        assert(success(document));

        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(1u, root.get_length());

        const value& e0 = root.get_array_element(0);
        CHECK_EQUAL(TYPE_STRING, e0.get_type());
        CHECK_EQUAL(10u, e0.get_string_length());
        CHECK_EQUAL("\n\xc2\x80\xe0\xa0\x80\xf0\x90\x80\x80", e0.as_string());
        CHECK_EQUAL("\n\xc2\x80\xe0\xa0\x80\xf0\x90\x80\x80", e0.as_cstring());
    }
}

SUITE(objects) {
    ABSTRACT_TEST(empty_object) {
        const sajson::document& document = parse(literal("{}"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_OBJECT, root.get_type());
        CHECK_EQUAL(0u, root.get_length());
    }

    ABSTRACT_TEST(nested_object) {
        const sajson::document& document
            = parse(literal("{\"a\":{\"b\":{}}} "));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_OBJECT, root.get_type());
        CHECK_EQUAL(1u, root.get_length());

        const string& key = root.get_object_key(0);
        CHECK_EQUAL("a", key.data());
        CHECK_EQUAL("a", key.as_string());

        const value& element = root.get_object_value(0);
        CHECK_EQUAL(TYPE_OBJECT, element.get_type());
        CHECK_EQUAL("b", element.get_object_key(0).data());
        CHECK_EQUAL("b", element.get_object_key(0).as_string());

        const value& inner = element.get_object_value(0);
        CHECK_EQUAL(TYPE_OBJECT, inner.get_type());
        CHECK_EQUAL(0u, inner.get_length());
    }

    ABSTRACT_TEST(object_whitespace) {
        const sajson::document& document = parse(literal(" { \"a\" : 0 } "));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_OBJECT, root.get_type());
        CHECK_EQUAL(1u, root.get_length());

        const string& key = root.get_object_key(0);
        CHECK_EQUAL("a", key.data());
        CHECK_EQUAL("a", key.as_string());

        const value& element = root.get_object_value(0);
        CHECK_EQUAL(TYPE_INTEGER, element.get_type());
        CHECK_EQUAL(0, element.get_integer_value());
    }

    ABSTRACT_TEST(search_for_keys) {
        const sajson::document& document
            = parse(literal(" { \"b\" : 1 , \"aa\" : 0 } "));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_OBJECT, root.get_type());
        CHECK_EQUAL(2u, root.get_length());

        const size_t index_b = root.find_object_key(literal("b"));
        CHECK_EQUAL(0U, index_b);

        const size_t index_aa = root.find_object_key(literal("aa"));
        CHECK_EQUAL(1U, index_aa);

        const size_t index_c = root.find_object_key(literal("c"));
        CHECK_EQUAL(2U, index_c);

        const size_t index_ccc = root.find_object_key(literal("ccc"));
        CHECK_EQUAL(2U, index_ccc);
    }

    ABSTRACT_TEST(get_value) {
        const sajson::document& document
            = parse(literal(" { \"b\" : 123 , \"aa\" : 456 } "));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_OBJECT, root.get_type());
        CHECK_EQUAL(2u, root.get_length());

        const value& vb = root.get_value_of_key(literal("b"));
        CHECK_EQUAL(TYPE_INTEGER, vb.get_type());

        const value& vaa = root.get_value_of_key(literal("aa"));
        CHECK_EQUAL(TYPE_INTEGER, vaa.get_type());

        int ib = root.get_value_of_key(literal("b")).get_integer_value();
        CHECK_EQUAL(123, ib);

        int iaa = root.get_value_of_key(literal("aa")).get_integer_value();
        CHECK_EQUAL(456, iaa);
    }

    ABSTRACT_TEST(get_value_large_object) {
        unsigned values[1024];
        for (unsigned i = 0; i < 1024; ++i) {
            values[i] = i;
        }
        std::random_device rd;
        std::mt19937 g(rd());
        std::shuffle(std::begin(values), std::end(values), g);

        bool comma = false;
        std::string contents = "{";
        for (unsigned v : values) {
            contents += (comma ? ",\"" : "\"") + std::to_string(v)
                + "\":" + std::to_string(v);
            comma = true;
        }
        contents += "}";
        const sajson::document& document
            = parse(string(contents.data(), contents.size()));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_OBJECT, root.get_type());
        CHECK_EQUAL(1024u, root.get_length());

        const value& v56 = root.get_value_of_key(literal("56"));
        CHECK_EQUAL(TYPE_INTEGER, v56.get_type());
        CHECK_EQUAL(56, v56.get_integer_value());

        const value& vnone = root.get_value_of_key(literal("5.0"));
        CHECK_EQUAL(TYPE_NULL, vnone.get_type());
    }

    ABSTRACT_TEST(get_missing_value_returns_null) {
        const sajson::document& document = parse(literal("{\"a\": 123}"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_OBJECT, root.get_type());
        CHECK_EQUAL(1u, root.get_length());

        const value& vb = root.get_value_of_key(literal("b"));
        CHECK_EQUAL(TYPE_NULL, vb.get_type());
    }

    ABSTRACT_TEST(binary_search_handles_prefix_keys) {
        const sajson::document& document
            = parse(literal(" { \"prefix_key\" : 0 } "));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_OBJECT, root.get_type());
        CHECK_EQUAL(1u, root.get_length());

        const size_t index_prefix = root.find_object_key(literal("prefix"));
        CHECK_EQUAL(1U, index_prefix);
    }
}

SUITE(errors) {
    ABSTRACT_TEST(error_extension) {
        using namespace sajson;
        using namespace sajson::internal;

        CHECK_EQUAL(get_error_text(ERROR_NO_ERROR), "no error");
        CHECK_EQUAL(get_error_text(ERROR_OUT_OF_MEMORY), "out of memory");
        CHECK_EQUAL(
            get_error_text(ERROR_UNEXPECTED_END), "unexpected end of input");
        CHECK_EQUAL(
            get_error_text(ERROR_MISSING_ROOT_ELEMENT), "missing root element");
        CHECK_EQUAL(
            get_error_text(ERROR_BAD_ROOT),
            "document root must be object or array");
        CHECK_EQUAL(get_error_text(ERROR_EXPECTED_COMMA), "expected ,");
        CHECK_EQUAL(
            get_error_text(ERROR_MISSING_OBJECT_KEY), "missing object key");
        CHECK_EQUAL(get_error_text(ERROR_EXPECTED_COLON), "expected :");
        CHECK_EQUAL(
            get_error_text(ERROR_EXPECTED_END_OF_INPUT),
            "expected end of input");
        CHECK_EQUAL(get_error_text(ERROR_UNEXPECTED_COMMA), "unexpected comma");
        CHECK_EQUAL(get_error_text(ERROR_EXPECTED_VALUE), "expected value");
        CHECK_EQUAL(get_error_text(ERROR_EXPECTED_NULL), "expected 'null'");
        CHECK_EQUAL(get_error_text(ERROR_EXPECTED_FALSE), "expected 'false'");
        CHECK_EQUAL(get_error_text(ERROR_EXPECTED_TRUE), "expected 'true'");
        CHECK_EQUAL(get_error_text(ERROR_MISSING_EXPONENT), "missing exponent");
        CHECK_EQUAL(
            get_error_text(ERROR_ILLEGAL_CODEPOINT),
            "illegal unprintable codepoint in string");
        CHECK_EQUAL(
            get_error_text(ERROR_INVALID_UNICODE_ESCAPE),
            "invalid character in unicode escape");
        CHECK_EQUAL(
            get_error_text(ERROR_UNEXPECTED_END_OF_UTF16),
            "unexpected end of input during UTF-16 surrogate pair");
        CHECK_EQUAL(get_error_text(ERROR_EXPECTED_U), "expected \\u");
        CHECK_EQUAL(
            get_error_text(ERROR_INVALID_UTF16_TRAIL_SURROGATE),
            "invalid UTF-16 trail surrogate");
        CHECK_EQUAL(get_error_text(ERROR_UNKNOWN_ESCAPE), "unknown escape");
        CHECK_EQUAL(get_error_text(ERROR_INVALID_UTF8), "invalid UTF-8");
    }

    ABSTRACT_TEST(empty_file_is_invalid) {
        const sajson::document& document = parse(literal(""));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(1u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_MISSING_ROOT_ELEMENT,
            document._internal_get_error_code());
    }

    ABSTRACT_TEST(two_roots_are_invalid) {
        const sajson::document& document = parse(literal("[][]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        // CHECK_EQUAL(3, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_EXPECTED_END_OF_INPUT,
            document._internal_get_error_code());
    }

    ABSTRACT_TEST(root_must_be_object_or_array) {
        const sajson::document& document = parse(literal("0"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(1u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_BAD_ROOT, document._internal_get_error_code());
    }

    ABSTRACT_TEST(incomplete_object_key) {
        const sajson::document& document = parse(literal("{\"\\:0}"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(4u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_UNKNOWN_ESCAPE, document._internal_get_error_code());
    }

    ABSTRACT_TEST(commas_are_necessary_between_elements) {
        const sajson::document& document = parse(literal("[0 0]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        // CHECK_EQUAL(3, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_EXPECTED_COMMA, document._internal_get_error_code());
    }

    ABSTRACT_TEST(keys_must_be_strings) {
        const sajson::document& document = parse(literal("{0:0}"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(2u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_MISSING_OBJECT_KEY,
            document._internal_get_error_code());
    }

    ABSTRACT_TEST(objects_must_have_keys) {
        const sajson::document& document = parse(literal("{\"0\"}"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(5u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_EXPECTED_COLON, document._internal_get_error_code());
    }

    ABSTRACT_TEST(too_many_commas) {
        const sajson::document& document = parse(literal("[1,,2]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(4u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_UNEXPECTED_COMMA,
            document._internal_get_error_code());
    }

    ABSTRACT_TEST(object_missing_value) {
        const sajson::document& document = parse(literal("{\"x\":}"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(6u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_EXPECTED_VALUE, document._internal_get_error_code());
    }

    ABSTRACT_TEST(invalid_true_literal) {
        const sajson::document& document = parse(literal("[truf"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        // CHECK_EQUAL(3, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_EXPECTED_TRUE, document._internal_get_error_code());
    }

    ABSTRACT_TEST(incomplete_true_literal) {
        const sajson::document& document = parse(literal("[tru"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        // CHECK_EQUAL(3, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_UNEXPECTED_END, document._internal_get_error_code());
    }

    ABSTRACT_TEST(must_close_array_with_square_bracket) {
        const sajson::document& document = parse(literal("[}"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        // CHECK_EQUAL(3, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_EXPECTED_VALUE, document._internal_get_error_code());
    }

    ABSTRACT_TEST(must_close_object_with_curly_brace) {
        const sajson::document& document = parse(literal("{]"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(2u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_MISSING_OBJECT_KEY,
            document._internal_get_error_code());
    }

    ABSTRACT_TEST(incomplete_array_with_vero) {
        const sajson::document& document = parse(literal("[0"));
        CHECK_EQUAL(false, document.is_valid());
        CHECK_EQUAL(1u, document.get_error_line());
        CHECK_EQUAL(3u, document.get_error_column());
        CHECK_EQUAL(
            sajson::ERROR_UNEXPECTED_END, document._internal_get_error_code());
    }

#define CHECK_PARSE_ERROR(text, code)                                   \
    do {                                                                \
        const sajson::document& document = parse(literal(text));        \
        CHECK_EQUAL(false, document.is_valid());                        \
        CHECK_EQUAL(sajson::code, document._internal_get_error_code()); \
    } while (0)

    ABSTRACT_TEST(eof_after_number) {
        CHECK_PARSE_ERROR("[-", ERROR_UNEXPECTED_END);
        CHECK_PARSE_ERROR("[-12", ERROR_UNEXPECTED_END);
        CHECK_PARSE_ERROR("[-12.", ERROR_UNEXPECTED_END);
        CHECK_PARSE_ERROR("[-12.3", ERROR_UNEXPECTED_END);
        CHECK_PARSE_ERROR("[-12e", ERROR_UNEXPECTED_END);
        CHECK_PARSE_ERROR("[-12e-", ERROR_UNEXPECTED_END);
        CHECK_PARSE_ERROR("[-12e+", ERROR_UNEXPECTED_END);
        CHECK_PARSE_ERROR("[-12e3", ERROR_UNEXPECTED_END);
    }

    ABSTRACT_TEST(invalid_number) {
        CHECK_PARSE_ERROR("[-]", ERROR_INVALID_NUMBER);
        CHECK_PARSE_ERROR("[-12.]", ERROR_INVALID_NUMBER);
        CHECK_PARSE_ERROR("[-12e]", ERROR_MISSING_EXPONENT);
        CHECK_PARSE_ERROR("[-12e-]", ERROR_MISSING_EXPONENT);
        CHECK_PARSE_ERROR("[-12e+]", ERROR_MISSING_EXPONENT);

        // from https://github.com/chadaustin/sajson/issues/31
        CHECK_PARSE_ERROR("[-]", ERROR_INVALID_NUMBER);
        CHECK_PARSE_ERROR("[-2.]", ERROR_INVALID_NUMBER);
        CHECK_PARSE_ERROR("[0.e1]", ERROR_INVALID_NUMBER);
        CHECK_PARSE_ERROR("[2.e+3]", ERROR_INVALID_NUMBER);
        CHECK_PARSE_ERROR("[2.e-3]", ERROR_INVALID_NUMBER);
        CHECK_PARSE_ERROR("[2.e3]", ERROR_INVALID_NUMBER);
        CHECK_PARSE_ERROR("[-.123]", ERROR_INVALID_NUMBER);
        CHECK_PARSE_ERROR("[1.]", ERROR_INVALID_NUMBER);
    }
}

ABSTRACT_TEST(object_array_with_integers) {
    const sajson::document& document
        = parse(literal("[{ \"a\": 123456 }, { \"a\": 7890 }]"));
    assert(success(document));
    const value& root = document.get_root();
    CHECK_EQUAL(TYPE_ARRAY, root.get_type());
    CHECK_EQUAL(2u, root.get_length());

    const value& e1 = root.get_array_element(0);
    CHECK_EQUAL(TYPE_OBJECT, e1.get_type());
    size_t index_a = e1.find_object_key(literal("a"));
    const value& node = e1.get_object_value(index_a);
    CHECK_EQUAL(TYPE_INTEGER, node.get_type());
    CHECK_EQUAL(123456U, node.get_number_value());
    const value& e2 = root.get_array_element(1);
    CHECK_EQUAL(TYPE_OBJECT, e2.get_type());
    index_a = e2.find_object_key(literal("a"));
    const value& node2 = e2.get_object_value(index_a);
    CHECK_EQUAL(7890U, node2.get_number_value());
}

SUITE(api) {
    TEST(mutable_string_view_assignment) {
        sajson::mutable_string_view one(sajson::literal("hello"));
        sajson::mutable_string_view two;
        two = one;

        CHECK_EQUAL(5u, one.length());
        CHECK_EQUAL(5u, two.length());
    }

    TEST(mutable_string_view_self_assignment) {
        sajson::mutable_string_view one(sajson::literal("hello"));
        one = self_ref(one);
        CHECK_EQUAL(5u, one.length());
    }

    static sajson::mutable_string_view&& my_move(
        sajson::mutable_string_view & that) {
        return std::move(that);
    }

    TEST(mutable_string_view_self_move_assignment) {
        sajson::mutable_string_view one(sajson::literal("hello"));
        one = my_move(one);
        CHECK_EQUAL(5u, one.length());
    }
}

SUITE(allocator_tests) {
    TEST(single_allocation_into_existing_memory) {
        size_t buffer[2];
        const sajson::document& document
            = sajson::parse(sajson::single_allocation(buffer), literal("[]"));
        assert(success(document));
        const value& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(0u, root.get_length());
        CHECK_EQUAL(0u, buffer[1]);
    }

    TEST(bounded_allocation_size_just_right) {
        // This is awkward: the bounded allocator needs more memory in the worst
        // case than the single-allocation allocator.  That's because sajson's
        // AST construction algorithm briefly results in overlapping stack
        // and AST memory ranges, but it works because install_array and
        // install_object are careful to shift back-to-front.  However,
        // the bounded allocator disallows any overlapping ranges.
        size_t buffer[5];
        const auto& document = sajson::parse(
            sajson::bounded_allocation(buffer), literal("[[]]"));
        assert(success(document));
        const auto& root = document.get_root();
        CHECK_EQUAL(TYPE_ARRAY, root.get_type());
        CHECK_EQUAL(1u, root.get_length());
        const auto& element = root.get_array_element(0);
        CHECK_EQUAL(TYPE_ARRAY, element.get_type());
        CHECK_EQUAL(0u, element.get_length());
    }

    TEST(bounded_allocation_size_too_small) {
        // This is awkward: the bounded allocator needs more memory in the worst
        // case than the single-allocation allocator.  That's because sajson's
        // AST construction algorithm briefly results in overlapping stack
        // and AST memory ranges, but it works because install_array and
        // install_object are careful to shift back-to-front.  However,
        // the bounded allocator disallows any overlapping ranges.
        size_t buffer[4];
        const auto& document = sajson::parse(
            sajson::bounded_allocation(buffer), literal("[[]]"));
        CHECK(!document.is_valid());
        CHECK_EQUAL(
            sajson::ERROR_OUT_OF_MEMORY, document._internal_get_error_code());
    }
}

TEST(zero_initialized_document_is_invalid) {
    auto d = document{};
    CHECK(!d.is_valid());
    CHECK_EQUAL(0u, d.get_error_line());
    CHECK_EQUAL(0u, d.get_error_column());
    CHECK_EQUAL("uninitialized document", d.get_error_message_as_string());
}

TEST(zero_initialized_value_is_null) {
    auto v = value{};
    CHECK_EQUAL(TYPE_NULL, v.get_type());
}

TEST(value_is_copyable) {
    auto v = value{};
    auto u = v;
    CHECK_EQUAL(TYPE_NULL, u.get_type());
}

int main() { return UnitTest::RunAllTests(); }
