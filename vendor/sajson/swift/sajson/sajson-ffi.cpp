#include "sajson-ffi.h"
#include "../../include/sajson.h"

// never instantiated, only inherits so static_cast is legal
struct sajson_document : sajson::document {};
struct sajson_value : sajson::value {};

namespace {

sajson::document* unwrap(sajson_document* doc) {
    return static_cast<sajson::document*>(doc);
}

sajson_document* wrap(sajson::document* doc) {
    return static_cast<sajson_document*>(doc);
}

template <typename T>
typename std::underlying_type<T>::type to_underlying(T value) {
    return static_cast<typename std::underlying_type<T>::type>(value);
}
}

sajson_document* sajson_parse_single_allocation(char* bytes, size_t length) {
    auto doc = sajson::parse(
        sajson::single_allocation(),
        sajson::mutable_string_view(length, bytes));
    return wrap(new (std::nothrow) sajson::document(std::move(doc)));
}

sajson_document* sajson_parse_dynamic_allocation(char* bytes, size_t length) {
    auto doc = sajson::parse(
        sajson::dynamic_allocation(),
        sajson::mutable_string_view(length, bytes));
    return wrap(new (std::nothrow) sajson::document(std::move(doc)));
}

void sajson_free_document(sajson_document* doc) { delete unwrap(doc); }

int sajson_has_error(sajson_document* doc) { return !unwrap(doc)->is_valid(); }

size_t sajson_get_error_line(sajson_document* doc) {
    return unwrap(doc)->get_error_line();
}

size_t sajson_get_error_column(sajson_document* doc) {
    return unwrap(doc)->get_error_column();
}

const char* sajson_get_error_message(sajson_document* doc) {
    return unwrap(doc)->get_error_message_as_cstring();
}

uint8_t sajson_get_root_tag(sajson_document* doc) {
    return to_underlying(unwrap(doc)->_internal_get_root_tag());
}

const size_t* sajson_get_root(sajson_document* doc) {
    return unwrap(doc)->_internal_get_root();
}

const unsigned char* sajson_get_input(sajson_document* doc) {
    return reinterpret_cast<const unsigned char*>(
        unwrap(doc)->_internal_get_input().get_data());
}

size_t sajson_get_input_length(struct sajson_document* doc) {
    return unwrap(doc)->_internal_get_input().length();
}

// MARK: -

/// HACK: This is a re-implemented version of a similar function on
/// sajson::value. We should modify it to either share a common helper, or
/// remove it completely and re-implement the search logic in Swift.
size_t sajson_find_object_key(
    const size_t* const payload,
    const char* key,
    size_t length,
    const unsigned char* input) {
    auto inputCasted = reinterpret_cast<const char* const>(input);
    sajson::string key_string = sajson::string(key, length);
    size_t value_length = payload[0];

    const sajson::internal::object_key_record* start
        = reinterpret_cast<const sajson::internal::object_key_record*>(
            payload + 1);
    const sajson::internal::object_key_record* end = start + value_length;
    const sajson::internal::object_key_record* i = std::lower_bound(
        start,
        end,
        key_string,
        sajson::internal::object_key_comparator(inputCasted));
    return (i != end && (i->key_end - i->key_start) == key_string.length()
            && memcmp(
                   key_string.data(), input + i->key_start, key_string.length())
                == 0)
        ? i - start
        : value_length;
}
