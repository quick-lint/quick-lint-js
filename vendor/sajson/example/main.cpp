#include "sajson.h"
#include <assert.h>

using namespace sajson;

inline bool success(const document& doc) {
    if (!doc.is_valid()) {
        fprintf(stderr, "%s\n", doc.get_error_message_as_cstring());
        return false;
    }
    return true;
}

struct jsonstats {
    jsonstats()
        : null_count(0)
        , false_count(0)
        , true_count(0)
        , number_count(0)
        , object_count(0)
        , array_count(0)
        , string_count(0)
        , total_string_length(0)
        , total_array_length(0)
        , total_object_length(0)
        , total_number_value(0) {}

    size_t null_count;
    size_t false_count;
    size_t true_count;
    size_t number_count;
    size_t object_count;
    size_t array_count;
    size_t string_count;

    size_t total_string_length;
    size_t total_array_length;
    size_t total_object_length;
    double total_number_value;
};

void traverse(jsonstats& stats, const sajson::value& node) {
    using namespace sajson;

    switch (node.get_type()) {
    case TYPE_NULL:
        ++stats.null_count;
        break;

    case TYPE_FALSE:
        ++stats.false_count;
        break;

    case TYPE_TRUE:
        ++stats.true_count;
        break;

    case TYPE_ARRAY: {
        ++stats.array_count;
        auto length = node.get_length();
        stats.total_array_length += length;
        for (size_t i = 0; i < length; ++i) {
            traverse(stats, node.get_array_element(i));
        }
        break;
    }

    case TYPE_OBJECT: {
        ++stats.object_count;
        auto length = node.get_length();
        stats.total_object_length += length;
        for (auto i = 0u; i < length; ++i) {
            traverse(stats, node.get_object_value(i));
        }
        break;
    }

    case TYPE_STRING:
        ++stats.string_count;
        stats.total_string_length += node.get_string_length();
        break;

    case TYPE_DOUBLE:
    case TYPE_INTEGER:
        ++stats.number_count;
        stats.total_number_value += node.get_number_value();
        break;

    default:
        assert(false && "unknown node type");
    }
}

int main(int argc, char** argv) {
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        fprintf(stderr, "Failed to open file\n");
        return 1;
    }
    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    fseek(file, 0, SEEK_SET);

    // "leak"
    char* buffer = new char[length];
    if (length != fread(buffer, 1, length, file)) {
        fprintf(stderr, "Failed to read entire file\n");
        return 1;
    }
    fclose(file);

    const sajson::document& document = sajson::parse(
        sajson::dynamic_allocation(), mutable_string_view(length, buffer));
    if (!success(document)) {
        return 1;
    }

    jsonstats stats;
    traverse(stats, document.get_root());

    printf("object count: %d\n", (int)stats.object_count);
    printf("array count: %d\n", (int)stats.array_count);
    printf("bool count: %d\n", (int)stats.true_count + (int)stats.false_count);
    printf("number count: %d\n", (int)stats.number_count);
    printf("string count: %d\n", (int)stats.string_count);
    printf("null count: %d\n", (int)stats.null_count);
}
