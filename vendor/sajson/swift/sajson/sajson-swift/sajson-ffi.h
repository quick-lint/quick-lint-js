#pragma once

#include <stddef.h>
#include <stdint.h>

struct sajson_document;

// Swift turns size_t into Int but we want UInt on the Swift side
typedef unsigned long sajson_element;

#ifdef __cplusplus
static_assert(
    sizeof(sajson_element) == sizeof(size_t),
    "sajson_element should be pointer-sized and also convert to the "
    "right Swift types");

extern "C" {
#endif

struct sajson_document*
sajson_parse_single_allocation(char* bytes, size_t length);
struct sajson_document*
sajson_parse_dynamic_allocation(char* bytes, size_t length);
void sajson_free_document(struct sajson_document* doc);
int sajson_has_error(struct sajson_document* doc);
size_t sajson_get_error_line(struct sajson_document* doc);
size_t sajson_get_error_column(struct sajson_document* doc);
const char* sajson_get_error_message(struct sajson_document* doc);
uint8_t sajson_get_root_tag(struct sajson_document* doc);
const sajson_element* sajson_get_root(struct sajson_document* doc);
const unsigned char* sajson_get_input(struct sajson_document* doc);
size_t sajson_get_input_length(struct sajson_document* doc);
size_t sajson_find_object_key(
    const sajson_element* payload,
    const char* key,
    size_t length,
    const unsigned char* input);

#ifdef __cplusplus
}
#endif
