#include "sajson.h"
#include <assert.h>

using namespace sajson;

int main() {
    char input_text[]
        = "{\"menu\":{\"header\":\"SVG "
          "Viewer\",\"items\":[{\"id\":\"Open\"},{\"id\":\"OpenNew\",\"label\":"
          "\"Open New\"},null,{\"id\":\"ZoomIn\",\"label\":\"Zoom "
          "In\"},{\"id\":\"ZoomOut\",\"label\":\"Zoom "
          "Out\"},{\"id\":\"OriginalView\",\"label\":\"Original "
          "View\"},null,{\"id\":\"Quality\"},{\"id\":\"Pause\"},{\"id\":"
          "\"Mute\"},"
          "null,{\"id\":\"Find\",\"label\":\"Find...\"},{\"id\":\"FindAgain\","
          "\"label\":\"Find "
          "Again\"},{\"id\":\"Copy\"},{\"id\":\"CopyAgain\",\"label\":\"Copy "
          "Again\"},{\"id\":\"CopySVG\",\"label\":\"Copy "
          "SVG\"},{\"id\":\"ViewSVG\",\"label\":\"View "
          "SVG\"},{\"id\":\"ViewSource\",\"label\":\"View "
          "Source\"},{\"id\":\"SaveAs\",\"label\":\"Save "
          "As\"},null,{\"id\":\"Help\"},{\"id\":\"About\",\"label\":\"About "
          "Adobe "
          "CVG Viewer...\"}]}}";

    const size_t AST_BUFFER_SIZE = 500;
    size_t ast_buffer[AST_BUFFER_SIZE];

    const document& doc = parse(
        // The bounded allocation mode attempts to fit the AST into the given
        // fixed-size buffer, returning ERROR_OUT_OF_MEMORY if it is not large
        // enough to parse this document.
        bounded_allocation(ast_buffer, AST_BUFFER_SIZE),
        // If sajson is asked to parse a mutable_string_view, it will not
        // allocate a copy and instead parse in-place.
        mutable_string_view(strlen(input_text), input_text));

    if (doc.is_valid()) {
        puts("success!");
        return 0;
    } else {
        printf("parse failed: %s\n", doc.get_error_message_as_cstring());
        return 1;
    }
}
