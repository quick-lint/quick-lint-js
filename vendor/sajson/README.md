# sajson

sajson is an extremely high-performance, in-place, DOM-style JSON parser written in C++.

Originally, sajson meant Single Allocation JSON, but it now supports dynamic allocation too.

## Features

sajson parses an input document into a contiguous AST structure.  Unlike some other high-performance JSON parsers, the AST is efficiently queryable.  Object lookups by key are O(lg N) and array indexing is O(1).

sajson does not require that the input buffer is null-terminated.  You can use it to parse straight out of a disk mmap or network buffer, for example.

sajson is in-situ: it modifies the input string.  While parsing, string values are converted to UTF-8.

(Note: sajson pays a slight performance penalty for not requiring null termination of the input string.  Because sajson is in-situ, many uses cases require copying the input data anyway.  Therefore, I could be convinced to add an option for requiring null termination.)

### Other Features

* Single header file -- simply drop sajson.h into your project.
* No exceptions, RTTI, or longjmp.
* O(1) stack usage. No document will overflow the stack.
* Only two number types: 32-bits and doubles.
* Small code size -- suitable for Emscripten.
* Has been fuzzed with American Fuzzy Lop.

## AST Structure

The parsed AST's size is computed as such:

* 2 words per string
* 1 word per 32-bit integer value
* 64 bits per floating point value
* 1+N words per array, where N is the number of elements
* 1+3N words per object, where N is the number of members

The values null, true, and false are encoded in tag bits and have no cost otherwise.

## Allocation Modes

### Single

The original sajson allocation mode allocates one word per byte of the input document.  This is the fastest mode: because the AST and parse stack are guaranteed to fit, no allocation checks are required at runtime.

That is, on 32-bit platforms, sajson allocates 4 bytes per input character.  On 64-bit platforms, sajson allocates 8 bytes per input character.  Only use this parse mode if you can handle allocating the worst-case buffer size for your input documents.

### Dynamic

The dynamic allocation mode grows the parse stack and AST buffer as needed.  It's about 10-40% slower than single allocation because it needs to check for out-of-memory every time data is appended, and occasionally the buffers need to be reallocated and copied.

### Bounded

The bounded allocation mode takes a fixed-size memory buffer and uses it for both
the parse stack and the resulting AST.  If the parse stack and AST fit in the given
buffer, the parse succeeds.  This allocation mode allows using sajson without
the library making any allocations.

## Performance

sajson's performance is excellent - it frequently benchmarks faster than RapidJSON, for example.

Implementation details are available at [http://chadaustin.me/tag/sajson/](http://chadaustin.me/tag/sajson/).

## Documentation

API documentation is available at http://chadaustin.github.io/sajson/doxygen/

## Downsides / Missing Features

* sajson does not support UTF-16 or UTF-32.  However, I have never seen one of those in the wild, so I suspect they may be a case of aggressive overspecification.  Some JSON specifications indicate that UTF-8 is the only valid encoding.  Either way, just transcode to UTF-8 first.

* No support for 64-bit integers.  If this is something you want, just ask.  (There's little technical reason not to support it.  It's just that most people avoid 64-bit integers in JSON because JavaScript can't read them.)

* Requires C++11.  Some of the ownership semantics were awkward to express in C++03.
