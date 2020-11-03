#pragma once

#include "sajson.h"
#include <ostream>

namespace sajson {
inline std::ostream& operator<<(std::ostream& os, type t) {
    switch (t) {
    case TYPE_INTEGER:
        return os << "<integer>";
    case TYPE_DOUBLE:
        return os << "<double>";
    case TYPE_NULL:
        return os << "<null>";
    case TYPE_FALSE:
        return os << "<false>";
    case TYPE_TRUE:
        return os << "<true>";
    case TYPE_STRING:
        return os << "<string>";
    case TYPE_ARRAY:
        return os << "<array>";
    case TYPE_OBJECT:
        return os << "<object>";
    default:
        return os << "<unknown type";
    }
}
} // namespace sajson
