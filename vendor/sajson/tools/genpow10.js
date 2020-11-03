var util = require('util');

function findExtreme(direction) {
    var p = 0;
    while (Math.pow(10, p) !== Math.pow(10, p + direction)) {
        p += direction;
    }
    return p - direction;
}

var maximum = findExtreme(1);
var minimum = findExtreme(-1);

util.print("double pow10(int exponent) {\n");
util.print("    if (exponent > ", maximum, ") {\n");
util.print("        return std::numeric_limits<double>::infinity();\n");
util.print("    } else if (exponent < ", minimum, ") {\n");
util.print("        return 0.0;\n");
util.print("    }\n");
util.print("    static const double constants[] = {\n");
var currentLine = "";
for (var i = minimum; i <= maximum; ++i) {
    var next = "1e" + i;
    if (i < maximum) {
        next += ",";
    }
    any = true;
    if ((currentLine + next).length > 72) {
        util.print("        " + currentLine + "\n");
        currentLine = "";
    }
    currentLine += next;
}
if (currentLine.length) {
    util.print("        " + currentLine + "\n");
}
util.print("    };\n");
util.print("    return constants[exponent + ", -minimum, "];\n");
util.print("}\n");
