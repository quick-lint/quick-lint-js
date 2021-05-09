#!/usr/bin/env python3

import os
import sys
import textwrap


self_path = os.path.dirname(os.path.realpath(__file__));
f = open(self_path + "/unicode/CaseFolding.txt", "r")

status_list = [ "C", "F" ]

folding_list = [ dict(), dict(), dict() ]

# Filter the foldings for "full" folding.
for line in f:
    comment_off = line.find("#")
    if comment_off >= 0:
        line = line[:comment_off]
    line = line.strip()
    if not line:
        continue

    raw_codepoint, status, raw_mapping, ignored_tail = line.split(";", 3)
    if not status.strip() in status_list:
        continue
    codepoint = int(raw_codepoint.strip(), 16)
    mapping = [int(it, 16) for it in raw_mapping.strip().split(" ")]
    mapping_len = len(mapping)

    if mapping_len in range(1, 4):
        folding_list[mapping_len-1][codepoint] = mapping
    else:
        assert(False)
f.close()


# If we assume that (index0 ... index-1) makes a range (as defined below),
# check that the newly provided index is compatible with the range too; i.e.
# verify that the range can be extended without breaking its properties.
#
# Currently, we can handle ranges which:
#
# (1) either form consecutive sequence of codepoints and which map that range
#     to other consecutive range of codepoints (of the same length);
#
# (2) or a consecutive sequence of codepoints with step 2 where each codepoint
#     CP is mapped to the codepoint CP+1
#     (e.g. 0x1234 -> 0x1235; 0x1236 -> 0x1237; 0x1238 -> 0x1239; ...).
#
# Note: When the codepoints in the range are mapped to multiple codepoints,
# only the 1st mapped codepoint is considered. All the other ones have to be
# shared by all the mappings covered by the range.
def is_range_compatible(folding, codepoint_list, index0, index):
    N = index - index0
    codepoint0 = codepoint_list[index0]
    codepoint1 = codepoint_list[index0+1]
    codepointN = codepoint_list[index]
    mapping0 = folding[codepoint0]
    mapping1 = folding[codepoint1]
    mappingN = folding[codepointN]

    # Check the range type (1):
    if codepoint1 - codepoint0 == 1 and codepointN - codepoint0 == N                \
            and mapping1[0] - mapping0[0] == 1 and mapping1[1:] == mapping0[1:]     \
            and mappingN[0] - mapping0[0] == N and mappingN[1:] == mapping0[1:]:
        return True

    # Check the range type (2):
    if codepoint1 - codepoint0 == 2 and codepointN - codepoint0 == 2 * N            \
            and mapping0[0] - codepoint0 == 1                                       \
            and mapping1[0] - codepoint1 == 1 and mapping1[1:] == mapping0[1:]      \
            and mappingN[0] - codepointN == 1 and mappingN[1:] == mapping0[1:]:
        return True

    return False


def mapping_str(list, mapping):
    return ",".join("0x{:04x}".format(x) for x in mapping)

for mapping_len in range(1, 4):
    folding = folding_list[mapping_len-1]
    codepoint_list = list(folding)

    index0 = 0
    count = len(folding)

    records = list()
    data_records = list()

    while index0 < count:
        index1 = index0 + 1
        while index1 < count and is_range_compatible(folding, codepoint_list, index0, index1):
            index1 += 1

        if index1 - index0 > 2:
            # Range of codepoints
            records.append("R(0x{:04x},0x{:04x})".format(codepoint_list[index0], codepoint_list[index1-1]))
            data_records.append(mapping_str(data_records, folding[codepoint_list[index0]]))
            data_records.append(mapping_str(data_records, folding[codepoint_list[index1-1]]))
            index0 = index1
        else:
            # Single codepoint
            records.append("S(0x{:04x})".format(codepoint_list[index0]))
            data_records.append(mapping_str(data_records, folding[codepoint_list[index0]]))
            index0 += 1

    sys.stdout.write("static const unsigned FOLD_MAP_{}[] = {{\n".format(mapping_len))
    sys.stdout.write("\n".join(textwrap.wrap(", ".join(records), 110,
                        initial_indent = "    ", subsequent_indent="    ")))
    sys.stdout.write("\n};\n")

    sys.stdout.write("static const unsigned FOLD_MAP_{}_DATA[] = {{\n".format(mapping_len))
    sys.stdout.write("\n".join(textwrap.wrap(", ".join(data_records), 110,
                        initial_indent = "    ", subsequent_indent="    ")))
    sys.stdout.write("\n};\n")



