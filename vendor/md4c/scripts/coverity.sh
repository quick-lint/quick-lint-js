#!/bin/sh
#
# This scripts attempts to build the project via cov-build utility, and prepare
# a package for uploading to the coverity scan service.
#
# (See http://scan.coverity.com for more info.)

set -e

# Check presence of coverity static analyzer.
if ! which cov-build; then
    echo "Utility cov-build not found in PATH."
    exit 1
fi

# Choose a build system (ninja or GNU make).
if which ninja; then
    BUILD_TOOL=ninja
    GENERATOR=Ninja
elif which make; then
    BUILD_TOOL=make
    GENERATOR="MSYS Makefiles"
else
    echo "No suitable build system found."
    exit 1
fi

# Choose a zip tool.
if which 7za; then
    MKZIP="7za a -r -mx9"
elif which 7z; then
    MKZIP="7z a -r -mx9"
elif which zip; then
    MKZIP="zip -r"
else
    echo "No suitable zip utility found"
    exit 1
fi

# Change dir to project root.
cd `dirname "$0"`/..

CWD=`pwd`
ROOT_DIR="$CWD"
BUILD_DIR="$CWD/coverity"
OUTPUT="$CWD/cov-int.zip"

# Sanity checks.
if [ ! -x "$ROOT_DIR/scripts/coverity.sh" ]; then
    echo "There is some path mismatch."
    exit 1
fi
if [ -e "$BUILD_DIR" ]; then
    echo "Path $BUILD_DIR already exists. Delete it and retry."
    exit 1
fi
if [ -e "$OUTPUT" ]; then
    echo "Path $OUTPUT already exists. Delete it and retry."
    exit 1
fi

# Build the project with the Coverity analyzes enabled.
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"
cmake -G "$GENERATOR" "$ROOT_DIR"
cov-build --dir cov-int "$BUILD_TOOL"
$MKZIP "$OUTPUT" "cov-int"
cd "$ROOT_DIR"
rm -rf "$BUILD_DIR"

