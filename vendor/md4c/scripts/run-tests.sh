#!/bin/sh
#
# Run this script from build directory.

#set -e

SELF_DIR=`dirname $0`
PROJECT_DIR="$SELF_DIR/.."
TEST_DIR="$PROJECT_DIR/test"


PROGRAM="md2html/md2html"
if [ ! -x "$PROGRAM" ]; then
    echo "Cannot find the $PROGRAM." >&2
    echo "You have to run this script from the build directory." >&2
    exit 1
fi

if which py >>/dev/null 2>&1; then
    PYTHON=py
elif which python3 >>/dev/null 2>&1; then
    PYTHON=python3
elif which python >>/dev/null 2>&1; then
    if [ `python --version | awk '{print $2}' | cut -d. -f1` -ge 3 ]; then
        PYTHON=python
    fi
fi

echo
echo "CommonMark specification:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/spec.txt" -p "$PROGRAM"

echo
echo "Code coverage & regressions:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/coverage.txt" -p "$PROGRAM"

echo
echo "Permissive e-mail autolinks extension:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/permissive-email-autolinks.txt" -p "$PROGRAM --fpermissive-email-autolinks"

echo
echo "Permissive URL autolinks extension:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/permissive-url-autolinks.txt" -p "$PROGRAM --fpermissive-url-autolinks"

echo
echo "WWW autolinks extension:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/permissive-www-autolinks.txt" -p "$PROGRAM --fpermissive-www-autolinks"

echo
echo "Tables extension:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/tables.txt" -p "$PROGRAM --ftables"

echo
echo "Strikethrough extension:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/strikethrough.txt" -p "$PROGRAM --fstrikethrough"

echo
echo "Task lists extension:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/tasklists.txt" -p "$PROGRAM --ftasklists"

echo
echo "LaTeX extension:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/latex-math.txt" -p "$PROGRAM --flatex-math"

echo
echo "Wiki links extension:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/wiki-links.txt" -p "$PROGRAM --fwiki-links --ftables"

echo
echo "Underline extension:"
$PYTHON "$TEST_DIR/spec_tests.py" -s "$TEST_DIR/underline.txt" -p "$PROGRAM --funderline"

echo
echo "Pathological input:"
$PYTHON "$TEST_DIR/pathological_tests.py" -p "$PROGRAM"
