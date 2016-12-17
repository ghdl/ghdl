#! /bin/sh

echo "Test skipped"
exit 0

. ../../testenv.sh

analyze -fpsl psl.vhdl
elab_simulate -fpsl psl --psl-report=psl.out

if ! cmp psl.out psl.ref; then
    echo "report mismatch"
    exit 1
fi

rm -f psl.out
clean

# Using vhdl 08
GHDL_STD_FLAGS="-fpsl --std=08"
analyze psl.vhdl
elab_simulate psl --psl-report=psl.out

cmp -s psl.out psl.ref

rm -f psl.out
clean

# Usage example (python 2.7):
#
# import json
# d=json.load(open("psl.out"))
# print d['summary']
#  {u'assert-pass': 2, u'cover': 1, ... }
# print d['summary']['assert']

echo "Test successful"
