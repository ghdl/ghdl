#! /bin/sh

. ../../testenv.sh

analyze -fpsl psl.vhdl
elab_simulate -fpsl psl --psl-report=psl.out

if ! cmp psl.out psl.ref; then
    echo "report mismatch"
    exit 1
fi

# Usage example (python 2.7):
#
# import json
# d=json.load(open("psl.out"))
# print d['summary']
#  {u'assert-pass': 2, u'cover': 1, ... }
# print d['summary']['assert']

rm -f psl.out
clean

echo "Test successful"
