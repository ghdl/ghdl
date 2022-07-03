#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="-fpsl"
analyze psl.vhdl
elab psl
if ghdl_has_feature psl psl; then
  simulate psl --psl-report=psl.out

  if ! diff_nocr psl.out psl.ref; then
      echo "report mismatch"
      exit 1
  fi

  rm -f psl.out
fi
clean

# Using vhdl 08
GHDL_STD_FLAGS="-fpsl --std=08"
analyze psl.vhdl
elab -fpsl psl
if ghdl_has_feature psl psl; then
  simulate psl --psl-report=psl.out

  diff_nocr psl.out psl.ref

  rm -f psl.out
fi
clean

# Usage example (python 2.7):
#
# import json
# d=json.load(open("psl.out"))
# print d['summary']
#  {u'assert-pass': 2, u'cover': 1, ... }
# print d['summary']['assert']

echo "Test successful"
