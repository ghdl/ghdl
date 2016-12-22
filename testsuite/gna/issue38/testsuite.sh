#! /bin/sh

. ../../testenv.sh

analyze_failure bugreport_aliasprotected.vhdl 2>log.txt

if grep -q "'protected' is expected instead of 'protected'" log.txt; then
  echo "Incorrect error message"
  exit 1
fi
rm log.txt

clean test

# TODO
if false; then
GHDL_STD_FLAGS=--std=08

analyze bugreport_aliasprotected.vhdl
simulate test

clean test
fi

echo "Test successful"
