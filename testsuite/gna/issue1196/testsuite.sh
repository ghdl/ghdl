#! /bin/sh

. ../../testenv.sh

analyze_failure my_pkg.vhdl

if $GHDL -s my_pkg.vhdl ; then
  echo "failure expected"
  exit 1;
fi

echo "Test successful"
