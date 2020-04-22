#! /bin/sh

. ../../testenv.sh

cp simple01.vhdl simple.vhdl
analyze simple.vhdl

cp simple02.vhdl simple.vhdl
if $GHDL --synth 2>&1 | grep "Bug occurred"; then
  exit 1
fi

clean

echo "Test successful"
