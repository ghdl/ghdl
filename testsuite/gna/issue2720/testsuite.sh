#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze timer.vhdl tb.vhdl
if elab_simulate T19_ProcedureTb --stop-time=108000000ms > err.txt; then
  echo "error expected"
  exit 1
fi

grep "too large" err.txt

clean

echo "Test successful"
