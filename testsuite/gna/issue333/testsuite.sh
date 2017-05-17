#! /bin/sh

. ../../testenv.sh

if $GHDL --version | grep -q "mcode code generator"; then
  analyze repro.vhdl
  elab_simulate_failure repro

  analyze time_test.vhdl
  elab_simulate_failure time_test
fi

clean

echo "Test successful"
