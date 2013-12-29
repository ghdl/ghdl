#! /bin/sh

. ../../testenv.sh

analyze twoscomplement.vhdl
elab_simulate_failure test

clean

echo "Test successful"
