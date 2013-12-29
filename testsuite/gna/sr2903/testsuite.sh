#! /bin/sh

. ../../testenv.sh

analyze boundcheck.vhdl
elab_simulate_failure tb

clean

echo "Test successful"
