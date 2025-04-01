#! /bin/sh

. ../../testenv.sh

analyze e.vhdl
elab_simulate_failure e

analyze e2.vhdl
elab_simulate_failure e2

clean

echo "Test successful"
