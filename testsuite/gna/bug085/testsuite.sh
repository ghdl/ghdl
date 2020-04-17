#! /bin/sh

. ../../testenv.sh

analyze foo.vhdl
elab_simulate_failure foo

clean

echo "Test successful"
