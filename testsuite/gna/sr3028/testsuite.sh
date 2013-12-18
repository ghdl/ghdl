#! /bin/sh

. ../../testenv.sh

echo "Skipped !!!!"
exit 0

analyze vc.vhdl
analyze top.vhdl
elab_simulate_failure top

clean

echo "Test successful"
