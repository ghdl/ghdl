#! /bin/sh

. ../../testenv.sh

analyze test2.vhdl
elab_simulate_failure test2 2>&1 > test2.out
grep "NULL access" test2.out

clean

echo "Test successful"
