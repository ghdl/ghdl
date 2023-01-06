#! /bin/sh

. ../../testenv.sh

analyze test2.vhdl
elab_simulate_failure test2 > test2.out 2>&1
grep "NULL access" test2.out

clean

echo "Test successful"
