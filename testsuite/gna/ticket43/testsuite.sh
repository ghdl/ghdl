#! /bin/sh

. ../../testenv.sh

analyze --std=02 file1.vhd
analyze --std=02 file2.vhd
analyze --std=02 bug.vhdl

elab_simulate --std=02 bug

clean

echo "Test successful"
