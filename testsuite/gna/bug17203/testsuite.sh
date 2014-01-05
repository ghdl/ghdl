#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate test_time

clean

echo "Test successful"
