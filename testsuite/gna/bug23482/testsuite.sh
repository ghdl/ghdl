#! /bin/sh

. ../../testenv.sh

analyze_failure test1.vhdl

analyze test2.vhdl
elab_simulate test2

clean

echo "Test successful"
