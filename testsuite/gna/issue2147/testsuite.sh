#! /bin/sh

. ../../testenv.sh

analyze_failure e.vhdl

analyze e2.vhdl
elab_simulate e2

clean

echo "Test successful"
