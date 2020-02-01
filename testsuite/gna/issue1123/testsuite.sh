#! /bin/sh

. ../../testenv.sh

analyze_failure b.vhdl
analyze b1.vhdl
elab_simulate b

clean

echo "Test successful"
