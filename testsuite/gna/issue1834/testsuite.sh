#! /bin/sh

. ../../testenv.sh

analyze_failure -Werror tb.vhdl

analyze tb.vhdl
elab_simulate_failure tb

clean

echo "Test successful"
