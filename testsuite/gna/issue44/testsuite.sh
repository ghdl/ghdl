#! /bin/sh

. ../../testenv.sh

analyze loopfilter.vhdl
analyze dffregister.vhdl
elab_simulate loopfilter

analyze costasloop.vhdl
analyze q_one_dot_fp_multiplier.vhdl

clean

echo "Test successful"

