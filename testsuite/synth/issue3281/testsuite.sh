#! /bin/sh

. ../../testenv.sh

synth_only inout
fgrep -q "q0 => q0" syn_inout.vhdl

synth_only inout2
fgrep -q "q0 <= " syn_inout2.vhdl

echo "Test successful"
