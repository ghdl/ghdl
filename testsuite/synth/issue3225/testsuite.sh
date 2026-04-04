#! /bin/sh

. ../../testenv.sh

synth_only tb
fgrep -q 'a => a' syn_tb.vhdl

echo "Test successful"
