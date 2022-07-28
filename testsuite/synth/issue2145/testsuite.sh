#! /bin/sh

. ../../testenv.sh

GHDL_SYNTH_FLAGS=-gwidth=1
synth_analyze bug
clean

echo "Test successful"
