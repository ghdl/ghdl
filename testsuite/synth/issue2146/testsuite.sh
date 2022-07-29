#! /bin/sh

. ../../testenv.sh

GHDL_SYNTH_FLAGS=-gCONFIG="10111011" 
synth_analyze rom_test 2> synth.log
grep "found ROM" synth.log

clean

echo "Test successful"
