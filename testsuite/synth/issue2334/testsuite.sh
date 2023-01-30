#! /bin/sh

. ../../testenv.sh

synth_failure repro.vhdl -e

synth_failure top_ecp5pll.vhd ecp5pll.vhd  -e top_ecp5pll

echo "Test successful"
