#! /bin/sh

. ../../testenv.sh

synth_tb top
# Check data is stored at the correct index
grep -q -F '10 => "00001010",' syn_top.vhdl

echo "Test successful"
