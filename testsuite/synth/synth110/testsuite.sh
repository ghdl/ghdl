#! /bin/sh

. ../../testenv.sh

synth_analyze top
grep "if rising_edge" syn_top.vhdl

clean

echo "Test successful"
