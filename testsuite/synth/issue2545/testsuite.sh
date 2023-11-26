#! /bin/sh

. ../../testenv.sh

synth_only top

! grep -q assert syn_top.vhdl

echo "Test successful"
