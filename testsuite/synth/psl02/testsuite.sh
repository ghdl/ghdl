#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth assert1.vhdl -e assert1 > syn_assert1.vhdl
analyze syn_assert1.vhdl

synth assert2.vhdl verif1.vhdl -e assert2 > syn_assert21.vhdl
analyze syn_assert21.vhdl

clean

synth assert2.vhdl verif2.vhdl -e assert2 > syn_assert22.vhdl
analyze syn_assert22.vhdl

clean

synth assert2.vhdl verif3.vhdl -e assert2 > syn_assert23.vhdl
analyze syn_assert23.vhdl

clean

synth assert2.vhdl verif4.vhdl -e assert2 > syn_assert24.vhdl
analyze syn_assert23.vhdl

clean

synth assert2.vhdl verif5.vhdl -e assert2 > syn_assert25.vhdl
analyze syn_assert25.vhdl

clean

echo "Test successful"
