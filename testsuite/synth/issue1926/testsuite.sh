#! /bin/sh

. ../../testenv.sh

synth -fsynopsys ecp5pll.vhdl top_ecp5pll.vhdl -e > syn_top_ecp5pll.vhdl

echo "Test successful"
