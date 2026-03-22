#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=19


synth axis_pkg.vhdl view01.vhdl -e > syn_view01.vhdl
analyze axis_pkg.vhdl syn_view01.vhdl

synth axis_pkg.vhdl view02.vhdl -e > syn_view02.vhdl
analyze axis_pkg.vhdl syn_view02.vhdl

clean

echo "Test successful"
