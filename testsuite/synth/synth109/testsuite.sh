#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="-fsynopsys"
synth_tb ram1
synth_tb ram2
synth_tb ram4

synth_analyze ram9
synth asymmetric_ram_2a.vhd  -e > syn_asymmetric_ram_2a.vhdl

echo "Test successful"
