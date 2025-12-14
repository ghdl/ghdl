#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_analyze attr01
fgrep -q "keep of counter" syn_attr01.vhdl
clean

synth_analyze attr02
fgrep -q "keep of rst" syn_attr02.vhdl
clean

# TODO: generated vhdl is not correct for the attribute type
synth_only attr04
fgrep -q 'my_attr of counter : signal is "01X"' syn_attr04.vhdl
clean

GHDL_STD_FLAGS=-frelaxed
synth_tb attr03

echo "Test successful"
