#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in min01 smin01 smin02 smin03 umin01 umin02 umin03 \
	 max01 smax01 smax02 smax03 umax01 umax02 umax03 \
	 uns02 \
	 urot01 srot01 \
	 sshift01 ushift01 nushift01 \
	 nand01 nand02 \
	 nor01 nor02; do
    synth_tb $t
done

synth_only shift02
synth_only ushift02
synth_failure nor03.vhdl -e
synth_only nor04
synth_failure nor05.vhdl -e
synth_only nor06
synth_only nor07

synth_failure urot02.vhdl -e

echo "Test successful"
