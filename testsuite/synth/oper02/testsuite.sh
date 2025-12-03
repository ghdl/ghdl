#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in min01 smin01 umin01 \
	 max01 smax01 umax01 \
	 uns02 \
	 urot01 srot01 \
	 sshift01 ushift01 nushift01; do
    synth_tb $t
done

synth_only shift02

echo "Test successful"
