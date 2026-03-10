#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in my_is_x; do
    synth_tb $t
done

synth float16_pkg.vhdl fadd.vhdl -e > syn_fadd.vhdl

echo "Test successful"
