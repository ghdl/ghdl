#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in my_is_x; do
    synth_tb $t
done

synth float16_pkg.vhdl fadd.vhdl -e > syn_fadd.vhdl
analyze float16_pkg.vhdl syn_fadd.vhdl tb.vhdl
elab_simulate tb --ieee-asserts=disable-at-0

clean

echo "Test successful"
