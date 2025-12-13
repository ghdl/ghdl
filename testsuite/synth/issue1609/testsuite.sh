#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only exp_psl
grep -q "gate_anyconst" syn_exp_psl.vhdl

synth_only exp_psl2
grep -q "gate_allconst" syn_exp_psl2.vhdl

synth_only exp_vhd
grep -q "gate_anyconst" syn_exp_vhd.vhdl

echo "Test successful"
