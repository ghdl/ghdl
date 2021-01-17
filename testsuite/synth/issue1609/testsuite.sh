#! /bin/sh

. ../../testenv.sh

synth --std=08 exp_psl.vhdl -e > syn_psl.vhdl
grep -q "gate_anyconst" syn_psl.vhdl

synth --std=08 exp_vhd.vhdl -e > syn_vhd.vhdl
grep -q "gate_anyconst" syn_vhd.vhdl

echo "Test successful"
