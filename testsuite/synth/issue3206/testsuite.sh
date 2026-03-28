#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=19

synth axis_pkg.vhdl ent2.vhdl -e > syn_ent2.vhdl
analyze axis_pkg.vhdl syn_ent2.vhdl

clean

synth axis_pkg.vhdl ent.vhdl -e > syn_ent.vhdl
analyze axis_pkg.vhdl syn_ent.vhdl

clean

echo "Test successful"
