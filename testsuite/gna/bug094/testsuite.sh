#! /bin/sh

. ../../testenv.sh

cp pkga_v1.vhdl pkga.vhdl
analyze pkga.vhdl
analyze enta.vhdl
analyze topa.vhdl
elab_simulate topa

cp pkga_v2.vhdl pkga.vhdl
analyze_failure topa.vhdl

clean
rm -f pkga.vhdl

export GHDL_STD_FLAGS="--std=08 -frelaxed-rules"
analyze pkgb.vhdl
analyze entb.vhdl
analyze topb.vhdl
elab_simulate topb

export GHDL_STD_FLAGS="--std=08"
analyze_failure topb.vhdl

clean

echo "Test successful"
