#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=93
analyze_failure pkg.vhdl

analyze repro2.vhdl
analyze repro3.vhdl
analyze_failure repro3b.vhdl

GHDL_STD_FLAGS=--std=93c
analyze pkg.vhdl
analyze ent.vhdl
elab_simulate ent

analyze_failure repro3b.vhdl

clean

GHDL_STD_FLAGS=--std=08
analyze ent2.vhdl
elab_simulate ent

analyze repro3b.vhdl
analyze_failure repro3.vhdl
analyze -frelaxed repro3.vhdl

clean

echo "Test successful"
