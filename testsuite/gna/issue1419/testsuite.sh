#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze_failure -Werror repro3.vhdl
analyze_failure -Werror repro4.vhdl

analyze repro1.vhdl
elab_simulate repro1

analyze repro2.vhdl
elab_simulate repro2

analyze model_pkg.vhdl
analyze tb.vhdl
elab_simulate tb

clean

echo "Test successful"
