#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze_failure -Werror repro3.vhdl
analyze_failure -Werror repro4.vhdl

#analyze model_pkg.vhdl
#analyze tb
#elab_simulate tb

clean

echo "Test successful"
