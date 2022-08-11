#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze array_of_string_elements.vhdl
elab_simulate_failure array_of_string_elements

clean

echo "Test successful"
