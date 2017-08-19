#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=87
analyze hello.vhdl
elab_simulate hello

# Check that vhdl-87 is used
analyze_failure err87.vhdl

clean

# Check that err87 is a valid vhdl-93 source file
GHDL_STD_FLAGS=
analyze err87.vhdl

clean

echo "test successful"
