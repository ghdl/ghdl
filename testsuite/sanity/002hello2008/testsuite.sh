#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze hello.vhdl
elab_simulate hello

# Check that vhdl-08 is used
analyze err93.vhdl

clean

# Check that err93 is not valid in vhdl-93
GHDL_STD_FLAGS=
analyze_failure err93.vhdl

clean

echo "test successful"
