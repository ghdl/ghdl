#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze --work=modulation_lib types_pkg.vhdl
    analyze --work=modulation_lib generic_stack_pkg.vhdl
    analyze --work=modulation_lib symbol_stack_pkg.vhdl
    analyze --work=modulation_lib modulator.vhdl
    analyze testbench.vhdl
    elab_simulate testbench

    clean modulation_lib
    clean
fi

echo "Test successful"
