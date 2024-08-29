#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

if ghdl_is_preelaboration; then
    analyze mwe.vhdl
    elab_simulate mwe

    clean
    
    analyze mwe2.vhdl
    elab_simulate mwe

    clean

    analyze mwe3.vhdl
    elab_simulate mwe

    clean

    analyze_failure mwe4.vhdl
fi

echo "Test successful"
