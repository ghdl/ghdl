#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze repro.vhdl
    elab_simulate b

    analyze repro1.vhdl
    elab_simulate b1 --vcd-nodate --vcd=out.vcd

    diff_nocr out.vcd ref.vcd

    clean
fi

echo "Test successful"
