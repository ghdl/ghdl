#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze blk1.vhdl
elab_simulate_failure blk1

analyze blk2.vhdl
elab_simulate_failure blk2

if ghdl_is_preelaboration; then
    analyze repro.vhdl
    elab_simulate_failure repro

    analyze repro2.vhdl
    elab_simulate repro2

    clean
fi

echo "Test successful"
