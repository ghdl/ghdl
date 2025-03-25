#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze jcsr.vhdl
    elab_simulate j_csr 2> log.err
    grep 'assert warning' log.err
    grep 'jcsr.vhdl:14' log.err

    clean
fi

echo "Test successful"
