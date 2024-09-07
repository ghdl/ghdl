#! /bin/sh

. ../../testenv.sh

synth_only repro1b

if grep "when rising_edge" syn_repro1b.vhdl; then
    exit 1
fi

echo "Test successful"
