#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

for t in repro1 repro2 repro3 repro4 repro5 repro6 repro7; do
    analyze $t.vhdl
    elab_simulate $t
done

clean

echo "Test successful"
