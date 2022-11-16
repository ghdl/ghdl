#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

for f in mve repro1 repro2 repro3; do
    analyze $f.vhdl
    elab_simulate $f
done

clean

echo "Test successful"
