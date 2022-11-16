#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

# TODO: repro1
for f in mve  repro2 repro3; do
    analyze $f.vhdl
    elab_simulate $f
done

clean

echo "Test successful"
