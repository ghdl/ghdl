#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
for f in repro repro2 repro3 repro4; do
analyze $f.vhdl
elab_simulate $f
done

clean

echo "Test successful"
