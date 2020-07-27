#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
for i in repro1 repro4 repro5 repro6 repro7 repro8 repro12 repro13; do
  analyze $i.vhdl
  elab_simulate $i
done

clean

echo "Test successful"
