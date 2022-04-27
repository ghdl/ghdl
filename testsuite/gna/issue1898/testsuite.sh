#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

for f in scale scale2 scale3 scale4; do
  analyze $f.vhdl
  elab_simulate $f
done

clean

echo "Test successful"
