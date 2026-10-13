#! /bin/sh

. ../../testenv.sh

for std in 08 19; do
  export GHDL_STD_FLAGS=--std=$std
  analyze repro.vhdl
  elab_simulate repro --assert-level=error
  clean
done

echo "Test successful"
