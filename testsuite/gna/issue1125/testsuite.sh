#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze etest.vhdl
elab_simulate_failure etest > etest.err
grep "slice direction" etest.err

clean
rm -f etest.err

echo "Test successful"
