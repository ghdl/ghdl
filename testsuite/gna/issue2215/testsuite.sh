#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze e.vhdl
elab_simulate e > e.out
diff_nocr e.ref e.out

clean

echo "Test successful"
