#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test.vhdl
elab_simulate test

for f in tst_rec tst_arr; do
    analyze_failure $f.vhdl
done

clean

echo "Test successful"
