#! /bin/sh

. ../../testenv.sh

analyze_failure foo.vhdl

export GHDL_STD_FLAGS=--std=08

for f in foo slv01 numstd01; do
    analyze $f.vhdl
    elab_simulate $f
done

clean

echo "Test successful"
