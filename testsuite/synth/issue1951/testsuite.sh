#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys
synth_only ent

for f in sub01 sub02 sub03 sub04; do
    synth_tb $f
done

echo "Test successful"
