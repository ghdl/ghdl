#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
for f in conf1.vhdl psl1.vhdl psl2.vhdl retid.vhdl; do
    analyze_failure $f
done

clean

echo "Test successful"

