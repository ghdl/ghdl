#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mwe1.vhdl
elab_simulate mwe1

analyze mwe2.vhdl
elab_simulate mwe2

for f in acc1 acc2; do
    analyze ${f}.vhdl
    elab_simulate $f
done

clean

echo "Test successful"
