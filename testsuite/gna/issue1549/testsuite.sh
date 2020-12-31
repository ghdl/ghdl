#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

for f in ent ent2 ent3 ent4 ent5 ent6 ent7 ent8; do
    analyze $f.vhdl
    elab_simulate $f
done

clean

echo "Test successful"
