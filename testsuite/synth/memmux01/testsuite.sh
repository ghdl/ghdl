#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in memmux01 memmux02 memmux03 memmux04 memmux05 memmux07; do
    synth_tb $t
done

echo "Test successful"
