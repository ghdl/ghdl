#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for t in var01c var01b var01a var01 var02 var03 var04 var05 var06; do
    synth_tb $t
done

echo "Test successful"
