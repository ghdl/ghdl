#! /bin/sh

. ../../testenv.sh

for keep in yes no; do
    GHDL_SYNTH_FLAGS=--keep-hierarchy=$keep
    for unit in comp04 comp05 comp06; do
	synth_tb $unit
    done
done

echo "Test successful"
