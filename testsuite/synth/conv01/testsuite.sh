#! /bin/sh

. ../../testenv.sh

for t in conv01 pos01; do
    synth_analyze $t
    clean
done

synth_only convacc01

synth_failure convarr01.vhdl -e
synth_failure convflt01.vhdl -e

GHDL_STD_FLAGS=--std=08
synth_failure convrec01.vhdl -e

echo "Test successful"
