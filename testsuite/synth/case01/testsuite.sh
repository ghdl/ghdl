#! /bin/sh

. ../../testenv.sh

for t in case01 case02 case03 case04; do
    synth_tb $t
done

synth case05.vhdl -e case05 > syn_case05.vhdl

echo "Test successful"
