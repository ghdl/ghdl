#! /bin/sh

. ../../testenv.sh

synth definitions.vhdl cm_target.vhdl -e > syn_cm_target.vhdl

for t in dff1 dff2 dff3; do
    synth_tb $t
done

echo "Test successful"
