#! /bin/sh

. ../../testenv.sh

for t in const01; do
    synth_tb $t
done

# synth const02.vhdl -e > syn_const02.vhdl
synth_analyze const03
clean

echo "Test successful"
