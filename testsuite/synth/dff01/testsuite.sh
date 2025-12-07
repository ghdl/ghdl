#! /bin/sh

. ../../testenv.sh

for t in dff01 dff01b dff02 dff03 dff04 dff05 dff06 dff07 dff08 dff09 \
               dff10 dff11 dff12 dff13 dff14 dff15 dff22; do
    synth_tb $t
done

synth_failure dff18.vhdl -e
synth_failure dff19.vhdl -e
synth_failure dff20.vhdl -e
synth_failure dff21.vhdl -e

echo "Test successful"
