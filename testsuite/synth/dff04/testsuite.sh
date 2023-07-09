#! /bin/sh

. ../../testenv.sh

for f in dff04 dff01 dff05; do
    verilog_synth_tb $f
done

echo "Test successful"
