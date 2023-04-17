#! /bin/sh

. ../../testenv.sh

synth --out=raw test.vhdl -e > syn_test.raw
grep -q mem_rd_sync syn_test.raw

synth_tb test2
synth_tb test2n

echo "Test successful"
