#! /bin/sh

. ../../testenv.sh

synth --out=raw test.vhdl -e | grep -q mem_rd_sync

echo "Test successful"
