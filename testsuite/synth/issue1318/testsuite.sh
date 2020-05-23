#! /bin/sh

. ../../testenv.sh

synth_analyze ram_blk

grep ram_style syn_ram_blk.vhdl

clean

echo "Test successful"
