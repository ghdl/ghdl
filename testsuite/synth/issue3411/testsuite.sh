#! /bin/sh

. ../../testenv.sh

synth --out=raw --latches latch1.vhdl -e > syn_latch1.raw

fgrep dlatch syn_latch1.raw
fgrep dff syn_latch1.raw

clean

echo "Test successful"
