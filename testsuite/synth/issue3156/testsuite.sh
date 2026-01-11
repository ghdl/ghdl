#! /bin/sh

. ../../testenv.sh

for fmt in vhdl verilog raw; do
    synth -Wno-binding --out=$fmt blinky.vhdl -e > syn_blinky.$fmt
done

fgrep -q 'CLKIN1_PERIOD=1.0e1,' syn_blinky.raw
fgrep -q 'DIVCLK_DIVIDE=1,' syn_blinky.raw

fgrep -q '.CLKIN1_PERIOD(1.0e1),' syn_blinky.verilog
fgrep -q '.DIVCLK_DIVIDE(1),' syn_blinky.verilog

fgrep -q 'CLKIN1_PERIOD => 1.0e1,' syn_blinky.vhdl
fgrep -q 'DIVCLK_DIVIDE => 1,' syn_blinky.vhdl

echo "Test successful"
