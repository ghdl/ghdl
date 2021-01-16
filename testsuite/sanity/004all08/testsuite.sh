#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08"
analyze all08.vhdl

# Test reprint
$GHDL --reprint --std=08 all08.vhdl > all08.out
$GHDL --compare-tokens --std=08 all08.out all08.vhdl

# Test reprint --no-sem
$GHDL --reprint --std=08 --no-sem all08.vhdl > all08.out
$GHDL --compare-tokens --std=08 all08.vhdl all08.out

rm -f all08.out

# Test reprint ams (no-sem)
$GHDL --reprint --ams --no-sem ams08.vhdl > ams08.out
$GHDL --compare-tokens --ams ams08.out ams08.vhdl

rm -f ams08.out

clean

echo "test successful"
