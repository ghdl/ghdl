#! /bin/sh

. ../../testenv.sh

for t in rom1 srom01 sram01 sram02 sram03 sram05 dpram1 dpram2 dpram3; do
    synth_tb $t 2> $t.log

    # Each design has either a RAM or a ROM
    grep 'found R' $t.log
done

echo "Test successful"
