#! /bin/sh

. ../../testenv.sh

for t in snum01 snum02 snum03 snum04 snum05 cmp01 cmp02 \
	 match01 match02 match05 uns01 \
	 nand01 nor01 xnor01; do
    synth_tb $t
done

synth_only cmp03
synth_only snum06

synth_failure match03.vhdl -e
synth_failure match04.vhdl -e

echo "Test successful"
