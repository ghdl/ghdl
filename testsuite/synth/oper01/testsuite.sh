#! /bin/sh

. ../../testenv.sh

for t in snum01 snum02 snum03 snum04 snum05 cmp01 cmp02 match01 uns01 \
	 nand01 nor01 xnor01; do
    synth_tb $t
done

synth_only cmp03
synth_only snum06

echo "Test successful"
