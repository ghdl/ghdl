#! /bin/sh

. ../../testenv.sh

for t in snum01 snum02 snum03 snum04 snum05 cmp01 cmp02 match01 uns01; do
    synth_tb $t
done

synth_only snum06

echo "Test successful"
