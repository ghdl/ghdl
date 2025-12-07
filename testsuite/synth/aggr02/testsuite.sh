#! /bin/sh

. ../../testenv.sh

for t in targ01 targ02 targ03; do
    synth_tb $t
done

synth_only targ04

echo "Test successful"
