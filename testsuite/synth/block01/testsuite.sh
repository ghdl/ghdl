#! /bin/sh

. ../../testenv.sh

for t in block01 block02; do
    synth_tb $t
done

echo "Test successful"
