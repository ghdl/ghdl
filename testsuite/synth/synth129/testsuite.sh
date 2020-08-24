#! /bin/sh

. ../../testenv.sh

for t in test; do
    synth_tb $t
done

echo "Test successful"
