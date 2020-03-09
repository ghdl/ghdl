#! /bin/sh

. ../../testenv.sh

for t in simple01; do
    synth_tb $t
done

echo "Test successful"
