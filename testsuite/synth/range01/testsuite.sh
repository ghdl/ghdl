#! /bin/sh

. ../../testenv.sh

for t in revrng01; do
    synth_tb $t
done

echo "Test successful"
