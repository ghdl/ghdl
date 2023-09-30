#! /bin/sh

. ../../testenv.sh

for t in shift1; do
    synth_tb $t
done

echo "Test successful"
