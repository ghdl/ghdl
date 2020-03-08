#! /bin/sh

. ../../testenv.sh

for t in func01; do
    synth_tb $t
done

echo "Test successful"
