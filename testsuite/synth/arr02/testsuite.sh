#! /bin/sh

. ../../testenv.sh

for t in arr01 arr02; do
    synth_tb $t
done

echo "Test successful"
