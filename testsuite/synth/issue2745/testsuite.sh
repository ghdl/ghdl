#! /bin/sh

. ../../testenv.sh

for t in init initport; do
    synth_tb $t
done

echo "Test successful"
