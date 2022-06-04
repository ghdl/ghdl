#! /bin/sh

. ../../testenv.sh

for t in swaptest; do
    synth_tb $t
done

echo "Test successful"
