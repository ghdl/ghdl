#! /bin/sh

. ../../testenv.sh

for t in enot bnot; do
    synth_tb $t
done

echo "Test successful"
