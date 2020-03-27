#! /bin/sh

. ../../testenv.sh

for t in mdim01; do
    synth_tb $t
done

echo "Test successful"
