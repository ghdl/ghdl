#! /bin/sh

. ../../testenv.sh

for t in cnt01 cnt02 cnt04; do
    synth_tb $t
done

echo "Test successful"
