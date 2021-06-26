#! /bin/sh

. ../../testenv.sh

for t in record_test rec2; do
    synth_tb $t
done

echo "Test successful"
