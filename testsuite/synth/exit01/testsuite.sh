#! /bin/sh

. ../../testenv.sh

for t in exit01 exit02; do
    synth_tb $t
done

echo "Test successful"
