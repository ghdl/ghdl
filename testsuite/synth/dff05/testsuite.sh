#! /bin/sh

. ../../testenv.sh

for t in dff01 dff02; do
    synth_tb $t
done

echo "Test successful"
