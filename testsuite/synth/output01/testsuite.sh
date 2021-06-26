#! /bin/sh

. ../../testenv.sh

for t in output01 output06 output07; do
    synth_tb $t
done

echo "Test successful"
