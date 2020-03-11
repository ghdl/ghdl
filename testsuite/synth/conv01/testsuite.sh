#! /bin/sh

. ../../testenv.sh

for t in conv01; do
    synth_analyze $t
    clean
done

echo "Test successful"
