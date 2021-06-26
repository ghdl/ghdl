#! /bin/sh

. ../../testenv.sh

for t in dff05 dff06 dff08 dff08a dff08b dff08c dff08d dff09; do
    synth_tb $t
done

echo "Test successful"
