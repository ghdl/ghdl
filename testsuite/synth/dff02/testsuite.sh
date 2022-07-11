#! /bin/sh

. ../../testenv.sh

# dff06 creates a latch (that is unused)
for t in dff05 dff08 dff08a dff08b dff08c dff08d dff09; do
    synth_tb $t
done

echo "Test successful"
