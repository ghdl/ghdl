#! /bin/sh

. ../../testenv.sh

for t in dff01 dff02 dff03 dff04 dff05 dff06 dff07; do
    synth_tb $t
done

echo "Test successful"
