#! /bin/sh

. ../../testenv.sh

for t in dff01 dff02 dff03 dff04 dff05 dff06 dff07 dff08 dff09 \
               dff10 dff11 dff12 dff13 dff14 dff15; do
    synth_tb $t
done

echo "Test successful"
