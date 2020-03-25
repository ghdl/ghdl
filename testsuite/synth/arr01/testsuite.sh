#! /bin/sh

. ../../testenv.sh

for t in arr01 arr02 arr04 arr05 arr06 arr07 arr09; do
    synth_tb $t
done

synth_analyze arr10
synth_analyze arr11

echo "Test successful"
