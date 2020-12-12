#! /bin/sh

. ../../testenv.sh

for t in slice01 slice02 slice03; do
    synth_tb $t
done

for t in slice04 slice05 slice06 slice07; do
    synth_analyze $t
done

echo "Test successful"
