#! /bin/sh

. ../../testenv.sh

for t in bug bug4 bug2; do
    synth_tb $t
done

echo "Test successful"
